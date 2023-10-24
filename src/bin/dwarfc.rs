use std::{env, ffi::OsString, fs, os::unix::ffi::OsStringExt, path::PathBuf, process};

use clap::{ArgAction, Parser};
use snafu::prelude::*;
use tracy_client::Client;

use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};

use dwarf::{
    dwarf::{
        error::{FileSnafu, IOSnafu, Result},
        new_lu_dog, parse_dwarf,
    },
    s_read,
};

const TARGET_DIR: &str = "target";
const BUILD_DIR: &str = "sarzak";
const EXTENSIONS: [&str; 2] = ["tao", "道"];

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Args {
    /// Dwarf Source File
    ///
    /// Path to the source file to compile.
    source: PathBuf,
    /// Debug Output
    ///
    /// Debug output flag. If set, `sarzak` will output the store in "long"
    /// format, where each object is a directory and each instance is a JSOn
    /// file.
    #[arg(long, short, action=ArgAction::SetTrue)]
    #[cfg(not(feature = "multi-nd-vec"))]
    debug: Option<bool>,
    /// Meta-Model File
    ///
    /// Path to the meta-model, sarzak.
    // #[arg(long, short)]
    // sarzak: PathBuf,
    /// Path to package
    ///
    /// If included, `sarzak` will create a new domain in the specified
    /// location. It must exist, and must be part of a Rust package.
    #[arg(long, short)]
    package_dir: Option<PathBuf>,
    /// Do uber stuff
    ///
    /// This is like sudo mode. You probably don't want this.
    #[arg(long, action=ArgAction::SetTrue)]
    uber: Option<bool>,
}

fn find_package_dir(start_dir: &Option<PathBuf>) -> Result<PathBuf> {
    if let Some(dir) = start_dir {
        std::env::set_current_dir(dir)
            .context(IOSnafu {
                description: "Failed to set current dir".to_owned(),
            })
            .map_err(|e| vec![e])?;
    }

    // Figure out where Cargo.toml is located.
    //
    let output = process::Command::new("cargo")
        .arg("locate-project")
        .arg("--message-format")
        .arg("plain")
        .output()
        .context(IOSnafu {
            description: "Failed to run cargo locate-project".to_owned(),
        })
        .map_err(|e| vec![e])?;

    if !output.status.success() {
        return Err(vec![dwarf::dwarf::error::DwarfError::Generic {
            description: "cargo locate-project failed".to_owned(),
        }]);
    }

    let mut stdout = output.stdout;

    // I don't know if it's kosher, but this does nicely to get rid of
    // that newline character.
    stdout.pop();
    let os_string = OsString::from_vec(stdout);
    let mut package_root = PathBuf::from(os_string);
    // Get rid of Cargo.toml
    package_root.pop();

    log::debug!("Found root 🦀 at {:?}!", package_root);

    Ok(package_root)
}

fn main() -> Result<()> {
    pretty_env_logger::init();
    color_backtrace::install();
    Client::start();

    let args = Args::parse();

    let is_uber = args.uber.is_some() && args.uber.unwrap();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let path = if args.package_dir.is_some() {
        let package_dir = find_package_dir(&args.package_dir)?;

        // This is where we output the file. I think this is stupid. We should just write
        // it to current working directory. We do however want the package_dir, because
        // we need to feed it to the lu dog populator.
        let mut path = package_dir;
        path.push(TARGET_DIR);
        path.push(BUILD_DIR);
        path
    } else {
        // Write the output at the same location as the source file.
        fs::canonicalize(&args.source)
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    };

    let mut out_file = path.clone();
    // Build the output path
    out_file.push("toadstool"); // This gets replaced below -- I'm just being silly.
    out_file.set_file_name(args.source.file_name().unwrap());
    out_file.set_extension(EXTENSIONS[1]);

    let source_code = fs::read_to_string(&args.source)
        .context(FileSnafu {
            description: "Could not read source file".to_owned(),
            path: &args.source,
        })
        .map_err(|e| vec![e])?;

    let ast = parse_dwarf(args.source.to_str().unwrap(), &source_code).map_err(|e| vec![e])?;

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let ctx = match new_lu_dog(
        args.source.to_str().unwrap().to_owned(),
        Some((source_code.clone(), &ast)),
        &dwarf_home,
        &sarzak,
    ) {
        Ok(lu_dog) => lu_dog,
        Err(errors) => {
            for err in errors {
                eprintln!(
                    "{}",
                    dwarf::dwarf::error::DwarfErrorReporter(&err, is_uber, &source_code,)
                );
            }
            return Ok(());
        }
    };

    #[cfg(not(feature = "multi-nd-vec"))]
    if args.debug.is_some() && args.debug.unwrap() {
        s_read!(ctx.lu_dog)
            .persist(&out_file)
            .context(FileSnafu {
                description: "Could not persist Lu-Dog domain".to_owned(),
                path: &out_file,
            })
            .map_err(|e| vec![e])?;
    } else {
        s_read!(ctx.lu_dog)
            .persist_bincode(&out_file)
            .context(FileSnafu {
                description: "Could not persist Lu-Dog domain".to_owned(),
                path: &out_file,
            })
            .map_err(|e| vec![e])?;
    }

    println!("Lu-Dog domain created at {:?}", out_file);

    Ok(())
}
