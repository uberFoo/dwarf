use std::{ffi::OsString, fs, os::unix::ffi::OsStringExt, path::PathBuf, process};

use clap::{ArgAction, Parser};

use snafu::prelude::*;

use sarzak::{
    domain::DomainBuilder,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};

use dwarf::dwarf::{new_lu_dog, parse_dwarf, FileSnafu, GenericSnafu, IOSnafu, Result};

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
    debug: Option<bool>,
    /// Model File
    ///
    /// Path to the model, corresponding to the source file, to build the
    /// Lu-Dog domain.
    #[arg(long, short)]
    model: Option<PathBuf>,
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
}

fn find_package_dir(start_dir: &Option<PathBuf>) -> Result<PathBuf> {
    if let Some(dir) = start_dir {
        std::env::set_current_dir(dir).context(IOSnafu {
            description: "Failed to set current dir".to_owned(),
        })?;
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
        })?;

    ensure!(
        output.status.success(),
        GenericSnafu {
            description: "cargo locate-project failed".to_owned()
        }
    );

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

    let args = Args::parse();

    let model = if let Some(ref model) = args.model {
        vec![DomainBuilder::new()
            .cuckoo_model(model)
            .unwrap()
            .build_v2()
            .unwrap()
            .sarzak()
            .clone()]
    } else {
        vec![]
    };

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
        // Write the output at the same loccation as the source file.
        fs::canonicalize(&args.source)
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf()
    };

    let mut out_file = path.clone();
    // Build the output path
    out_file.push("toadstool");
    out_file.set_file_name(args.source.file_name().unwrap());
    out_file.set_extension(EXTENSIONS[1]);

    // fs::create_dir_all(&path).context(FileSnafu {
    //     description: "creating type directory".to_owned(),
    //     path: &path,
    // })?;

    let source_code = fs::read_to_string(&args.source).context(FileSnafu {
        description: "Could not read source file".to_owned(),
        path: &args.source,
    })?;

    let ast = parse_dwarf(&source_code)?;

    let lu_dog = new_lu_dog(
        Some(&path),
        Some((source_code.clone(), &ast)),
        &model,
        &sarzak,
    )
    .map_err(|e| {
        eprintln!("{}", dwarf::dwarf::DwarfErrorReporter(&e, &source_code));
        e
    })?;

    if args.debug.is_some() && args.debug.unwrap() {
        lu_dog.persist(&out_file).context(FileSnafu {
            description: "Could not persist Lu-Dog domain".to_owned(),
            path: &out_file,
        })?;
    } else {
        lu_dog.persist_bincode(&out_file).context(FileSnafu {
            description: "Could not persist Lu-Dog domain".to_owned(),
            path: &out_file,
        })?;
    }

    println!("Lu-Dog domain created at {:?}", out_file);

    Ok(())
}
