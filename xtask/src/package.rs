use std::{
    env,
    fs::File,
    io::{self, BufWriter},
    path::{Path, PathBuf},
};

use flate2::{write::GzEncoder, Compression};
use time::OffsetDateTime;
use xshell::{cmd, Shell};
use zip::{write::FileOptions, DateTime, ZipWriter};

use crate::{date_iso, flags, project_root};

const VERSION_STABLE: &str = "0.1";
const VERSION_NIGHTLY: &str = "0.1";

impl flags::Package {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let stable = sh.var("GITHUB_REF").unwrap_or_default().as_str() == "refs/heads/master";

        let project_root = project_root();
        let target = Target::get(&project_root);
        let dist = project_root.join("dist");
        sh.remove_path(&dist)?;
        sh.create_dir(&dist)?;

        if let Some(patch_version) = self.client_patch_version {
            let version = if stable {
                format!("{VERSION_STABLE}.{patch_version}")
            } else {
                // A hack to make VS Code prefer nightly over stable.
                format!("{VERSION_NIGHTLY}.{patch_version}")
            };
            dist_server(sh, &format!("{version}-standalone"), &target)?;
        } else {
            dist_server(sh, "0.0.0-standalone", &target)?;
        }
        Ok(())
    }
}

fn dist_server(sh: &Shell, release: &str, target: &Target) -> anyhow::Result<()> {
    let _e = sh.push_env("CARGO_PROFILE_RELEASE_LTO", "fat");

    // 🚧 Not doing anything with the release str atm. I can see using it like RA
    // do in the future.

    // Uncomment to enable debug info for releases. Note that:
    //   * debug info is split on windows and macs, so it does nothing for those platforms,
    //   * on Linux, this blows up the binary size from 8MB to 43MB, which is unreasonable.
    // let _e = sh.push_env("CARGO_PROFILE_RELEASE_DEBUG", "1");

    if target.name.contains("-linux-") {
        env::set_var("CC", "clang");
    }

    let target_name = &target.name;
    cmd!(
        sh,
        "cargo build --bin dwarf --target {target_name} --release"
    )
    .run()?;

    let dst = Path::new("dist").join(&target.artifact_name);
    gzip(&target.server_path, &dst.with_extension("gz"))?;
    if target_name.contains("-windows-") {
        zip(
            &target.server_path,
            target.symbols_path.as_ref(),
            &dst.with_extension("zip"),
        )?;
    }

    Ok(())
}

fn gzip(src_path: &Path, dest_path: &Path) -> anyhow::Result<()> {
    let mut encoder = GzEncoder::new(File::create(dest_path)?, Compression::best());
    let mut input = io::BufReader::new(File::open(src_path)?);
    io::copy(&mut input, &mut encoder)?;
    encoder.finish()?;
    Ok(())
}

fn zip(src_path: &Path, symbols_path: Option<&PathBuf>, dest_path: &Path) -> anyhow::Result<()> {
    let file = File::create(dest_path)?;
    let mut writer = ZipWriter::new(BufWriter::new(file));
    writer.start_file(
        src_path.file_name().unwrap().to_str().unwrap(),
        FileOptions::default()
            .last_modified_time(
                DateTime::try_from(OffsetDateTime::from(
                    std::fs::metadata(src_path)?.modified()?,
                ))
                .unwrap(),
            )
            .unix_permissions(0o755)
            .compression_method(zip::CompressionMethod::Deflated)
            .compression_level(Some(9)),
    )?;
    let mut input = io::BufReader::new(File::open(src_path)?);
    io::copy(&mut input, &mut writer)?;
    if let Some(symbols_path) = symbols_path {
        writer.start_file(
            symbols_path.file_name().unwrap().to_str().unwrap(),
            FileOptions::default()
                .last_modified_time(
                    DateTime::try_from(OffsetDateTime::from(
                        std::fs::metadata(src_path)?.modified()?,
                    ))
                    .unwrap(),
                )
                .compression_method(zip::CompressionMethod::Deflated)
                .compression_level(Some(9)),
        )?;
        let mut input = io::BufReader::new(File::open(symbols_path)?);
        io::copy(&mut input, &mut writer)?;
    }
    writer.finish()?;
    Ok(())
}

struct Target {
    name: String,
    server_path: PathBuf,
    symbols_path: Option<PathBuf>,
    artifact_name: String,
}

impl Target {
    fn get(project_root: &Path) -> Self {
        let name = match env::var("RA_TARGET") {
            Ok(target) => target,
            _ => {
                if cfg!(target_os = "linux") {
                    "x86_64-unknown-linux-gnu".to_string()
                } else if cfg!(target_os = "windows") {
                    "x86_64-pc-windows-msvc".to_string()
                } else if cfg!(target_os = "macos") {
                    "x86_64-apple-darwin".to_string()
                } else {
                    panic!("Unsupported OS")
                }
            }
        };
        let out_path = project_root.join("target").join(&name).join("release");
        let (exe_suffix, symbols_path) = if name.contains("-windows-") {
            (".exe".into(), Some(out_path.join("dwarf.pdb")))
        } else {
            (String::new(), None)
        };
        let server_path = out_path.join(format!("dwarf{exe_suffix}"));
        let artifact_name = format!("dwarf-{name}{exe_suffix}");
        Self {
            name,
            server_path,
            symbols_path,
            artifact_name,
        }
    }
}
