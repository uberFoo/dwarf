use std::{
    env,
    fs::{self, DirEntry},
    path::PathBuf,
};

use glob::glob;
use xshell::{cmd, Shell};

use crate::flags;

const EXT_DIR: &str = "extensions";
const LIB_DIR: &str = "lib";
const PLUGIN_DIR: &str = "plug-ins";
const MODEL_DIR: &str = "models";
const SRC_DIR: &str = "src";
const TAO_DIR: &str = "ore";

impl flags::Plugins {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let dwarf_home = env::var("DWARF_HOME").unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        });

        let mut current_dir = std::env::current_dir()?;
        current_dir.push(PLUGIN_DIR);
        sh.change_dir(PLUGIN_DIR);

        fs::read_dir(&current_dir)?
            .filter(|e| {
                if let Ok(e) = e {
                    match &self.plugin {
                        Some(plugin) => e.file_name().into_string().unwrap() == *plugin,
                        None => true,
                    }
                } else {
                    false
                }
            })
            .for_each(|entry| {
                // for entry in fs::read_dir(&current_dir)? {
                let entry = entry.unwrap();
                if entry.file_type().unwrap().is_dir() {
                    sh.change_dir(entry.path());
                    build_plugin(&entry, &sh, &dwarf_home).unwrap()
                }
            });

        Ok(())
    }
}

fn build_plugin(entry: &DirEntry, sh: &Shell, dwarf_home: &String) -> anyhow::Result<()> {
    println!("Building {}", entry.path().display());
    sh.change_dir(entry.path());

    let mut current_dir = entry.path();

    let mut sarzak_toml = current_dir.clone();
    sarzak_toml.push("sarzak.toml");

    let have_models = sarzak_toml.exists();
    if have_models {
        println!("Generating models");
        cmd!(sh, "sarzak gen").run()?;
    } else {
        println!("No models found");
    }

    cmd!(sh, "cargo update").run()?;
    cmd!(sh, "cargo build").run()?;

    let name = entry.file_name();
    let name = name.to_str().unwrap();
    let lib_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{LIB_DIR}");
    let src_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{SRC_DIR}");
    let model_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{MODEL_DIR}");
    fs::create_dir_all(&lib_dir)?;
    fs::create_dir_all(&src_dir)?;
    fs::create_dir_all(&model_dir)?;

    if env::consts::OS == "macos" {
        println!("Copying lib{}.dylib", name);
        sh.copy_file(format!("target/debug/lib{}.dylib", name), lib_dir)?;
    } else if env::consts::OS == "linux" {
        println!("Copying lib{}.so", name);
        sh.copy_file(format!("target/debug/lib{}.so", name), lib_dir)?;
    } else if env::consts::OS == "windows" {
        println!("Copying {}.dll", name);
        sh.copy_file(format!("target/debug/{}.dll", name), lib_dir)?;
    } else {
        panic!("{} is not a supported platform", env::consts::OS);
    }

    // Copy dwarf files
    current_dir.push(entry.path());
    current_dir.push(TAO_DIR);

    for entry in fs::read_dir(&current_dir)? {
        let path = entry?.path();
        println!("Copying {}", path.display());
        sh.copy_file(path, &src_dir)?;
    }

    // Copy model files
    if have_models {
        current_dir.pop();
        current_dir.push(MODEL_DIR);
        sh.change_dir(&current_dir);

        env::set_current_dir(&current_dir)?;
        for entry in glob("*.json")? {
            let path = entry?;
            let metadata = fs::metadata(&path)?;
            if metadata.is_dir() {
                continue;
            }
            println!("Copying {}", path.display());
            sh.copy_file(path, &model_dir)?;
        }
    }

    Ok(())
}
