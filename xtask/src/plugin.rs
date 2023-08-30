use std::{env, fs};

use glob::glob;
use xshell::{cmd, Shell};

use crate::flags;

const EXT_DIR: &str = "extensions";
const LIB_DIR: &str = "lib";
const PLUGIN_DIR: &str = "plug-ins";
const MODEL_DIR: &str = "models";
const SRC_DIR: &str = "src";
const TAO_DIR: &str = "tao";

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

        for entry in fs::read_dir(&current_dir)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                sh.change_dir(entry.path());
                println!("Building {}", entry.path().display());
                cmd!(sh, "sarzak gen").run()?;
                cmd!(sh, "sarzak gen grace dwarf").run()?;
                cmd!(sh, "cargo build").run()?;

                let name = entry.file_name();
                let name = name.to_str().unwrap();
                let lib_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{LIB_DIR}");
                let src_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{SRC_DIR}");
                let model_dir = format!("{dwarf_home}/{EXT_DIR}/{name}/{MODEL_DIR}");
                fs::create_dir_all(&lib_dir)?;
                fs::create_dir_all(&src_dir)?;
                fs::create_dir_all(&model_dir)?;

                println!("Copying lib{}.dylib", name);
                sh.copy_file(format!("target/debug/lib{}.dylib", name), lib_dir)?;

                current_dir.push(entry.path());
                current_dir.push(TAO_DIR);

                for entry in fs::read_dir(&current_dir)? {
                    let path = entry?.path();
                    println!("Copying {}", path.display());
                    sh.copy_file(path, &src_dir)?;
                }

                current_dir.pop();
                current_dir.push(MODEL_DIR);
                sh.change_dir(&current_dir);

                for entry in glob("*.json")? {
                    let path = entry?;
                    println!("Copying {}", path.display());
                    sh.copy_file(path, &model_dir)?;
                }
            }
        }

        Ok(())
    }
}
