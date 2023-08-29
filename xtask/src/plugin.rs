use std::{env, fs};

use xshell::{cmd, Shell};

use crate::flags;

const PLUGIN_DIR: &str = "plug-ins";
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
                let lib_dir = format!("{}/extensions/{}/lib", dwarf_home, name);
                let src_dir = format!("{}/extensions/{}/src", dwarf_home, name);
                fs::create_dir_all(&lib_dir)?;
                fs::create_dir_all(&src_dir)?;

                println!("Copying lib{}.dylib", name);
                sh.copy_file(format!("target/debug/lib{}.dylib", name), lib_dir)?;

                current_dir.push(entry.path());
                current_dir.push(TAO_DIR);

                dbg!(&current_dir);
                for entry in fs::read_dir(&current_dir)? {
                    let path = entry?.path();
                    println!("Copying {}", path.display());
                    sh.copy_file(path, &src_dir)?;
                }
            }
        }

        Ok(())
    }
}
