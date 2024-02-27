use std::{env, fs};

use crate::flags;

use xshell::{cmd, Shell};

impl flags::Install {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let dwarf_home = env::var("DWARF_HOME").unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        });

        cmd!(sh, "cargo install --path . --force --locked").run()?;
        cmd!(sh, "cargo xtask plugin --plugin http").run()?;

        let std_dst = format!("{}/lib/std", dwarf_home);
        fs::create_dir_all(&std_dst)?;
        let mut std_lib_dir = std::env::current_dir()?;
        std_lib_dir.push("std");
        std_lib_dir.push("src");
        for entry in fs::read_dir(&std_lib_dir)? {
            let file = entry?.path();
            println!("Copyning {}", file.display());
            sh.copy_file(file, &std_dst)?;
        }

        let compiled = format!("{}/compiled", dwarf_home);
        fs::create_dir_all(&compiled)?;

        Ok(())
    }
}
