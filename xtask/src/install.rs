use std::{env, fs};

use crate::flags;

use xshell::{cmd, Shell};

impl flags::Install {
    pub(crate) fn run(self, sh: &Shell, dwarf_home: &String) -> anyhow::Result<()> {
        if self.debug.unwrap_or(false) {
            cmd!(sh, "cargo install --path . --force --locked --debug").run()?;
        } else {
            cmd!(sh, "cargo install --path . --force --locked").run()?;
        }
        let debug = if self.debug.unwrap_or(false) {
            "true"
        } else {
            "false"
        };
        cmd!(sh, "cargo xtask plugin --name http --debug {debug}").run()?;
        cmd!(sh, "cargo xtask plugin --name std --debug {debug}").run()?;
        cmd!(sh, "cargo xtask plugin --name md --debug {debug}").run()?;
        cmd!(sh, "cargo xtask plugin --name sqlx --debug {debug}").run()?;

        // let std_dst = format!("{}/lib/std", dwarf_home);
        // fs::create_dir_all(&std_dst)?;
        // let mut std_lib_dir = std::env::current_dir()?;
        // std_lib_dir.push("std");
        // std_lib_dir.push("ore");

        // for entry in fs::read_dir(&std_lib_dir)? {
        //     let file = entry?.path();
        //     println!("Copying {}", file.display());
        //     sh.copy_file(file, &std_dst)?;
        // }

        let compiled = format!("{}/compiled", dwarf_home);
        fs::create_dir_all(&compiled)?;

        let extruded = format!("{}/extruded", dwarf_home);
        fs::create_dir_all(&extruded)?;

        Ok(())
    }
}
