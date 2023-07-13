use std::{env, fs, path::Path};

use walkdir::WalkDir;

fn main() {
    let mut tests = String::new();
    let mut in_dir = std::env::current_dir().unwrap();
    in_dir.push("tests");
    for entry in WalkDir::new(&in_dir).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        let root = path.parent().unwrap();
        if root.file_name().unwrap() == "failing" {
            continue;
        }
        if path.is_file() {
            let ext = path.extension().unwrap();
            if ext != "tao" {
                continue;
            }

            let name = path.file_stem().unwrap().to_str().unwrap();
            let contents = fs::read_to_string(&path).unwrap();
            tests += "#[test]\n";
            tests += &format!("fn {name}() {{\n");
            tests += "    let _ = env_logger::builder().is_test(true).try_init();\n";
            tests += "    color_backtrace::install();\n";
            tests += &format!("    let result = run_program(\"{name}\", r#\"{contents}\"#);\n");

            let stderr = root.join(format!("{}.stderr", name));
            if stderr.exists() {
                tests += &format!(
                    "    let _ = result.map_err(|e| {{assert!(diff_std_err(\"{stderr}\", \"{name}\", &e).is_ok()); Err::<(), String>(e)}});\n",
                    stderr = stderr.display()
                );
            } else {
                tests += "    assert!(result.is_ok());\n";
            }

            let stdout = root.join(format!("{}.stdout", name));
            if stdout.exists() {
                tests += &format!(
                    "    let _ = result.and_then(|ok| {{assert!(diff_std_out(\"{stdout}\", \"{name}\", &ok.1).is_ok()); Ok(ok)}});\n",
                    stdout = stdout.display()
                );
            }

            tests += "}\n\n";
        }
    }
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("tests.rs");
    fs::write(dest_path, tests).unwrap();
    println!("cargo:rerun-if-changed=tests");
}
