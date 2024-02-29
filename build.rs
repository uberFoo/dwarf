use std::{env, fs, io::Write, path::Path};

use walkdir::WalkDir;

const EXT1: &str = "tao";
const EXT2: &str = "ore";
const TEST_DIR: &str = "tests";
const INTERP_HARNESS_DIR: &str = "harness";
const VM_HARNESS_DIR: &str = "vm_harness";
const INTERP_OUT_NAME: &str = "tests.rs";
const VM_OUT_NAME: &str = "vm_tests.rs";

fn main() {
    // Create a timestamp file
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("timestamp.txt");

    let mut f = fs::File::create(dest_path).unwrap();
    // write!(f, r#""{}""#, time::OffsetDateTime::now_utc()).unwrap();
    write!(f, r#""{}""#, chrono::Utc::now().to_rfc3339()).unwrap();

    // Generate the tests
    let mut tests = String::new();
    let mut in_dir = std::env::current_dir().unwrap();
    in_dir.push(TEST_DIR);
    in_dir.push(INTERP_HARNESS_DIR);
    for entry in WalkDir::new(&in_dir).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        let root = path.parent().unwrap();
        if root.file_name().unwrap() == "failing" {
            continue;
        }
        if path.is_file() {
            let ext = path.extension().unwrap();
            if ext != EXT1 && ext != EXT2 {
                continue;
            }
            let parent = root.file_name().unwrap().to_str().unwrap();
            let name = path.file_stem().unwrap().to_str().unwrap();
            let contents = fs::read_to_string(path).unwrap();
            tests += "#[test]\n";
            tests += &format!("fn {parent}_{name}() {{\n");
            tests += "    let _ = env_logger::builder().is_test(true).try_init();\n";
            tests += "    color_backtrace::install();\n";
            tests += &format!("    let result = run_program(\"{name}\", r#\"{contents}\"#);\n");

            let stderr = root.join(format!("{}.stderr", name));
            if stderr.exists() {
                tests += &format!(
                    "    let _ = result.map_err(|e| {{assert!(diff_with_file(\"{stderr}\", \"{name}\", &e).is_ok()); Err::<(), String>(e)}});\n",
                    stderr = stderr.display()
                );
            } else {
                tests += "    assert!(result.is_ok());\n";
            }

            let stdout = root.join(format!("{}.stdout", name));
            if stdout.exists() {
                tests += &format!(
                    "    let _ = result.map(|ok| {{assert!(diff_with_file(\"{stdout}\", \"{name}\", &ok.1).is_ok()); ok}});\n",
                    stdout = stdout.display()
                );
            }

            tests += "}\n\n";
        }
    }

    let dest_path = Path::new(&out_dir).join(INTERP_OUT_NAME);
    fs::write(dest_path, tests).unwrap();

    let mut tests = String::new();
    let mut in_dir = std::env::current_dir().unwrap();
    in_dir.push(TEST_DIR);
    in_dir.push(VM_HARNESS_DIR);
    for entry in WalkDir::new(&in_dir).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        let root = path.parent().unwrap();
        if root.file_name().unwrap() == "failing" {
            continue;
        }
        if path.is_file() {
            let ext = path.extension().unwrap();
            if ext != EXT1 && ext != EXT2 {
                continue;
            }
            let parent = root.file_name().unwrap().to_str().unwrap();
            let name = path.file_stem().unwrap().to_str().unwrap();
            let contents = fs::read_to_string(path).unwrap();
            tests += "#[test]\n";
            tests += &format!("fn {parent}_{name}() {{\n");
            tests += "    let _ = env_logger::builder().is_test(true).try_init();\n";
            tests += "    color_backtrace::install();\n";
            tests += &format!("    let result = run_program(\"{name}\", r#\"{contents}\"#);\n");

            let stderr = root.join(format!("{}.stderr", name));
            if stderr.exists() {
                tests += &format!(
                    "    let _ = result.map_err(|e| {{assert!(diff_with_file(\"{stderr}\", \"{name}\", &e).is_ok()); Err::<(), String>(e)}});\n",
                    stderr = stderr.display()
                );
            } else {
                tests += "    assert!(result.is_ok());\n";
            }

            let stdout = root.join(format!("{}.stdout", name));
            if stdout.exists() {
                tests += &format!(
                    "    let _ = result.map(|ok| {{assert!(diff_with_file(\"{stdout}\", \"{name}\", &ok.1).is_ok()); ok}});\n",
                    stdout = stdout.display()
                );
            }

            tests += "}\n\n";
        }
    }

    let dest_path = Path::new(&out_dir).join(VM_OUT_NAME);
    fs::write(dest_path, tests).unwrap();

    println!("cargo:rerun-if-changed={TEST_DIR}/{INTERP_HARNESS_DIR}");
    println!("cargo:rerun-if-changed={TEST_DIR}/{VM_HARNESS_DIR}");
}
