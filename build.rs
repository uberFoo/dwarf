use std::{env, fs, path::Path};

fn main() {
    let mut tests = String::new();
    let mut in_dir = std::env::current_dir().unwrap();
    in_dir.push("tests");
    for file in fs::read_dir(&in_dir).unwrap() {
        let file = file.unwrap();
        let path = file.path();
        if path.is_file() {
            let ext = path.extension().unwrap();
            if ext != "tao" {
                continue;
            }
            let name = path.file_stem().unwrap().to_str().unwrap();
            let contents = fs::read_to_string(&path).unwrap();
            tests += "#[test]\n";
            tests += &format!("fn {}() {{\n", name);
            tests += "    let _ = env_logger::builder().is_test(true).try_init();\n";
            tests += &format!("    assert!(run_program(\"{contents}\"));\n");
            tests += "\n";
            tests += "}\n";
        }
    }
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("tests.rs");
    fs::write(&dest_path, tests).unwrap();
    println!("cargo:rerun-if-changed=tests");
}
