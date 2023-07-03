use std::path::PathBuf;

use ansi_term::Colour;
use env_logger;

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Value,
};

fn diff_errors(test: &str, errors: &String) -> Result<Value, ()> {
    let stderr_path = PathBuf::from(format!("tests/{}.stderr", test));
    if stderr_path.exists() {
        // We found a .stderr file. Read it and compare it to the error
        // message.
        let stderr = std::fs::read_to_string(stderr_path)
            .unwrap()
            .trim()
            .to_owned();
        if &stderr == errors {
            // The error message matches the .stderr file. We pass the
            // test.
            return Ok(Value::Empty);
        } else {
            // The error message does not match the .stderr file. We
            // fail the test.
            eprintln!(
                "{}",
                Colour::Red.paint(format!(
                    "Error message does not match .stderr file for test {}",
                    test
                ))
            );
            eprintln!("Expected:\n{}", stderr);
            eprintln!("Found:\n{}", errors);
            eprintln!("Diff:");
            for line in diff::lines(&stderr, &errors) {
                match line {
                    diff::Result::Left(expected) => {
                        eprintln!("{1} {0}", expected, Colour::Green.paint("+++"));
                    }
                    diff::Result::Right(found) => {
                        eprintln!("{1} {0}", found, Colour::Red.paint("---"));
                    }
                    diff::Result::Both(a, _) => eprintln!("    {}", a),
                }
            }
            return Err(());
        }
    } else {
        return Err(());
    }
}
fn run_program(test: &str, program: &str) -> Result<Value, ()> {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let ast = parse_dwarf(&program).unwrap();
    let lu_dog = match new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak) {
        Ok(lu_dog) => lu_dog,
        Err(e) => {
            // 🚧 This isn't quite right. We should be checking for the existence
            // of a stderr file to do this, not relying on the test to fail.
            //
            // We got here because of an error. We need to look and see if this
            // test has a .stderr file. If it does, we need to compare the error
            // message to what's in the file. If they match we pass the test and
            // if they differ we fail. Passing means business as usual, failure
            // implies that we just need to return the error and let the rust
            // test framework handle the rest.
            // Look for a .stderr file.
            let errors = e
                .iter()
                .map(|e| format!("{}", dwarf::dwarf::DwarfErrorReporter(e, program, test)))
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();

            eprintln!("{errors}");
            return diff_errors(test, &errors);
        }
    };

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
    match start_main(false, ctx) {
        Ok(v) => Ok(v.0),
        Err(e) => {
            // 🚧 Likely as not I'll need some sort of thing here like I have above.
            let error = format!(
                "Interpreter exited with:\n{}",
                dwarf::ChaChaErrorReporter(&e, program, test)
            )
            .trim()
            .to_owned();
            eprintln!("{error}");
            return diff_errors(test, &error);
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
