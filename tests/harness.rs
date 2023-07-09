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

fn diff_std_err(test: &str, errors: &str) -> Result<Value, Option<i32>> {
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
            let mut diff_count = 0;
            for line in diff::lines(&stderr, &errors) {
                match line {
                    diff::Result::Left(expected) => {
                        diff_count += 1;
                        eprintln!("{1} {0}", expected, Colour::Green.paint("+++"));
                    }
                    diff::Result::Right(found) => {
                        eprintln!("{1} {0}", found, Colour::Red.paint("---"));
                    }
                    diff::Result::Both(a, _) => eprintln!("    {}", a),
                }
            }
            return Err(Some(diff_count));
        }
    } else {
        return Err(None);
    }
}

fn diff_std_out(test: &str, out: &str) -> Result<Value, i32> {
    let stdout_path = PathBuf::from(format!("tests/{}.stdout", test));
    if stdout_path.exists() {
        // We found a .stdout file. Read it and compare it to the error
        // message.
        let stdout = std::fs::read_to_string(stdout_path)
            .unwrap()
            .trim()
            .to_owned();
        if &stdout == out {
            // The output matches the .stdout file. We pass the test.
            Ok(Value::Empty)
        } else {
            // The output does not match the .stdout file. We fail the test.
            eprintln!(
                "{}",
                Colour::Red.paint(format!(
                    "Error message does not match .stdout file for test {}",
                    test
                ))
            );
            eprintln!("Expected:\n{}", stdout);
            eprintln!("Found:\n{}", out);
            eprintln!("Diff:");
            let mut diff_count = 0;
            for line in diff::lines(&stdout, &out) {
                match line {
                    diff::Result::Left(expected) => {
                        diff_count += 1;
                        eprintln!("{1} {0}", expected, Colour::Green.paint("+++"));
                    }
                    diff::Result::Right(found) => {
                        eprintln!("{1} {0}", found, Colour::Red.paint("---"));
                    }
                    diff::Result::Both(a, _) => eprintln!("    {}", a),
                }
            }
            Err(diff_count)
        }
    } else {
        Ok(Value::Empty)
    }
}

fn run_program(test: &str, program: &str) -> Result<Value, ()> {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let ast = match parse_dwarf(&test, &program) {
        Ok(ast) => ast,
        Err(dwarf::dwarf::DwarfError::Parse { error, ast: _ }) => {
            let error = error.trim();
            eprintln!("{error}");
            // The first line may be different, and there's not much I can do about
            // it. It's because the expected tokens are an opaque type on the error
            // type I'm using in the parser. I suppose I could use a different error
            // type to get around it. But that's a low priority item atm.
            match diff_std_err(test, error) {
                Err(None) => return Err(()),
                Err(Some(diff_count)) => {
                    dbg!(diff_count);
                    if diff_count < 2 {
                        return Ok(Value::Empty);
                    } else {
                        return Err(());
                    }
                }
                Ok(_) => return Ok(Value::Empty),
            }
        }
        _ => unreachable!(),
    };
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
            eprintln!(
                "{}",
                e.iter()
                    .map(|e| {
                        format!(
                            "{}",
                            dwarf::dwarf::DwarfErrorReporter(e, true, program, test)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .trim()
            );

            let errors = e
                .iter()
                .map(|e| {
                    format!(
                        "{}",
                        dwarf::dwarf::DwarfErrorReporter(e, false, program, test)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();
            return diff_std_err(test, &errors).map_err(|_| ());
        }
    };

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
    match start_main(false, ctx) {
        Ok(v) => {
            let stdout = v.1.drain_std_out().join("\n").trim().to_owned();
            match diff_std_out(test, &stdout) {
                // 🚧 We should check the result. Doing so will require that we parse magic
                // out of the source file.
                Ok(_) => Ok(v.0),
                Err(_) => Err(()),
            }
        }
        Err(e) => {
            eprintln!("{}", dwarf::ChaChaErrorReporter(&e, true, program, test));

            let error = format!(
                "Interpreter exited with:\n{}",
                dwarf::ChaChaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();
            return diff_std_err(test, &error).map_err(|_| ());
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
