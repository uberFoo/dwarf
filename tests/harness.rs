use std::path::PathBuf;

use ansi_term::Colour;

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Value,
};

fn output_diffs(expected: &str, found: &str, test: &str) -> Result<(), ()> {
    let mut diff_count = 0;
    let mut diff = String::new();
    for line in diff::lines(expected, found) {
        match line {
            diff::Result::Left(expected) => {
                if expected.starts_with("[31mError:[0m Unexpected token in input, expected") {
                    continue;
                }
                diff_count += 1;
                diff += &format!("{} {expected}\n", Colour::Green.paint("+++"));
            }
            diff::Result::Right(found) => {
                if found.starts_with("[31mError:[0m Unexpected token in input, expected") {
                    continue;
                }
                diff += &format!("{} {found}\n", Colour::Red.paint("---"));
            }
            diff::Result::Both(a, _) => eprintln!("    {a}"),
        }
    }

    if diff_count > 0 {
        eprintln!(
            "{}",
            Colour::Red.paint(format!(
                "stderr does not match .stderr file for test {test}"
            ))
        );
        eprintln!("Expected:\n{expected}");
        eprintln!("Found:\n{found}");
        eprintln!("Diff:\n{diff}");
        Err(())
    } else {
        Ok(())
    }
}

fn diff_std_err(path: &str, test: &str, errors: &str) -> Result<(), ()> {
    let path = PathBuf::from(path);
    let stderr = std::fs::read_to_string(path).unwrap().trim().to_owned();
    if stderr == errors {
        // The error message matches the .stderr file. We pass the
        // test.
        Ok(())
    } else {
        // The error message does not match the .stderr file. Let's do a diff and
        // see what's up.
        output_diffs(&stderr, errors, test)
    }
}

fn diff_std_out(path: &str, test: &str, found: &str) -> Result<(), ()> {
    let path = PathBuf::from(path);
    let stdout = std::fs::read_to_string(path).unwrap().trim().to_owned();
    if stdout == found {
        // The output matches the .stdout file. We pass the test.
        Ok(())
    } else {
        // The output does not match the .stdout file -- do a diff.
        output_diffs(&stdout, found, test)
    }
}

fn run_program(test: &str, program: &str) -> Result<(Value, String), String> {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let ast = match parse_dwarf(test, program) {
        Ok(ast) => ast,
        Err(dwarf::dwarf::error::DwarfError::Parse { error, ast: _ }) => {
            let error = error.trim();
            eprintln!("{error}");
            return Err(error.to_owned());
        }
        _ => unreachable!(),
    };
    let lu_dog = match new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak) {
        Ok(lu_dog) => lu_dog,
        Err(e) => {
            eprintln!(
                "{}",
                e.iter()
                    .map(|e| {
                        format!(
                            "{}",
                            dwarf::dwarf::error::DwarfErrorReporter(e, true, program, test)
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
                        dwarf::dwarf::error::DwarfErrorReporter(e, false, program, test)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();

            return Err(errors);
        }
    };

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
    match start_main(false, ctx) {
        Ok(v) => {
            let stdout = v.1.drain_std_out().join("").trim().to_owned();

            Ok((v.0, stdout))
        }
        Err(e) => {
            eprintln!("{}", dwarf::ChaChaErrorReporter(&e, true, program, test));

            let error = format!(
                "Interpreter exited with:\n{}",
                dwarf::ChaChaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            Err(error)
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
