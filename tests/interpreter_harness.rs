use std::{env, path::PathBuf};

#[cfg(feature = "async")]
use futures_lite::future;

#[cfg(feature = "async")]
use dwarf::ref_to_inner;

use ansi_term::Colour;
use lazy_static::lazy_static;
use parking_lot::Mutex;

use dwarf::{
    chacha::{
        error::ChaChaErrorReporter,
        interpreter::{initialize_interpreter, start_func},
        value::Value,
    },
    dwarf::{new_lu_dog, parse_dwarf},
    s_read,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};

lazy_static! {
    static ref EXEC_MUTEX: Mutex<()> = Mutex::new(());
}

#[cfg(feature = "print-std-out")]
compile_error!("The tests don't function with the print-std-out feature enabled.");

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

fn diff_with_file(path: &str, test: &str, found: &str) -> Result<(), ()> {
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
    let _guard = EXEC_MUTEX.lock();
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let ast = match parse_dwarf(test, program) {
        Ok(ast) => ast,
        Err(e) => match *e {
            dwarf::dwarf::error::DwarfError::Parse { error, ast: _ } => {
                let error = error.trim();
                eprintln!("{error}");
                return Err(error.to_owned());
            }
            e => {
                eprintln!("{e:?}");
                return Err(e.to_string());
            }
        },
    };

    let ctx = match new_lu_dog(
        test.to_owned(),
        Some((program.to_owned(), &ast)),
        &dwarf_home,
        &sarzak,
    ) {
        Ok(lu_dog) => lu_dog,
        Err(e) => {
            eprintln!(
                "{}",
                e.iter()
                    .map(|e| {
                        format!(
                            "{}",
                            // This one is uber.
                            dwarf::dwarf::error::DwarfErrorReporter(e, true)
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
                        // This one is not uber.
                        dwarf::dwarf::error::DwarfErrorReporter(e, false)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();

            return Err(errors);
        }
    };

    let mut ctx = initialize_interpreter(1, dwarf_home, ctx).unwrap();
    let result = match start_func("main", false, &mut ctx) {
        Ok(value) => {
            #[cfg(not(feature = "async"))]
            let value = (*s_read!(value)).clone();
            #[cfg(feature = "async")]
            let value = {
                unsafe {
                    let value = std::sync::Arc::into_raw(value);
                    let value = std::ptr::read(value);
                    let value = ref_to_inner!(value);

                    let value = future::block_on(value);

                    let value = std::sync::Arc::into_raw(value);
                    let value = std::ptr::read(value);
                    ref_to_inner!(value)
                }
            };

            match value {
                Value::Error(msg) => {
                    let msg = *msg;
                    let error = format!(
                        "Interpreter exited with:\n{}",
                        ChaChaErrorReporter(&msg.into(), true, program, test)
                    )
                    .trim()
                    .to_owned();

                    eprintln!("{error}");

                    Err(error)
                }
                _ => {
                    let stdout = ctx.drain_std_out().join("").trim().to_owned();
                    Ok((value, stdout))
                }
            }
        }
        Err(e) => {
            let error = format!(
                "Interpreter exited with:\n{}",
                ChaChaErrorReporter(&e, true, program, test)
            )
            .trim()
            .to_owned();

            eprintln!("{error}");

            let error = format!(
                "Interpreter exited with:\n{}",
                ChaChaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            Err(error)
        }
    };

    result
}

// This loads the generated tests.
include!(concat!(env!("OUT_DIR"), "/tests.rs"));
