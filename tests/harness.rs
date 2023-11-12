use std::{env, path::PathBuf};

use ansi_term::Colour;
use tracy_client::Client;

use dwarf::{
    chacha::{
        error::ChaChaErrorReporter,
        interpreter::{initialize_interpreter, shutdown_interpreter, start_func},
        value::Value,
    },
    dwarf::{new_lu_dog, parse_dwarf},
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    RefType,
};

cfg_if::cfg_if! {
    if #[cfg(feature = "async")] {
        use std::future::Future;
        use std::sync::Arc;
        use std::thread;
        use smol::Task;
        use smol::future;
    }
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
        Err(dwarf::dwarf::error::DwarfError::Parse { error, ast: _ }) => {
            let error = error.trim();
            eprintln!("{error}");
            return Err(error.to_owned());
        }
        _ => unreachable!(),
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
                            dwarf::dwarf::error::DwarfErrorReporter(e, true, program)
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
                        dwarf::dwarf::error::DwarfErrorReporter(e, false, program)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();

            return Err(errors);
        }
    };

    let mut ctx = initialize_interpreter(1, dwarf_home, ctx, sarzak).unwrap();
    let result = match start_func("main", false, &mut ctx) {
        Ok(value) => {
            unsafe {
                let value = std::sync::Arc::into_raw(value);
                let value = std::ptr::read(value);
                let value = value.into_inner().unwrap();
                let value = future::block_on(value);

                let value = std::sync::Arc::into_raw(value);
                let value = std::ptr::read(value);
                let value = value.into_inner().unwrap();
                match value {
                    Value::Error(msg) => {
                        let msg = *msg;
                        let error = format!(
                            "Interpreter exited with:\n{}",
                            ChaChaErrorReporter(&msg.into(), false, program, test)
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

            // let stdout = ctx.drain_std_out().join("").trim().to_owned();
            // Ok((value, stdout))

            // ctx.executor().shutdown();
            // let _ = future::block_on(async { ctx.executor().run().await });
            // #[cfg(feature = "async")]
            // unsafe {
            //     let v = std::sync::Arc::into_raw(value.clone());
            //     let v = std::ptr::read(v);
            //     let v = v.into_inner().unwrap();
            //     match v {
            //         Value::Future(name, mut task) => {
            //             dbg!(&name);
            //             let task = task.take().unwrap();
            //             match ctx.executor().block_on(task) {
            //                 Ok(value) => {
            //                     let stdout = ctx.drain_std_out().join("").trim().to_owned();

            //                     println!("{stdout}");

            //                     Ok((value, stdout))
            //                 }
            //                 Err(e) => {
            //                     let e = e.into();
            //                     eprintln!("{}", ChaChaErrorReporter(&e, true, program, test));

            //                     let error = format!(
            //                         "Interpreter exited with:\n{}",
            //                         ChaChaErrorReporter(&e, false, program, test)
            //                     )
            //                     .trim()
            //                     .to_owned();

            //                     Err(error)
            //                 }
            //             }
            //         }
            //         _ => {
            //             let stdout = ctx.drain_std_out().join("").trim().to_owned();

            //             println!("{stdout}");

            //             Ok((value, stdout))
            //         }
            //     }
            // }
        }
        Err(e) => {
            let error = format!(
                "Interpreter exited with:\n{}",
                ChaChaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            eprintln!("{error}");

            Err(error)
        }
    };

    shutdown_interpreter();

    result
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
