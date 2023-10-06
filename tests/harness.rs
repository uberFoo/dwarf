use std::{env, path::PathBuf};

use ansi_term::Colour;
use tracy_client::Client;

use dwarf::{
    chacha::{
        error::ChaChaErrorReporter,
        interpreter::{initialize_interpreter, start_func},
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

/// Spawns a future on a new dedicated thread.
///
/// The returned task can be used to await the output of the future.
#[cfg(feature = "async")]
fn spawn_on_thread<F, T>(future: F) -> Task<T>
where
    F: Future<Output = T> + Send + 'static,
    T: Send + 'static,
{
    // Create a channel that holds the task when it is scheduled for running.
    let (sender, receiver) = flume::unbounded();
    let sender = Arc::new(sender);
    let s = Arc::downgrade(&sender);

    // Wrap the future into one that disconnects the channel on completion.
    let future = async move {
        // When the inner future completes, the sender gets dropped and disconnects the channel.
        let _sender = sender;
        future.await
    };

    // Create a task that is scheduled by sending it into the channel.
    let schedule = move |runnable| s.upgrade().unwrap().send(runnable).unwrap();
    let (runnable, task) = async_task::spawn(future, schedule);

    // Schedule the task by sending it into the channel.
    runnable.schedule();

    // Spawn a thread running the task to completion.
    thread::spawn(move || {
        // Keep taking the task from the channel and running it until completion.
        for runnable in receiver {
            runnable.run();
        }
    });

    task
}

fn run_program(test: &str, program: &str) -> Result<(RefType<Value>, String), String> {
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
    let ctx = match new_lu_dog(Some((program.to_owned(), &ast)), &dwarf_home, &sarzak) {
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

    let ctx = initialize_interpreter(dwarf_home, ctx, sarzak).unwrap();
    match start_func("main", false, ctx) {
        Ok(v) => {
            let stdout = v.1.drain_std_out().join("").trim().to_owned();

            println!("{stdout}");

            Ok((v.0, stdout))
        }
        Err(e) => {
            eprintln!("{}", ChaChaErrorReporter(&e, true, program, test));

            let error = format!(
                "Interpreter exited with:\n{}",
                ChaChaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            Err(error)
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
