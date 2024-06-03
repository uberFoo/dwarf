use std::{env, path::PathBuf};

use test_log::test;

use dwarf::{
    bubba::{
        compiler::{compile, BubbaCompilerErrorReporter},
        error::BubbaErrorReporter,
        value::Value,
        VM,
    },
    dwarf::{new_lu_dog, parse_dwarf},
    s_read,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};
#[cfg(feature = "tracy")]
use tracy_client::Client;

const NUM_THREADS: usize = 4;

fn run_program(test: &str, program: &str, cwd: &PathBuf) -> Result<(Value, String), String> {
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
        &env::current_dir().unwrap(),
        true,
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
                            // Print the "uber" error message.
                            dwarf::dwarf::error::DwarfErrorReporter(e, true)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .trim()
            );

            let errors = e
                .iter()
                .map(|e| format!("{}", dwarf::dwarf::error::DwarfErrorReporter(e, false)))
                .collect::<Vec<_>>()
                .join("\n")
                .trim()
                .to_owned();

            return Err(errors);
        }
    };

    let binary = match compile(&ctx, true) {
        Ok(program) => program,
        Err(e) => {
            let error = format!(
                "Unable to compile program:\n{}",
                BubbaCompilerErrorReporter(&e, true, program, test)
            )
            .trim()
            .to_owned();

            eprintln!("{error}");

            let error = format!(
                "Unable to compile program:\n{}",
                BubbaCompilerErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            return Err(error);
        }
    };

    #[cfg(feature = "async")]
    let mut vm = VM::new(&binary, &[], &dwarf_home, NUM_THREADS, false);
    #[cfg(not(feature = "async"))]
    let mut vm = VM::new(&program, &[], &dwarf_home, false);

    let result = match vm.invoke("main", &[]) {
        Ok(value) => {
            let value = s_read!(value).clone();

            match value {
                Value::Error(msg) => {
                    let msg = *msg;
                    let error = format!(
                        "Vm exited with:\n{}",
                        BubbaErrorReporter(&msg.into(), true, program, test)
                    )
                    .trim()
                    .to_owned();

                    eprintln!("{error}");

                    Err(error)

                    // eprintln!("{msg}");
                    // Err(msg.to_string())
                }
                _ => Ok((value, "Oops".to_owned())),
            }
        }
        Err(e) => {
            let error = format!(
                "VM exited with:\n{}",
                BubbaErrorReporter(&e, true, program, test)
            )
            .trim()
            .to_owned();

            eprintln!("{error}");

            let error = format!(
                "VM exited with:\n{}",
                BubbaErrorReporter(&e, false, program, test)
            )
            .trim()
            .to_owned();

            Err(error)
        }
    };

    result
}

#[test_log::test]
fn declaration() {
    let _ = env_logger::builder().is_test(true).try_init();
    #[cfg(feature = "tracy")]
    let _ = Client::start();
    color_backtrace::install();

    let program = include_str!("proxy/declare.ore");
    let cwd = env::current_dir().unwrap();
    // This needs to be fixed
    // run_program("proxy/declare.tao", program, &cwd).unwrap();
}
