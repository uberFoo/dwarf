use std::env;

use dwarf::{
    chacha::{
        error::ChaChaErrorReporter, interpreter::initialize_interpreter, interpreter::start_func,
        value::Value,
    },
    dwarf::{new_lu_dog, parse_dwarf},
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    RefType,
};
use tracy_client::Client;

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
                            // Print the "uber" error message.
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

            println!("{}", stdout);

            Ok((v.0, stdout))
        }
        Err(e) => {
            // Print the "uber" error message.
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

#[test]
fn declaration() {
    let _ = env_logger::builder().is_test(true).try_init();
    let _ = Client::start();
    color_backtrace::install();
    let program = include_str!("proxy/declare.tao");
    run_program("proxy/declare.tao", program).unwrap();
}
