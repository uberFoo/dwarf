use std::path::PathBuf;

use rustc_hash::FxHashMap as HashMap;

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Value,
};

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

    let mut models = HashMap::default();
    models.insert("sarzak".to_owned(), sarzak.clone());
    let lu_dog = match new_lu_dog(None, Some((program.to_owned(), &ast)), &models, &sarzak) {
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

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
    match start_main(false, ctx) {
        Ok(v) => {
            let stdout = v.1.drain_std_out().join("").trim().to_owned();

            Ok((v.0, stdout))
        }
        Err(e) => {
            // Print the "uber" error message.
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

#[test]
fn declaration() {
    let _ = env_logger::builder().is_test(true).try_init();
    let program = include_str!("proxy/declare.tao");
    run_program("proxy/declare.tao", program).unwrap();
}
