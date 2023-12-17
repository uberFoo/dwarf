#![cfg(feature = "repl")]
use std::{env, thread};

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};

use crate::{
    bubba::VM,
    chacha::error::ChaChaErrorReporter,
    dwarf::{error::DwarfErrorReporter, inter_statement, parse_line, Context as ExtruderContext},
    interpreter::{banner2, debug, eval_statement, function, Context, Error, PrintableValueType},
    lu_dog::DwarfSourceFile,
    new_ref, s_read, s_write, ChaChaError, NewRef, RefType,
};

pub fn start_repl(context: &mut Context, is_uber: bool) -> Result<(), Error> {
    use std::io;

    use rustyline::error::ReadlineError;
    use rustyline::validate::{ValidationContext, ValidationResult, Validator};
    use rustyline::{Completer, Helper, Highlighter, Hinter};
    use rustyline::{Editor, Result};

    const HISTORY_FILE: &str = ".dwarf_history";

    let models = context.models().models().clone();
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let block = context.block().clone();

    let vm_stack = context.memory().clone();
    let mut vm = VM::new(&vm_stack);

    let notice_style = Colour::Red.bold().italic();
    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let type_style = Colour::Blue.italic().dimmed();

    #[derive(Completer, Helper, Highlighter, Hinter)]
    struct DwarfValidator {}

    impl DwarfValidator {
        fn validate(&self, _input: &str) -> ValidationResult {
            ValidationResult::Valid(None)
            // match parse_line(input) {
            //     Ok(Some(_)) => ValidationResult::Valid(None),
            //     Ok(None) => ValidationResult::Incomplete,
            //     Err(e) => ValidationResult::Invalid(Some(format!("\n{e}")))
            // }
        }
    }

    impl Validator for DwarfValidator {
        fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
            Ok(self.validate(ctx.input()))
        }
    }

    println!("{}", banner2());

    let mut rl = Editor::new().map_err(|e| ChaChaError::RustyLine { source: e })?;
    let v = DwarfValidator {};
    rl.set_helper(Some(v));

    // #[cfg(feature = "with-file-history")]
    if rl.load_history(HISTORY_FILE).is_err() {
        println!("No previous history.");
    }

    println!(
        "\n{}\n",
        notice_style.paint("Currently the REPL only supports single line statements.")
    );

    let reader = context.std_out_recv().clone();
    let handle = thread::spawn(move || loop {
        match reader.recv() {
            Ok(line) => {
                print!("{}", line);
                io::Write::flush(&mut io::stdout()).unwrap();
            }
            Err(_) => {
                debug!("Debugger control thread exiting");
                break;
            }
        };
    });

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    let mut stmt_index = 0;
    loop {
        let readline = rl.readline(&format!("{} ", prompt_style.paint("道:>")));
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .map_err(|e| ChaChaError::RustyLine { source: e })?;

                // Should do a regex here, or something.
                match parse_line(&line) {
                    Ok(Some((stmt, _))) => {
                        debug!("stmt from readline {stmt:?}");

                        let mut dirty = Vec::new();
                        let stmt = {
                            let mut lu_dog = s_write!(lu_dog);
                            match inter_statement(
                                &new_ref!(crate::dwarf::Statement, stmt),
                                stmt_index,
                                &block,
                                &mut ExtruderContext {
                                    location: location!(),
                                    struct_fields: Vec::new(),
                                    source: DwarfSourceFile::new(line.clone(), &mut lu_dog),
                                    models: &mut s_write!(models),
                                    sarzak: &s_read!(sarzak),
                                    dwarf_home: &dwarf_home,
                                    cwd: env::current_dir().unwrap(),
                                    dirty: &mut dirty,
                                    file_name: "REPL",
                                    func_defs: HashMap::default(),
                                },
                                &mut lu_dog,
                            ) {
                                Ok(stmt) => stmt.0,
                                Err(errors) => {
                                    for e in errors {
                                        println!("{}", DwarfErrorReporter(&e, is_uber, &line));
                                    }
                                    continue;
                                }
                            }
                        };
                        stmt_index += 1;

                        context.set_dirty(dirty);

                        match eval_statement(stmt.0, context, &mut vm) {
                            Ok(value) => {
                                let ty = s_read!(value)
                                    .get_value_type(&s_read!(sarzak), &s_read!(lu_dog));
                                let value = format!("{}", s_read!(value));
                                print!("\n'{}'", result_style.paint(value));

                                let ty = PrintableValueType(true, ty, context.models());
                                let ty = format!("{}", ty);
                                println!("\t  ──➤  {}", type_style.paint(ty));
                            }
                            Err(e) => {
                                let adiós = matches!(e, ChaChaError::Return { value: _, ty: _ });

                                println!(
                                    "{}",
                                    ChaChaErrorReporter(&Error(e), is_uber, &line, "REPL")
                                );

                                if adiós {
                                    println!("👋 Bye bye!");
                                    break;
                                }
                            }
                        }
                    }
                    Ok(None) => {
                        continue;
                    }
                    Err(e) => {
                        eprintln!("{e}");
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    // #[cfg(feature = "with-file-history")]
    rl.save_history(HISTORY_FILE)
        .map_err(|e| ChaChaError::RustyLine { source: e })?;

    handle.join().unwrap();

    Ok(())
}
