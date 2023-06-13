use std::path::PathBuf;

use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use env_logger;

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf, DwarfError},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Value,
};

fn run_program(program: &str) -> Result<Value, ()> {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let ast = parse_dwarf(&program).unwrap();
    let lu_dog = new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak)
        .map_err(|e| {
            match &e {
                DwarfError::BadSelf { span } | DwarfError::ImplementationBlock { span } => {
                    let span = span.clone();
                    let msg = format!("{}", e);

                    Report::build(ReportKind::Error, (), span.start)
                        .with_message(&msg)
                        .with_label(
                            Label::new(span)
                                .with_message(format!("{}", msg.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                        .finish()
                        .eprint(Source::from(&program))
                        .unwrap()
                }
                DwarfError::GenericWarning {
                    description: desc,
                    span,
                } => {
                    let span = span.clone();

                    Report::build(ReportKind::Error, (), span.start)
                        .with_message(&desc)
                        .with_label(
                            Label::new(span)
                                .with_message(format!("{}", desc.fg(Color::Red)))
                                .with_color(Color::Red),
                        )
                        .finish()
                        .eprint(Source::from(&program))
                        .unwrap()
                }
                DwarfError::TypeMismatch {
                    expected,
                    found,
                    span,
                } => {
                    let span = span.clone();
                    let msg = format!(
                        "{}: Type mismatch: expected `{expected}`, found `{found}`.",
                        Colour::Red.bold().paint("error")
                    );

                    Report::build(ReportKind::Error, (), span.start)
                        .with_message(&msg)
                        .with_label(Label::new(span).with_message(msg).with_color(Color::Red))
                        .finish()
                        .eprint(Source::from(&program))
                        .unwrap()
                }
                DwarfError::Parse { error: _, ast } => {
                    for a in ast {
                        let msg = format!("{}", e);
                        let span = a.1.clone();

                        Report::build(ReportKind::Error, (), span.start)
                            .with_message(&msg)
                            .with_label(
                                Label::new(span)
                                    .with_message(format!("{}", msg.fg(Color::Red)))
                                    .with_color(Color::Red),
                            )
                            .finish()
                            .eprint(Source::from(&program))
                            .unwrap()
                    }
                }
                _ => panic!("Something that needs to be taken care of! {e}"),
            }
            return e;
        })
        .unwrap();

    let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
    start_main(false, false, ctx)
        .map_err(|e| {
            println!("{e}");
        })
        .and_then(|v| Ok(v.0))
}

include!(concat!(env!("OUT_DIR"), "/tests.rs"));
