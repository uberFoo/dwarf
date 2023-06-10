use std::path::PathBuf;

use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use dwarf::{
    dwarf::{parse_dwarf, populate_lu_dog, DwarfError},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Value,
};
use lambda_http::{run, service_fn, Body, Error, Request, RequestExt, Response};

/// This is the main body for the function.
/// Write your code inside it.
/// There are some code example in the following URLs:
/// - https://github.com/awslabs/aws-lambda-rust-runtime/tree/main/examples
async fn function_handler(event: Request) -> Result<Response<Body>, Error> {
    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let message = if let Body::Text(program) = event.into_body() {
        let ast = parse_dwarf(&program).unwrap();

        let lu_dog = populate_lu_dog(None, program.to_owned(), &ast, &[], &sarzak)
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
                    DwarfError::Parse { ast } => {
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
        let message: Value = start_main(false, false, ctx).unwrap().try_into().unwrap();
        let message = message.to_string();
        ansi_to_html::convert_escaped(&message).unwrap()
    } else {
        "No program provided".to_string()
    };

    // let message = "Hello, world!";

    // Return something that implements IntoResponse.
    // It will be serialized to the right response event automatically by the runtime
    let resp = Response::builder()
        .status(200)
        .header("content-type", "text/html")
        .body(message.into())
        .map_err(Box::new)?;
    Ok(resp)
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        // disable printing the name of the module in every log line.
        .with_target(false)
        // disabling time is handy because CloudWatch will add the ingestion time.
        .without_time()
        .init();

    run(service_fn(function_handler)).await
}
