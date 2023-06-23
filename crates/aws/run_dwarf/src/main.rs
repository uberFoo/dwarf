use std::{io::Write, path::PathBuf};

use ansi_term::Colour;
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf, DwarfError},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};
use lambda_http::{
    run, run_with_streaming_response, service_fn, Body, Error, Request, RequestExt, Response,
};
use tracing::{event, Level};

/// This is the main body for the function.
/// Write your code inside it.
/// There are some code example in the following URLs:
/// - https://github.com/awslabs/aws-lambda-rust-runtime/tree/main/examples
async fn function_handler(event: Request) -> Result<Response<Body>, Error> {
    let (mut tx, rx) = hyper::Body::channel();

    // tokio::spawn(async move {
    //     for message in messages.iter() {
    //         tx.send_data((*message).into()).await.unwrap();
    //         thread::sleep(Duration::from_millis(500));
    //     }
    // });

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let message = if let Body::Text(program) = event.into_body() {
        let mut std_err = Vec::new();

        event!(Level::INFO, "dwarf received program");

        let ast = parse_dwarf(&program).map_err(|e| {
            std_err.write(format!("{}", e).as_bytes()).unwrap();
        });

        event!(Level::INFO, "dwarf parsed program");

        match ast {
            Ok(ast) => {
                let lu_dog = new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak)
                    .map_err(|e| {
                        // Dang. I'd like to refactor the report creation up here, and then just
                        // add details below. The problem is that I don't have the span information
                        // until I get into the error.
                        match &e {
                            DwarfError::BadSelf { span }
                            | DwarfError::ImplementationBlock { span } => {
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
                                    .write(Source::from(&program), &mut std_err)
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
                                    .write(Source::from(&program), &mut std_err)
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
                                    .with_label(
                                        Label::new(span).with_message(msg).with_color(Color::Red),
                                    )
                                    .finish()
                                    .write(Source::from(&program), &mut std_err)
                                    .unwrap()
                            }
                            DwarfError::Parse { error, ast } => {
                                std_err.write(format!("{}", e).as_bytes()).unwrap();

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
                                        .write(Source::from(&program), &mut std_err)
                                        .unwrap()
                                }
                            }
                            _ => panic!("Something that needs to be taken care of! {e}"),
                        }
                        e
                    });

                match lu_dog {
                    Ok(lu_dog) => {
                        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
                        // 🚧 Can't unwrap this. Must deal with errors from ChaCha.
                        let (result, ctx) = start_main(false, ctx).unwrap().try_into().unwrap();
                        let result = result.to_string();
                        let result = ansi_to_html::convert_escaped(&result).unwrap();
                        let std_out = ctx.drain_std_out();
                        let std_out = std_out
                            .iter()
                            .map(|line| ansi_to_html::convert_escaped(&line).unwrap())
                            .collect::<Vec<String>>()
                            .join("");
                        std_out + &ansi_to_html::convert_escaped(&result).unwrap()
                    }
                    Err(_) => {
                        let err = String::from_utf8(std_err).unwrap();
                        ansi_to_html::convert_escaped(&err).unwrap()
                    }
                }
            }
            Err(_) => {
                let err = String::from_utf8(std_err).unwrap();
                ansi_to_html::convert_escaped(&err).unwrap()
            }
        }
    } else {
        event!(Level::INFO, "dwarf received no program");
        "No program provided".to_string()
    };

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
