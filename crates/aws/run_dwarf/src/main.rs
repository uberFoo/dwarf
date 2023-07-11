use std::{io::Write, path::PathBuf};

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
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

        let ast = parse_dwarf("lambda", &program).map_err(|e| {
            std_err.write(format!("{}", e).as_bytes()).unwrap();
        });

        event!(Level::INFO, "dwarf parsed program");

        match ast {
            Ok(ast) => {
                let lu_dog = new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak)
                    .map_err(|errors| {
                        for e in &errors {
                            std_err
                                .write(
                                    format!(
                                        "{}",
                                        dwarf::dwarf::DwarfErrorReporter(
                                            &e, false, &program, "lambda"
                                        )
                                    )
                                    .as_bytes(),
                                )
                                .unwrap();
                        }
                        errors
                    });

                match lu_dog {
                    Ok(lu_dog) => {
                        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
                        match start_main(false, ctx) {
                            Ok((result, ctx)) => {
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
                            Err(e) => ansi_to_html::convert_escaped(&format!(
                                "{}",
                                dwarf::ChaChaErrorReporter(&e, false, &program, "lambda")
                            ))
                            .unwrap(),
                        }
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
