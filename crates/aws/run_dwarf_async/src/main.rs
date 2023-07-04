use std::{io::Write, path::PathBuf};
use std::{thread, time::Duration};

use dwarf::{
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::start_main,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};
use hyper::{Body, Response};
use lambda_http::{
    run, run_with_streaming_response, service_fn, Body as RequestBody, Error, Request, RequestExt,
    Response as RequestResponse,
};
use lambda_runtime::LambdaEvent;
use tracing::{event, Level};

/// This is the main body for the function.
/// Write your code inside it.
/// There are some code example in the following URLs:
/// - https://github.com/awslabs/aws-lambda-rust-runtime/tree/main/examples
// async fn function_handler(event: Request) -> Result<Response<Body>, Error> {
async fn function_handler(event: LambdaEvent<serde_json::Value>) -> Result<Response<Body>, Error> {
    let (mut tx, rx) = hyper::Body::channel();
    let (otx, orx) = crossbeam::channel::bounded(1);

    // tokio::spawn(async move {
    //     for message in messages.iter() {
    //         tx.send_data((*message).into()).await.unwrap();
    //         thread::sleep(Duration::from_millis(500));
    //     }
    // });

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    // let message = if let Body::Text(program) = event.into_body() {
    // let handle = if let Some(body) = event.payload.get("body") {
    if let Some(body) = event.payload.get("body") {
        let program = body.as_str().unwrap();
        let mut std_err = Vec::new();

        event!(Level::INFO, "dwarf received program: {}", program);

        let ast = parse_dwarf("lambda", &program).map_err(|e| {
            // std_err.write(format!("{}", e).as_bytes()).unwrap();
            tx.try_send_data(format!("{}", e).into()).unwrap();
        });

        event!(Level::INFO, "dwarf parsed program");

        match ast {
            Ok(ast) => {
                let lu_dog = new_lu_dog(None, Some((program.to_owned(), &ast)), &[], &sarzak)
                    .map_err(|errors| {
                        for e in &errors {
                            tx.try_send_data(
                                format!(
                                    "{}",
                                    dwarf::dwarf::DwarfErrorReporter(&e, &program, "lambda")
                                )
                                .into(),
                            )
                            .unwrap();
                            // std_err
                            //     .write(
                            //         format!("{}", dwarf::dwarf::DwarfErrorReporter(&e, &program))
                            //             .as_bytes(),
                            //     )
                            //     .unwrap();
                        }
                        errors
                    });

                match lu_dog {
                    Ok(lu_dog) => {
                        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None).unwrap();
                        let std_out = ctx.get_std_out();

                        // tokio::spawn(async move {
                        //     for message in messages.iter() {
                        //         tx.send_data((*message).into()).await.unwrap();
                        //         thread::sleep(Duration::from_millis(500));
                        //     }
                        // });

                        let reader = tokio::task::spawn_blocking(move || loop {
                            match std_out.recv() {
                                Ok(output) => {
                                    let output = ansi_to_html::convert_escaped(&output).unwrap();
                                    // let output = serde_json::json!({
                                    //     "output": output,
                                    // })
                                    // .to_string();
                                    event!(Level::INFO, output);
                                    let mut result = tx.try_send_data(output.clone().into());
                                    event!(Level::INFO, "result: {:?}", result);
                                    // while result.is_err() {
                                    //     thread::sleep(std::time::Duration::from_millis(50));
                                    //     result = tx.try_send_data(output.clone().into());
                                    // }
                                }
                                Err(_) => {
                                    event!(Level::INFO, "std_out is kaput");
                                    break;
                                }
                            }
                            let err: Result<String, crossbeam::channel::RecvError> = orx.recv();
                            if err.is_ok() {
                                let err = err.unwrap();
                                let err = ansi_to_html::convert_escaped(&err).unwrap();
                                // let err = serde_json::json!({
                                //     "output": err,
                                // })
                                // .to_string();
                                event!(Level::INFO, err);
                                let mut result = tx.try_send_data(err.clone().into());
                                // while result.is_err() {
                                //     thread::sleep(std::time::Duration::from_millis(50));
                                //     result = tx.try_send_data(err.clone().into());
                                // }

                                break;
                            }
                        });
                        let ctx = ctx.clone();
                        let writer = tokio::task::spawn_blocking(|| match start_main(false, ctx) {
                            Ok((result, ctx)) => {
                                let result = result.to_string();
                                let result = ansi_to_html::convert_escaped(&result).unwrap();
                                // let std_out = ctx.drain_std_out();
                                // let std_out = std_out
                                //     .iter()
                                //     .map(|line| ansi_to_html::convert_escaped(&line).unwrap())
                                //     .collect::<Vec<String>>()
                                //     .join("");
                                // std_out + &ansi_to_html::convert_escaped(&result).unwrap()
                                result
                            }
                            Err(e) => {
                                let err = ansi_to_html::convert_escaped(&format!(
                                    "{}",
                                    dwarf::ChaChaErrorReporter(&e, &program, "lambda")
                                ))
                                .unwrap();

                                event!(Level::INFO, "{}", err.clone());

                                otx.send(err).unwrap();

                                "error".to_owned()
                            }
                        });

                        // Some((reader, writer))
                        reader.await.unwrap();
                        writer.await.unwrap();
                    }
                    Err(_) => {
                        let err = String::from_utf8(std_err).unwrap();
                        let err = ansi_to_html::convert_escaped(&err).unwrap();
                        event!(Level::INFO, "{}", err.clone());

                        tx.try_send_data(err.into()).unwrap();

                        // None
                    }
                }
            }
            Err(_) => {
                let err = String::from_utf8(std_err).unwrap();
                let err = ansi_to_html::convert_escaped(&err).unwrap();
                event!(Level::INFO, "{}", err.clone());

                tx.try_send_data(err.into()).unwrap();

                // None
            }
        }
    } else {
        event!(Level::INFO, "dwarf received no program");
        "No program provided".to_string();
        // None
    };

    // Return something that implements IntoResponse.
    // It will be serialized to the right response event automatically by the runtime
    let resp = Response::builder()
        .status(200)
        .header("content-type", "text/html")
        // .body(message.into())
        .body(rx)
        .map_err(Box::new)?;

    // if let Some(handle) = handle {
    //     handle.1.await.unwrap();
    //     handle.0.await.unwrap();
    // }

    Ok(resp)
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        // disable printing the name of the module in every log line.
        .with_target(false)
        // .with_ansi(false)
        // disabling time is handy because CloudWatch will add the ingestion time.
        .without_time()
        .init();

    // run(service_fn(function_handler)).await
    lambda_runtime::run_with_streaming_response(service_fn(function_handler)).await?;

    Ok(())
}
