use std::{
    fs,
    io::{BufReader, BufWriter},
    net::TcpListener,
    path::PathBuf,
    thread,
};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;
use dap::{prelude::BasicClient, server::Server};
use dwarf::{
    chacha::dap::DapAdapter,
    dwarf::{parse_dwarf, populate_lu_dog, DwarfError},
    initialize_interpreter,
    interpreter::{banner2, start_main},
    start_repl,
    // merlin::{ErrorExpressionProxy, ExpressionProxy},
    // merlin::{
    //     AnchorProxy, BisectionProxy, EdgeProxy, GlyphProxy, LineProxy, LineSegmentPointProxy,
    //     LineSegmentProxy, PointProxy, RelationshipNameProxy, RelationshipPhraseProxy, XBoxProxy,
    // },
};
use log;
use sarzak::lu_dog::ObjectStore as LuDogStore;
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Args {
    /// Dwarf Source File
    ///
    /// Path to the source file to compile.
    #[arg(long, short, group = "input")]
    source: Option<PathBuf>,
    /// Debug Adapter Protocol (DAP) Backend
    ///
    /// Enable the DAP backend. This will start a TCP server on port 4711.
    #[arg(long, short, action, group = "input")]
    dap: Option<bool>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = LuDogStore::new();

    let args = Args::parse();

    if let Some(source) = args.source {
        log::info!("Compiling source file {}", &source.display());

        let source_code = fs::read_to_string(&source)?;

        let ast = parse_dwarf(&source_code)?;

        let lu_dog =
            populate_lu_dog(None, source_code.clone(), &ast, &[], &sarzak).map_err(|e| {
                match &e {
                    DwarfError::BadSelf { span } => {
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
                            .eprint(Source::from(&source_code))
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
                            .eprint(Source::from(&source_code))
                            .unwrap()
                    }
                    DwarfError::ImplementationBlock { span } => {
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
                            .eprint(Source::from(&source_code))
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
                                .eprint(Source::from(&source_code))
                                .unwrap()
                        }
                    }
                    _ => {}
                }
                return e;
            })?;

        // let mut ctx = initialize_interpreter_paths("../sarzak/target/sarzak/lu_dog/lu_dog.道")?;
        // let mut ctx = initialize_interpreter_paths("fib.道")?;
        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None)?;

        // ctx.register_model("../sarzak/models/lu_dog.v2.json")?;

        // ctx.register_store_proxy(
        //     "ExpressionProxy".to_owned(),
        //     ExpressionProxy::new_type(ctx.lu_dog_heel()),
        // );

        // ctx.register_store_proxy(
        //     "ErrorExpressionProxy".to_owned(),
        //     ErrorExpressionProxy::new_type(ctx.lu_dog_heel()),
        // );

        // let result = start_vm(20).map_err(|e| {
        //     println!("Interpreter exited with: {}", e);
        //     e
        // });
        // println!("Interpreter exited with: {:?}", result);
        // Ok(())

        // if args.vscode {
        //     println!(
        //         "\nreturned: {}",
        //         start_main(true, ctx).map_err(|e| {
        //             println!("Interpreter exited with: {}", e);
        //             e
        //         })?
        //     );
        // // let stdin = io::stdin(); // We get `Stdin` here.
        // // stdin.read_line(&mut buffer)?;
        // } else {
        println!("{}", banner2());

        start_main(false, false, ctx).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;
        // }
    } else if let Some(_) = args.dap {
        let listener = TcpListener::bind("127.0.0.1:4711").unwrap();
        println!("Listening on port {}", listener.local_addr().unwrap());
        // let in_file = fs::File::create("/tmp/socket_in.txt").unwrap();
        // let mut in_file = BufWriter::new(in_file);
        loop {
            let (stream, addr) = listener.accept().unwrap();
            println!("Connection received from {}", addr);

            // let (send, recv) = channel::unbounded();

            let stream2 = stream.try_clone().unwrap();
            thread::spawn(move || {
                // let mut buffer = Cursor::new(vec![0; 1024]);

                let adapter = DapAdapter {};
                let client = BasicClient::new(BufWriter::new(stream2));
                let mut server = Server::new(adapter, client);
                let mut reader = BufReader::new(&stream);
                server.run(&mut reader).unwrap();

                // while let Ok(msg) = recv.recv() {
                // println!("Received: {:?}", msg);
                // *buffer.get_mut() = msg;
                // }
            });

            // let mut reader = BufReader::new(stream.try_clone().unwrap());
            // let mut buffer = String::new();

            // while let Ok(_len) = reader.read_line(&mut buffer) {
            //     println!("Received: {}", buffer);
            //     send.send(buffer.as_bytes().to_vec()).unwrap();
            // }

            //     in_file.write_all(line.as_bytes())?;
            //     in_file.write_all(b"\n")?;
            //     in_file.flush()?;
            // }
        }
    } else {
        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None)?;

        start_repl(ctx).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;
    }

    Ok(())

    // start_main(ctx).map_err(|e| {
    //     println!("Interpreter exited with: {}", e);
    //     e
    // })
}
