use std::{
    fs,
    io::{BufReader, BufWriter},
    net::TcpListener,
    path::PathBuf,
    thread,
};

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::{ArgAction, Args, Parser};
use dap::{prelude::BasicClient, server::Server};
use dwarf::{
    chacha::dap::DapAdapter,
    dwarf::{new_lu_dog, parse_dwarf, DwarfError},
    initialize_interpreter,
    interpreter::start_repl,
    // merlin::{ErrorExpressionProxy, ExpressionProxy},
    // merlin::{
    //     AnchorProxy, BisectionProxy, EdgeProxy, GlyphProxy, LineProxy, LineSegmentPointProxy,
    //     LineSegmentProxy, PointProxy, RelationshipNameProxy, RelationshipPhraseProxy, XBoxProxy,
    // },
    interpreter::{banner2, start_main},
};

use log;
use sarzak::lu_dog::ObjectStore as LuDogStore;
use sarzak::sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL};

#[cfg(not(feature = "repl"))]
compile_error!("The REPL requires the \"repl\" feature flag..");

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
/// Run the program without an input file and it will drop you into a REPL.
///
struct Arguments {
    /// Dwarf Source File
    ///
    /// Path to the source file to execute.
    source: Option<PathBuf>,
    /// Debug Adapter Protocol (DAP) Backend
    ///
    /// Enable the DAP backend. This will start a TCP server on port 4711.
    #[arg(long, short, action=ArgAction::SetTrue)]
    dap: Option<bool>,
    /// Post-execution behavior
    ///
    /// Drop into the REPL after executing the source file.
    #[arg(long, short, action=ArgAction::SetTrue)]
    repl: Option<bool>,
    /// Print the dwarf banner
    ///
    #[arg(long, short, action=ArgAction::SetTrue)]
    banner: Option<bool>,
    #[command(flatten)]
    dwarf: DwarfArgs,
}

#[derive(Clone, Debug, Args)]
#[group(required = false, multiple = true)]
struct DwarfArgs {
    /// Dwarf main arguments
    ///
    /// These argumnets are passed on to the dwarf `main` function.
    #[arg(last = true, allow_hyphen_values = true)]
    args: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    pretty_env_logger::init();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = LuDogStore::new();

    let args = Arguments::parse();

    if let Some(source) = args.source {
        log::info!("Compiling source file {}", &source.display());

        let source_code = fs::read_to_string(&source)?;

        let ast = parse_dwarf(&source_code)?;

        let lu_dog =
            new_lu_dog(None, Some((source_code.clone(), &ast)), &[], &sarzak).map_err(|e| {
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
                                .eprint(Source::from(&source_code))
                                .unwrap()
                        }
                    }
                    _ => {}
                }
                return e;
            })?;

        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None)?;

        if args.banner.is_some() && args.banner.unwrap() {
            println!("{}", banner2());
        }

        let (_, ctx) = start_main(false, false, ctx).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;

        if args.repl.is_some() && args.repl.unwrap() {
            start_repl(ctx).map_err(|e| {
                println!("Interpreter exited with: {}", e);
                e
            })?;
        }
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
