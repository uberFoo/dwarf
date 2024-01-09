use std::{
    env, fs,
    io::{self, BufReader, BufWriter},
    net::TcpListener,
    path::PathBuf,
    thread,
};

#[cfg(feature = "async")]
use futures_lite::future;

// #[cfg(feature = "async")]
// use dwarf::chacha::interpreter::Executor;

use clap::{ArgAction, Args, Parser};
use dap::{prelude::BasicClient, server::Server};

// #[cfg(feature = "async")]
// use smol::future;
#[cfg(feature = "async")]
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

#[cfg(feature = "async")]
use dwarf::ref_to_inner;

use dwarf::{
    bubba::{compiler::compile, VM},
    chacha::{
        dap::DapAdapter,
        error::{ChaChaError, ChaChaErrorReporter},
        interpreter::{banner2, initialize_interpreter, start_func, start_repl},
    },
    dwarf::{new_lu_dog, parse_dwarf},
    new_ref,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
    Context, NewRef, RefType, Value,
};
use reqwest::Url;
#[cfg(feature = "tracy")]
use tracy_client::Client;

#[cfg(not(feature = "repl"))]
compile_error!("The REPL requires the \"repl\" feature flag..");

#[derive(Clone, Debug)]
enum Source {
    File(PathBuf),
    Url(Url),
}

fn validate_source(s: &str) -> Result<Source, String> {
    if s.starts_with("http") {
        let url = Url::parse(s).map_err(|e| e.to_string())?;
        Ok(Source::Url(url))
    } else {
        let path = PathBuf::from(s);
        if path.exists() {
            Ok(Source::File(path))
        } else {
            Err(format!("{} does not exist", s))
        }
    }
}

#[derive(Debug, Parser)]
#[command(
    author,
    version,
    about,
    long_about = r#"
This is the dwarf interpreter, ChaCha.

By default, with no arguments you will be dropped into a REPL. If you pass
a source file, it will be executed and return to your shell.

This default behavior may be modified by using any of the options below.
"#
)]
#[command(propagate_version = true)]
/// This is the dwarf interpreter, ChaCha.
///
/// By default, with no arguments you will be dropped into a REPL. If you pass
/// a source file, it will be executed and return to your shell.
///
/// This default behavior may be modified by using any of the options below.
///
struct Arguments {
    /// Dwarf Source File
    ///
    /// Local path, or URL of the source file to execute.
    #[arg(value_parser=validate_source)]
    source: Option<Source>,
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
    dwarf_args: DwarfArgs,
    /// Bless a test
    ///
    /// This is only useful if you are writing tests for dwarf. I'd really like
    /// it if clap had hidden arguments.
    #[arg(long, action=ArgAction::SetTrue)]
    bless: Option<bool>,
    /// Do uber stuff
    ///
    /// This is like sudo mode. You probably don't want this.
    #[arg(long, action=ArgAction::SetTrue)]
    uber: Option<bool>,
    /// Stdin
    ///
    /// Read source file from stdin.
    #[arg(long, short, action=ArgAction::SetTrue)]
    stdin: Option<bool>,
    /// Print the AST
    ///
    #[arg(long, action=ArgAction::SetTrue)]
    ast: Option<bool>,
    /// Executor Thread Count
    ///
    /// The number of threads to use for the executor. Defaults to the number of cpus.
    #[arg(long)]
    threads: Option<usize>,
}

#[derive(Clone, Debug, Args)]
#[group(required = false, multiple = true)]
struct DwarfArgs {
    /// Dwarf main arguments
    ///
    /// These arguments are passed on to the dwarf `main` function.
    #[arg(last = true, allow_hyphen_values = true)]
    args: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "async")]
    {
        let format_layer = fmt::layer().with_thread_ids(true).pretty();
        let filter_layer =
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("error"));

        tracing_subscriber::registry()
            .with(filter_layer)
            .with(format_layer)
            .init();
    }
    #[cfg(not(feature = "async"))]
    pretty_env_logger::init();
    color_backtrace::install();
    #[cfg(feature = "tracy")]
    Client::start();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();

    let args = Arguments::parse();
    let bless = args.bless.is_some() && args.bless.unwrap();
    let is_uber = args.uber.is_some() && args.uber.unwrap();
    let print_ast = args.ast.is_some() && args.ast.unwrap();
    let threads = args.threads.unwrap_or_else(num_cpus::get);

    // if threads == 0 {
    //     return Err(Box::new(std::io::Error::new(
    //         std::io::ErrorKind::InvalidInput,
    //         "Thread count must be a positive integer greater than zero.",
    //     )));
    // }

    // Figure out what we're dealing with, input-wise.
    let input = if let Some(ref source) = args.source {
        match source {
            Source::File(source) => {
                log::info!("Compiling source file {}", &source.display());

                let file = source.clone();
                let file = if bless {
                    file.file_stem().unwrap().to_str().unwrap()
                } else {
                    file.to_str().unwrap()
                };

                let source_code = fs::read_to_string(source)?;

                let mut dwarf_args = vec![source.to_string_lossy().to_string()];
                dwarf_args.extend(args.dwarf_args.args);

                Some((source_code, dwarf_args, file.to_owned()))
            }
            Source::Url(url) => {
                let response = reqwest::blocking::get(url.to_owned())?;
                let source_code = response.text()?;

                let mut dwarf_args = vec![url.to_string()];
                dwarf_args.extend(args.dwarf_args.args);

                Some((source_code, dwarf_args, url.to_string()))
            }
        }
    } else if args.stdin.is_some() && args.stdin.unwrap() {
        let mut source_code = String::new();
        io::Read::read_to_string(&mut io::stdin(), &mut source_code)?;

        let mut dwarf_args = vec!["stdin".to_owned()];
        dwarf_args.extend(args.dwarf_args.args);

        Some((source_code, dwarf_args, "stdin".to_owned()))
    } else {
        None
    };

    let dwarf_home = env::var("DWARF_HOME")
        .unwrap_or_else(|_| {
            let mut home = env::var("HOME").unwrap();
            home.push_str("/.dwarf");
            home
        })
        .into();

    if fs::metadata(&dwarf_home).is_err() {
        fs::create_dir_all(&dwarf_home)?;
    }

    if let Some((source_code, dwarf_args, file_name)) = input {
        let ast = match parse_dwarf(&file_name, &source_code) {
            Ok(ast) => ast,
            Err(_) => {
                return Ok(());
            }
        };

        if print_ast {
            println!("{:#?}", ast);
        }

        let ctx = match new_lu_dog(
            file_name.to_owned(),
            Some((source_code.clone(), &ast)),
            &dwarf_home,
            &sarzak,
        ) {
            Ok(lu_dog) => lu_dog,
            Err(errors) => {
                for err in errors {
                    eprintln!(
                        "{}",
                        dwarf::dwarf::error::DwarfErrorReporter(&err, is_uber, &source_code)
                    );
                }
                return Ok(());
            }
        };

        if let Ok(program) = compile(&ctx) {
            println!("running in the VM");

            let args: Vec<RefType<Value>> = dwarf_args
                .into_iter()
                .map(|a| new_ref!(Value, a.into()))
                .collect();

            let mut vm = VM::new(&program, &args);
            vm.invoke("main", &[], false)?;
            return Ok(());
        }

        let mut ctx = initialize_interpreter(threads, dwarf_home, ctx)?;
        ctx.add_args(dwarf_args);

        if args.banner.is_some() && args.banner.unwrap() {
            println!("{}", banner2());
        }

        if args.repl.is_some() && args.repl.unwrap() {
            start_repl(&mut ctx, is_uber)
                .map_err(|e| {
                    println!("Interpreter exited with: {}", e);
                    e
                })
                .unwrap();
        } else {
            match start_func("main", false, &mut ctx) {
                // 🚧 What's a sensible thing to do with this?
                #[allow(unused_variables)]
                Ok(value) => {
                    #[cfg(feature = "async")]
                    {
                        unsafe {
                            let value = std::sync::Arc::into_raw(value);
                            let value = std::ptr::read(value);
                            let value = ref_to_inner!(value);

                            let value = future::block_on(value);

                            let value = std::sync::Arc::into_raw(value);
                            let value = std::ptr::read(value);
                            let value = ref_to_inner!(value);

                            match value {
                                Value::Error(msg) => {
                                    let msg = *msg;
                                    eprintln!("Interpreter exited with:");
                                    eprintln!(
                                        "{}",
                                        ChaChaErrorReporter(
                                            &msg.into(),
                                            is_uber,
                                            &source_code,
                                            &file_name
                                        )
                                    );
                                }
                                _ => println!("{}", value),
                            }
                        }
                    }

                    Ok::<(), ChaChaError>(())
                }
                Err(e) => {
                    eprintln!("Interpreter exited with:");
                    eprintln!(
                        "{}",
                        ChaChaErrorReporter(&e, is_uber, &source_code, &file_name)
                    );
                    Ok(())
                }
            }
            .unwrap();
        }
    } else if args.dap.is_some() && args.dap.unwrap() {
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
        let ctx = Context::default();
        let mut ctx = initialize_interpreter(2, dwarf_home, ctx)?;

        start_repl(&mut ctx, is_uber).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;
    }

    Ok(())
}
