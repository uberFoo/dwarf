use std::{
    fs,
    io::{BufReader, BufWriter},
    net::TcpListener,
    path::PathBuf,
    thread,
};

use clap::{ArgAction, Args, Parser};
use dap::{prelude::BasicClient, server::Server};
use dwarf::{
    chacha::dap::DapAdapter,
    dwarf::{new_lu_dog, parse_dwarf},
    initialize_interpreter,
    interpreter::{banner2, start_main, start_repl},
    lu_dog::ObjectStore as LuDogStore,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};

#[cfg(not(feature = "repl"))]
compile_error!("The REPL requires the \"repl\" feature flag..");

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
    dwarf_args: DwarfArgs,
    /// Bless a test
    ///
    /// This is only useful if you are writing tests for dwarf. I'd really like
    /// it if clap had hidden arguments.
    /// 🚧 Hack on clap
    #[arg(long, action=ArgAction::SetTrue)]
    bless: Option<bool>,
    /// Do uber stuff
    ///
    /// This is like sudo mode. You probably don't want this.
    #[arg(long, action=ArgAction::SetTrue)]
    uber: Option<bool>,
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
    pretty_env_logger::init();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = LuDogStore::new();

    let args = Arguments::parse();
    let bless = args.bless.is_some() && args.bless.unwrap();
    let is_uber = args.uber.is_some() && args.uber.unwrap();

    if let Some(ref source) = args.source {
        log::info!("Compiling source file {}", &source.display());

        let file = args.source.clone().unwrap();
        let file = if bless {
            file.file_stem().unwrap().to_str().unwrap()
        } else {
            file.to_str().unwrap()
        };

        let mut dwarf_args = vec![source.to_string_lossy().to_string()];
        dwarf_args.extend(args.dwarf_args.args);

        let source_code = fs::read_to_string(source)?;

        let ast = match parse_dwarf(file, &source_code) {
            Ok(ast) => ast,
            Err(_) => {
                // eprintln!("{}", dwarf::dwarf::DwarfErrorReporter(&e, &source_code));
                return Ok(());
            }
        };

        let lu_dog = match new_lu_dog(None, Some((source_code.clone(), &ast)), &[], &sarzak) {
            Ok(lu_dog) => lu_dog,
            Err(errors) => {
                for err in errors {
                    eprintln!(
                        "{}",
                        dwarf::dwarf::DwarfErrorReporter(&err, is_uber, &source_code, file)
                    );
                }
                return Ok(());
            }
        };

        dbg!(sarzak.iter_object().collect::<Vec<_>>());

        let mut ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None)?;
        ctx.add_args(dwarf_args);

        if args.banner.is_some() && args.banner.unwrap() {
            println!("{}", banner2());
        }

        let ctx = match start_main(false, ctx) {
            Ok((_, ctx)) => ctx,
            Err(e) => {
                eprintln!("Interpreter exited with:");
                eprintln!(
                    "{}",
                    dwarf::ChaChaErrorReporter(&e, is_uber, &source_code, file)
                );
                return Ok(());
            }
        };

        if args.repl.is_some() && args.repl.unwrap() {
            start_repl(ctx).map_err(|e| {
                println!("Interpreter exited with: {}", e);
                e
            })?;
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
        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, None)?;

        start_repl(ctx).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;
    }

    Ok(())
}
