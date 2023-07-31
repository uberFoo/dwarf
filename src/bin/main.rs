use std::{
    fs,
    io::{self, BufReader, BufWriter},
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
    plug_in::PluginId,
    sarzak::{ObjectStore as SarzakStore, MODEL as SARZAK_MODEL},
};
use reqwest::Url;
use rustc_hash::FxHashMap as HashMap;
use sarzak::domain::DomainBuilder;

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
    /// Model Files
    ///
    /// A comma-delimited list of model files to load into the dwarf program.
    #[arg(long, short, use_value_delimiter = true, value_delimiter = ',')]
    models: Option<Vec<PathBuf>>,
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
    color_backtrace::install();

    let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
    let lu_dog = LuDogStore::new();

    let args = Arguments::parse();
    let bless = args.bless.is_some() && args.bless.unwrap();
    let is_uber = args.uber.is_some() && args.uber.unwrap();

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
        // Maybe reading from stdin?
    } else if args.stdin.is_some() && args.stdin.unwrap() {
        let mut source_code = String::new();
        io::Read::read_to_string(&mut io::stdin(), &mut source_code)?;

        let mut dwarf_args = vec!["stdin".to_owned()];
        dwarf_args.extend(args.dwarf_args.args);

        Some((source_code, dwarf_args, "stdin".to_owned()))
    } else {
        None
    };

    let models = if let Some(ref models) = args.models {
        let mut map = HashMap::default();
        for model in models {
            let domain = DomainBuilder::new()
                .cuckoo_model(model)
                .unwrap()
                .build_v2()
                .unwrap();
            map.insert(domain.name().to_owned(), (domain.sarzak().clone(), None));
        }

        map
    } else {
        HashMap::default()
    };

    if let Some((source_code, dwarf_args, name)) = input {
        let ast = match parse_dwarf(&name, &source_code) {
            Ok(ast) => ast,
            Err(_) => {
                return Ok(());
            }
        };

        let lu_dog = match new_lu_dog(None, Some((source_code.clone(), &ast)), &models, &sarzak) {
            Ok(lu_dog) => lu_dog,
            Err(errors) => {
                for err in errors {
                    eprintln!(
                        "{}",
                        dwarf::dwarf::error::DwarfErrorReporter(&err, is_uber, &source_code, &name)
                    );
                }
                return Ok(());
            }
        };

        let mut ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, models, None)?;
        ctx.add_args(dwarf_args);

        if args.banner.is_some() && args.banner.unwrap() {
            println!("{}", banner2());
        }

        if args.repl.is_some() && args.repl.unwrap() {
            start_repl(ctx).map_err(|e| {
                println!("Interpreter exited with: {}", e);
                e
            })?;
        } else {
            match start_main(false, ctx) {
                Ok((_, ctx)) => ctx,
                Err(e) => {
                    eprintln!("Interpreter exited with:");
                    eprintln!(
                        "{}",
                        dwarf::ChaChaErrorReporter(&e, is_uber, &source_code, &name)
                    );
                    return Ok(());
                }
            };
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
        let ctx = initialize_interpreter::<PathBuf>(sarzak, lu_dog, models, None)?;

        start_repl(ctx).map_err(|e| {
            println!("Interpreter exited with: {}", e);
            e
        })?;
    }

    Ok(())
}

fn test_load_lib() -> Result<(), Box<dyn std::error::Error>> {
    use std::path::Path;

    // use abi_stable::std_types::RVec;
    use abi_stable::{
        external_types::crossbeam_channel::{self, RReceiver, RSender},
        library::{lib_header_from_path, LibraryError, LibrarySuffix, RawLibrary},
        sabi_trait::prelude::TD_Opaque,
        std_types::{RErr, ROk, RResult, RSome, RStr, RString, RVec},
    };
    use dwarf::plug_in::{
        load_root_module_in_directory, AppenderBox, Appender_TO, BoxedInterface, ExampleLib_Ref,
        PluginMod_Ref,
    };
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize)]
    pub enum Command {
        IterObject,
    }

    // let loaded_libraries = Vec::new();

    let library_path = RawLibrary::path_in_directory(
        &Path::new("./plug-ins/example/target/debug"),
        "example_library",
        LibrarySuffix::NoSuffix,
    );
    let root_module = (|| {
        let header = lib_header_from_path(&library_path)?;
        header.init_root_module::<PluginMod_Ref>()
    })()?;
    let name_key = "TextMunging".to_string();

    let ctor = root_module.load();
    dbg!(&ctor);
    let new_id = PluginId {
        named: name_key.clone().into(),
        instance: 0,
    };
    let mut foo = ctor(new_id, "../sarzak/models/sarzak.v2.json".into()).unwrap();

    let command = Command::IterObject;
    let command = serde_json::to_string(&command).unwrap();
    dbg!(&command);

    let response = foo.json_command(command.as_str().into()).into_result()?;

    dbg!(response);

    // foo.invoke_func(
    //     "foo_bar_baz".into(),
    //     vec!["42".into(), "3.14".into(), "hello world".into()].into(),
    // );

    // loaded_libraries.push(name_key.clone());

    // state.id_map.insert(
    //     name_key.into_::<RString>(),
    //     PluginModAndIndices {
    //         root_module,
    //         to_be_instantiated: instances,
    //         indices: Vec::with_capacity(instances as usize),
    //     },
    // );

    // for name in loaded_libraries {
    //     let mod_i = state.id_map.get_mut(&*name).unwrap();
    //     for _ in 0..mem::replace(&mut mod_i.to_be_instantiated, 0) {
    //         let plugin_constructor = mod_i.root_module.new();
    //         let new_id = PluginId {
    //             named: name.clone().into(),
    //             instance: mod_i.indices.len() as u64,
    //         };
    //         let plugin = match plugin_constructor(state.sender.clone(), new_id.clone()) {
    //             ROk(x) => x,
    //             RErr(e) => {
    //                 plugin_new_errs.push((name.clone(), e));
    //                 continue;
    //             }
    //         };

    //         let new_index = plugins.len();

    //         plugins.push(plugin);

    //         mod_i.indices.push(new_index);
    //         state.plugin_ids.push(new_id);
    //     }
    // }

    // {
    //     /*/////////////////////////////////////////////////////////////////////////////////

    //     This block demonstrates `#[sabi_trait]` generated trait objects

    //     */////////////////////////////////////////////////////////////////////////////////

    //     // The type annotation is for the reader
    //     let mut appender: AppenderBox<u32> = library.new_appender()();
    //     appender.push(100);
    //     appender.push(200);

    //     // The primary way to use the methods in the trait is through the inherent methods on
    //     // the ffi-safe trait object.
    //     Appender_TO::push(&mut appender, 300);
    //     appender.append(vec![500, 600].into());
    //     assert_eq!(
    //         appender.into_rvec(),
    //         RVec::from(vec![100, 200, 300, 500, 600])
    //     );
    // }
    // {
    //     /*/////////////////////////////////////////////////////////////////////////////////

    //     This block demonstrates the `DynTrait<>` trait object.

    //     `DynTrait` is used here as a safe opaque type which can only be unwrapped back to the
    //     original type in the dynamic library that constructed the `DynTrait` itself.

    //     */////////////////////////////////////////////////////////////////////////////////

    //     // The type annotation is for the reader
    //     let mut unwrapped: BoxedInterface = library.new_boxed_interface()();

    //     library.append_string()(&mut unwrapped, "Hello".into());
    //     library.append_string()(&mut unwrapped, ", world!".into());

    //     assert_eq!(&*unwrapped.to_string(), "Hello, world!");
    // }

    println!("success");

    Ok(())
}
