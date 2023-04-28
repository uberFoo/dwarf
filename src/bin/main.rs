use chacha::{do_repl, init, Error};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    init()?;

    do_repl().map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
