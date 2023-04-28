use chacha::{do_repl, init, merlin, Error};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    init()?;

    do_repl::<merlin::MerlinType>().map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
