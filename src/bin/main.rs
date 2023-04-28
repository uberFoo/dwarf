use chacha::{initialize_interpreter, merlin, start_repl, Error};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    initialize_interpreter()?;

    start_repl::<merlin::MerlinType>().map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
