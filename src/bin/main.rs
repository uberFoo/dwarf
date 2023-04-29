use chacha::{
    initialize_interpreter,
    merlin::{InflectionProxy, PointProxy},
    start_repl, Error,
};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let mut ctx = initialize_interpreter()?;

    ctx.register_store_proxy("INFLECTION".to_owned(), InflectionProxy::default());
    ctx.register_store_proxy("POINT".to_owned(), PointProxy::default());

    start_repl(ctx).map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
