use chacha::{
    initialize_interpreter,
    merlin::{InflectionProxy, PointProxy},
    start_repl, Error,
};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let mut ctx = initialize_interpreter(
        "../sarzak/models/sarzak.v2.json",
        "../sarzak/target/sarzak/merlin",
    )?;

    ctx.register_model("../sarzak/models/merlin.v2.json")?;

    ctx.register_store_proxy("INFLECTION".to_owned(), InflectionProxy::default());
    ctx.register_store_proxy("POINT".to_owned(), PointProxy::default());

    start_repl(ctx).map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
