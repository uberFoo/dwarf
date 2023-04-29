use std::sync::{Arc, RwLock};

use chacha::{
    initialize_interpreter,
    merlin::{InflectionProxy, PointProxy},
    start_repl, Error, Value,
};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let mut ctx = initialize_interpreter()?;

    ctx.insert_store_proxy(
        "INFLECTION".to_owned(),
        Value::ProxyType(Arc::new(RwLock::new(InflectionProxy::default()))),
    );
    ctx.insert_store_proxy(
        "POINT".to_owned(),
        Value::ProxyType(Arc::new(RwLock::new(PointProxy::default()))),
    );

    start_repl(ctx).map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })
}
