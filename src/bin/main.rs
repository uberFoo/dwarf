use chacha::{
    initialize_interpreter,
    interpreter::{start_main, start_vm},
    merlin::{
        AnchorProxy, BisectionProxy, EdgeProxy, GlyphProxy, LineProxy, LineSegmentPointProxy,
        LineSegmentProxy, PointProxy, RelationshipNameProxy, RelationshipPhraseProxy, XBoxProxy,
    },
    start_repl, Error,
};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    let mut ctx = initialize_interpreter("../sarzak/models/sarzak.v2.json", "fib.道")?;

    // ctx.register_model("../sarzak/models/merlin.v2.json")?;

    // ctx.register_store_proxy(
    //     "ANCHOR".to_owned(),
    //     AnchorProxy::new_type(ctx.lu_dog_heel()),
    // );
    // ctx.register_store_proxy(
    //     "BISECTION".to_owned(),
    //     BisectionProxy::new_type(ctx.lu_dog_heel()),
    // );
    // ctx.register_store_proxy("EDGE".to_owned(), EdgeProxy::new_type(ctx.lu_dog_heel()));
    // ctx.register_store_proxy("LINE".to_owned(), LineProxy::new_type(ctx.lu_dog_heel()));
    // ctx.register_store_proxy("POINT".to_owned(), PointProxy::new_type(ctx.lu_dog_heel()));

    // let result = start_vm(20).map_err(|e| {
    //     println!("Interpreter exited with: {}", e);
    //     e
    // });
    // println!("Interpreter exited with: {:?}", result);
    // Ok(())

    start_repl(ctx).map_err(|e| {
        println!("Interpreter exited with: {}", e);
        e
    })

    // start_main(ctx).map_err(|e| {
    //     println!("Interpreter exited with: {}", e);
    //     e
    // })
}
