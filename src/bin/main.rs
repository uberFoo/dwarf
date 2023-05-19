use chacha::{
    initialize_interpreter,
    interpreter::{initialize_interpreter_paths, start_main, start_vm},
    // merlin::{ErrorExpressionProxy, ExpressionProxy},
    // merlin::{
    //     AnchorProxy, BisectionProxy, EdgeProxy, GlyphProxy, LineProxy, LineSegmentPointProxy,
    //     LineSegmentProxy, PointProxy, RelationshipNameProxy, RelationshipPhraseProxy, XBoxProxy,
    // },
    start_repl,
    Error,
};

fn main() -> Result<(), Error> {
    pretty_env_logger::init();

    // let mut ctx = initialize_interpreter_paths("../sarzak/target/sarzak/lu_dog/lu_dog.道")?;
    let mut ctx = initialize_interpreter_paths("fib.道")?;

    // ctx.register_model("../sarzak/models/lu_dog.v2.json")?;

    // ctx.register_store_proxy(
    //     "ExpressionProxy".to_owned(),
    //     ExpressionProxy::new_type(ctx.lu_dog_heel()),
    // );

    // ctx.register_store_proxy(
    //     "ErrorExpressionProxy".to_owned(),
    //     ErrorExpressionProxy::new_type(ctx.lu_dog_heel()),
    // );

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
