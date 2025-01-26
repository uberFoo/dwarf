use std::ops::Range;

use ansi_term::Colour;
use snafu::location;
use uuid::Uuid;

use crate::{
    dwarf::{
        error::Result,
        extruder::{debug, function, inter_statements, update_span_value, Context, ExprSpan},
        BlockType, Statement, WrappedValueType,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, LocalVariable, Span, ValueType,
        Variable, XFuture, XValue,
    },
    new_ref, s_read, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    a_sink: BlockType,
    stmts: &[(Statement, Range<usize>)],
    vars: Vec<String>,
    tys: Vec<WrappedValueType>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let sync = match a_sink {
        BlockType::Async => false,
        BlockType::Sync => true,
    };
    let block = Block::new(!sync, Uuid::new_v4(), Some(block), None, lu_dog);

    for (var, ty) in vars.into_iter().zip(tys.into_iter()) {
        let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
        let var = Variable::new_local_variable(var, &local, lu_dog);
        debug!("variable {var:?}");
        // 🚧 We should really be passing a span in the Block so that
        // we can link this XValue to it.
        let _value = XValue::new_variable(&block, &ty.0, &var, lu_dog);
    }

    // let block = create_block::<P>(None, lu_dog)?;
    debug!("block {block:?}");
    let stmts_vec: Vec<RefType<Statement>> = stmts
        .iter()
        .map(|stmt| new_ref!(Statement, stmt.0.to_owned()))
        .collect();
    // 🚧 The one that's commented out is correct -- assuming the block
    // isn't `{}`. The one that isn't commented out _should_ be right,
    // but I'm not sure that it is.
    // let stmts_span = stmts.iter().map(|stmt| stmt.1.start).min().unwrap()
    //     ..stmts.iter().map(|stmt| stmt.1.end).max().unwrap();
    let stmts_span = s_read!(span).start as usize..s_read!(span).end as usize;

    let expr = Expression::new_block(true, &block, lu_dog);
    let ty = inter_statements(
        &stmts_vec,
        &stmts_span,
        &block,
        context,
        import_stack,
        lu_dog,
    )?;
    let value = XValue::new_expression(&block, &ty.0, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    // If it's an async block then wrap it in a future.
    let ty = match a_sink {
        BlockType::Async => {
            let span = ty.1;
            let future = XFuture::new(&ty.0, lu_dog);
            (ValueType::new_x_future(true, &future, lu_dog), span)
        }
        BlockType::Sync => ty,
    };

    debug!("block {expr:?}");
    Ok(((expr, span), ty.0))
}
