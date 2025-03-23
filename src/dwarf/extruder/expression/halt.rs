use std::ops::Range;

use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, HaltAndCatchFire, Span, ValueType,
        XValue,
    },
    new_ref, NewRef, RefType,
};
pub fn inter(
    expr: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (expr, _ty) = inter_expression(
        &new_ref!(ParserExpression, expr.0.to_owned()),
        &expr.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    let ty = ValueType::new_empty(true, lu_dog);
    let halt = HaltAndCatchFire::new(&expr.0, lu_dog);
    let expr = Expression::new_halt_and_catch_fire(true, &halt, lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
