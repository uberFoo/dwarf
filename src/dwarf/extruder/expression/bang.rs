use std::ops::Range;

use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, Operator, Span, Unary, ValueType,
        XValue,
    },
    new_ref, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    expr: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (expr, ty) = inter_expression(
        &new_ref!(ParserExpression, expr.0.to_owned()),
        &expr.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    let not = Unary::new_not(true, lu_dog);
    let operator = Operator::new_unary(&expr.0, None, &not, lu_dog);
    let expr = Expression::new_operator(true, &operator, lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
