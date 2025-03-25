use std::ops::Range;

use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, typecheck, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Binary, Block, Expression, Operator, Span, ValueType,
        XValue,
    },
    new_ref, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    lhs_p: Box<(ParserExpression, Range<usize>)>,
    rhs_p: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (lhs, lhs_ty) = inter_expression(
        &new_ref!(ParserExpression, lhs_p.0.to_owned()),
        &lhs_p.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    let (rhs, rhs_ty) = inter_expression(
        &new_ref!(ParserExpression, rhs_p.0.to_owned()),
        &rhs_p.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;

    typecheck(
        (&lhs_ty, &lhs_p.1),
        (&rhs_ty, &rhs_p.1),
        location!(),
        context,
        lu_dog,
    )?;

    let expr = Binary::new_assignment(true, lu_dog);
    let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
    let expr = Expression::new_operator(true, &expr, lu_dog);

    let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), lhs_ty))
}
