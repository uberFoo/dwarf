use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    dwarf::{
        error::Result,
        extruder::{
            debug, function, inter_expression, make_value_type, update_span_value, Context,
            ExprSpan,
        },
        Expression as ParserExpression, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, Span, TypeCast, ValueType, XValue,
    },
    new_ref, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
#[allow(clippy::borrowed_box)]
pub fn inter(
    expr: &Box<(ParserExpression, Range<usize>)>,
    ty: &(Type, Range<usize>),
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (expr, expr_ty) = inter_expression(
        &new_ref!(ParserExpression, expr.0.to_owned()),
        &expr.1,
        block,
        context,
        context_stack,
        lu_dog,
    )?;
    debug!("As lhs: {expr:?}: {expr_ty:?}");

    context.location = location!();
    let as_type = make_value_type(&ty.0, &ty.1, None, context, context_stack, lu_dog)?;

    let as_op = TypeCast::new(&expr.0, &as_type, lu_dog);
    let expr = Expression::new_type_cast(&as_op, lu_dog);
    let value = XValue::new_expression(block, &as_type, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), as_type))
}
