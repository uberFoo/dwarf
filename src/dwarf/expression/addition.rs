use std::ops::Range;

use log::debug;
use snafu::{location, Location};

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
    new_ref, s_read, s_write, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
#[allow(clippy::borrowed_box)]
pub fn inter(
    lhs_p: &Box<(ParserExpression, Range<usize>)>,
    rhs_p: &Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("Addition");
    let (lhs, lhs_ty) = inter_expression(
        &new_ref!(ParserExpression, lhs_p.0.to_owned()),
        &lhs_p.1,
        block,
        context,
        lu_dog,
    )?;
    let (rhs, rhs_ty) = inter_expression(
        &new_ref!(ParserExpression, rhs_p.0.to_owned()),
        &rhs_p.1,
        block,
        context,
        lu_dog,
    )?;

    // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
    // 🚧                        THIS IS SUPER IMPORTANT!
    // 🚧
    // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same.
    // 🚧 We also need to check that the type supports addition.
    // 🚧
    // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

    typecheck(
        (&lhs_ty, &lhs_p.1),
        (&rhs_ty, &rhs_p.1),
        location!(),
        context,
        lu_dog,
    )?;

    let expr = Binary::new_addition(lu_dog);
    let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
    let expr = Expression::new_operator(&expr, lu_dog);

    let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), lhs_ty))
}
