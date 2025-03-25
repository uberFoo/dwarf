use std::ops::Range;

use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, typecheck, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Comparison, Expression, Operator, Span, ValueType,
        XValue,
    },
    new_ref,
    sarzak::Ty,
    NewRef, RefType,
};

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

    // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
    // 🚧                        THIS IS SUPER IMPORTANT!
    // 🚧
    // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
    // 🚧 or at least compatible. Need to look into rust rules.
    // 🚧 We also need to check that the types implement PartialEq, and whatever else...
    // 🚧
    // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

    typecheck(
        (&lhs_ty, &lhs_p.1),
        (&rhs_ty, &rhs_p.1),
        location!(),
        context,
        lu_dog,
    )?;

    let expr = Comparison::new_greater_than_or_equal(true, lu_dog);
    let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
    let expr = Expression::new_operator(true, &expr, lu_dog);

    let ty = Ty::new_boolean(context.sarzak);
    let ty = ValueType::new_ty(true, &ty, lu_dog);

    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
