use std::ops::Range;

use snafu::{location, Location};

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{inter_expression, typecheck, update_span_value, Context, ExprSpan},
        Expression as ParserExpression, PrintableValueType,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Binary, Block, BooleanOperator, Expression, Operator,
        Span, ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType,
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

    if let ValueTypeEnum::Ty(ref id) = s_read!(lhs_ty).subtype {
        let ty = context.sarzak.exhume_ty(id).unwrap();
        matches!(&*ty.read().unwrap(), Ty::Boolean(_));
    } else {
        let lhs = PrintableValueType(&lhs_ty, context, lu_dog);
        return Err(vec![DwarfError::TypeMismatch {
            found: lhs.to_string(),
            expected: "bool".to_string(),
            found_span: lhs_p.1.to_owned(),
            expected_span: rhs_p.1.to_owned(),
            location: location!(),
        }]);
    }

    typecheck(
        (&lhs_ty, &lhs_p.1),
        (&rhs_ty, &rhs_p.1),
        location!(),
        context,
        lu_dog,
    )?;

    let expr = BooleanOperator::new_and(lu_dog);
    let expr = Binary::new_boolean_operator(&expr, lu_dog);
    let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
    let expr = Expression::new_operator(&expr, lu_dog);

    let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), lhs_ty))
}
