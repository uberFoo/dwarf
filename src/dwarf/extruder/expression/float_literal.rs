use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{update_span_value, Context, ExprSpan},
        DwarfFloat,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, FloatLiteral, Literal, Span,
        ValueType, XValue,
    },
    sarzak::Ty,
    RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    literal: DwarfFloat,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let expr = Expression::new_literal(
        true,
        &Literal::new_float_literal(true, &FloatLiteral::new(literal, lu_dog), lu_dog),
        lu_dog,
    );
    let ty = ValueType::new_ty(true, &Ty::new_float(context.sarzak), lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
