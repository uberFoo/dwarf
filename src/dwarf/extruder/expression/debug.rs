use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{update_span_value, ExprSpan},
    },
    lu_dog::{store::ObjectStore as LuDogStore, Block, Expression, Span, ValueType, XValue},
    RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    span: RefType<Span>,
    block: &RefType<Block>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let expr = Expression::new_x_debugger(true, lu_dog);
    let ty = ValueType::new_empty(true, lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
