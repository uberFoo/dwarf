use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{update_span_value, ExprSpan},
        DwarfInteger,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, CharLiteral, Expression, Literal, Span, ValueType,
        XValue,
    },
    RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    literal: DwarfInteger,
    span: RefType<Span>,
    block: &RefType<Block>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let literal = CharLiteral::new(literal, lu_dog);
    let expr = Expression::new_literal(
        true,
        &Literal::new_char_literal(true, &literal, lu_dog),
        lu_dog,
    );
    let ty = ValueType::new_char(true, lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
