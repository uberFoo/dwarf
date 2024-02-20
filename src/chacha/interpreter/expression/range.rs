use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    new_ref, s_read, DwarfInteger, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_range(range: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let range = s_read!(lu_dog).exhume_range_expression(range).unwrap();
    let lhs = s_read!(range).lhs.unwrap();
    let lhs = s_read!(lu_dog).exhume_expression(&lhs).unwrap();
    let rhs = s_read!(range).rhs.unwrap();
    let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();

    debug!("range lhs: {lhs:?}, range: {range:?}");

    let lhs = eval_expression(lhs, context)?;
    let rhs = eval_expression(rhs, context)?;

    let range = std::ops::Range {
        start: <Value as TryInto<DwarfInteger>>::try_into(s_read!(lhs).clone())?,
        end: s_read!(rhs).clone().try_into()?,
    };

    let result = Ok(new_ref!(Value, Value::Range(range)));

    #[allow(clippy::let_and_return)]
    result
}
