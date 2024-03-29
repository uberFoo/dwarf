use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{List, Range, ValueType},
    new_ref, s_read, s_write, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_range(
    range: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog_read = s_read!(lu_dog);

    let range = lu_dog_read.exhume_range_expression(range).unwrap();
    let range = s_read!(range);
    let lhs = range.lhs.unwrap();
    let lhs = lu_dog_read.exhume_expression(&lhs).unwrap();
    let rhs = range.rhs.unwrap();
    let rhs = lu_dog_read.exhume_expression(&rhs).unwrap();

    let (lhs, _) = eval_expression(lhs, context, vm)?;
    let (rhs, _) = eval_expression(rhs, context, vm)?;

    let range = Range {
        start: Box::new(lhs),
        end: Box::new(rhs),
    };

    Ok((
        new_ref!(Value, Value::Range(range)),
        ValueType::new_range(&mut s_write!(lu_dog)),
    ))
}
