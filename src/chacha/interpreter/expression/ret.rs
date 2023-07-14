use ansi_term::Colour;

use crate::{
    chacha::{
        error::{ChaChaError, Result},
        vm::VM,
    },
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::ValueType,
    s_read, RefType, SarzakStorePtr, Value,
};

pub fn eval_return_expression(
    expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let expr = s_read!(lu_dog).exhume_x_return(expr).unwrap();
    debug!("ExpressionEnum::XReturn {expr:?}");

    let expr = &s_read!(expr).expression;
    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();

    let (value, ty) = eval_expression(expr, context, vm)?;
    Err(ChaChaError::Return { value, ty })
}
