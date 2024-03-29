use ansi_term::Colour;

use crate::{
    chacha::error::{ChaChaError, Result},
    interpreter::{debug, eval_expression, function, Context},
    s_read, RefType, SarzakStorePtr, Value,
};

pub fn eval(expr: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let expr = s_read!(lu_dog).exhume_x_return(expr).unwrap();
    debug!("ExpressionEnum::XReturn {expr:?}");

    let expr = &s_read!(expr).expression;
    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();

    let value = eval_expression(expr, context)?;
    let ty = s_read!(value).get_value_type(&s_read!(context.sarzak_heel()), &s_read!(lu_dog));

    // This is a trick to get return to exit out in the evaluation.
    Err(ChaChaError::Return { value, ty })
}
