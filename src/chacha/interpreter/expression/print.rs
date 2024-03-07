use ansi_term::Colour;

use crate::{
    chacha::error::Result,
    interpreter::{chacha_print, debug, eval_expression, function, Context},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(print: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let print = s_read!(lu_dog).exhume_x_print(print).unwrap();
    debug!("ExpressionEnum::Print print {print:?}");
    let expr = s_read!(print).r32_expression(&s_read!(lu_dog))[0].clone();
    let value = eval_expression(expr, context)?;
    let result = s_read!(value).to_inner_string();
    let result = result.replace("\\n", "\n");

    chacha_print(result, context)?;

    let result = Ok(new_ref!(Value, Value::Empty));

    #[allow(clippy::let_and_return)]
    result
}
