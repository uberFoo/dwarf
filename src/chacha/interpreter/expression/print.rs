use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{chacha_print, debug, eval_expression, function, Context},
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_print(
    print: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let print = s_read!(lu_dog).exhume_print(print).unwrap();
    debug!("ExpressionEnum::Print print {print:?}");
    let expr = s_read!(print).r32_expression(&s_read!(lu_dog))[0].clone();
    let (value, _) = eval_expression(expr, context, vm)?;
    let result = format!("{}", s_read!(value));
    let result = result.replace("\\n", "\n");

    chacha_print(result, context)?;

    let result = Ok((
        new_ref!(Value, Value::Empty),
        Value::Empty.get_type(&s_read!(lu_dog)),
    ));

    #[allow(clippy::let_and_return)]
    result
}
