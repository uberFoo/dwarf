use crate::{
    chacha::error::Result, interpreter::Context, lu_dog::ValueType, new_ref, s_read, NewRef,
    RefType, SarzakStorePtr, Value,
};

/// Eval an error expression
///
/// I think that this just needs to go away -- I don't think it's used.
pub fn eval_error(
    error: &SarzakStorePtr,
    context: &mut Context,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let error = s_read!(lu_dog).exhume_error_expression(error).unwrap();

    // 🚧 This isn't going to cut it.
    print!("\t{}", s_read!(error).span);

    let result = Ok((
        new_ref!(Value, Value::Empty),
        Value::Empty.get_type(&s_read!(lu_dog)),
    ));

    result
}