use crate::{
    bubba::compiler::{compile_expression, CThonk, Context, Result, RANGE},
    lu_dog::ValueType,
    s_read, SarzakStorePtr, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    range: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_range"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let range_ty = context.get_type(RANGE).unwrap().clone();

    let range = lu_dog.exhume_range_expression(range).unwrap();
    let range = s_read!(range);

    let start = lu_dog.exhume_expression(&range.lhs.unwrap()).unwrap();
    let end = lu_dog.exhume_expression(&range.rhs.unwrap()).unwrap();

    // We push first the end, and then the start, onto the stack.
    compile_expression(&end, thonk, context)?;
    compile_expression(&start, thonk, context)?;

    Ok(Some(range_ty))
}
