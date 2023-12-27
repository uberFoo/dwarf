use crate::{
    bubba::compiler::{compile_expression, get_span, CThonk, Context, Result},
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    range: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let range = lu_dog.exhume_range_expression(range).unwrap();
    let range = s_read!(range);

    let start = lu_dog.exhume_expression(&range.lhs.unwrap()).unwrap();
    let start_span = get_span(&start, &lu_dog);
    let end = lu_dog.exhume_expression(&range.rhs.unwrap()).unwrap();
    let end_span = get_span(&end, &lu_dog);

    // We push first the end, then the start onto the stack.
    compile_expression(&end, thonk, context, end_span)?;
    compile_expression(&start, thonk, context, start_span)?;

    Ok(())
}
