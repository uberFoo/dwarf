use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    s_read, SarzakStorePtr, Span,
};

pub(in crate::bubba::compiler) fn compile(
    index: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let index = lu_dog.exhume_index(index).unwrap();
    let index = s_read!(index);
    let target = lu_dog.exhume_expression(&index.target).unwrap();
    let target_span = get_span(&target, &lu_dog);
    compile_expression(&target, thonk, context, target_span)?;

    let index_expr = lu_dog.exhume_expression(&index.index).unwrap();
    let index_expr_span = get_span(&index_expr, &lu_dog);
    compile_expression(&index_expr, thonk, context, index_expr_span)?;

    thonk.add_instruction_with_span(Instruction::Index, span);

    Ok(())
}
