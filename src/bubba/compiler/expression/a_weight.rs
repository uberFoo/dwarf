use crate::{
    bubba::compiler::{compile_expression, CThonk, Context, Result},
    lu_dog::ValueType,
    s_read, SarzakStorePtr, Span, POP_CLR,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}:\n  --> {}:{}:{}",POP_CLR.paint("await"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_a_wait(expr).unwrap();
    let expr = s_read!(expr).r98_expression(&lu_dog)[0].clone();
    compile_expression(&expr, thonk, context, span)
}
