use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    s_read, SarzakStorePtr, Span, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}:\n  --> {}:{}:{}", POP_CLR.paint("compile_await"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_a_wait(expr).unwrap();
    let expr = s_read!(expr).r98_expression(&lu_dog)[0].clone();
    let result = compile_expression(&expr, thonk, context, span.clone());

    thonk.insert_instruction_with_span(Instruction::Await, span, location!());

    result
}
