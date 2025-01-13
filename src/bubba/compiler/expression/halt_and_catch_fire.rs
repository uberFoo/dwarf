use snafu::location;

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    s_read, SarzakStorePtr, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_halt_and_catch_fire"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let halt = lu_dog.exhume_halt_and_catch_fire(expr).unwrap();
    let expr = s_read!(halt).r114_expression(&lu_dog)[0].clone();
    compile_expression(&expr, thonk, context)?;
    thonk.insert_instruction(Instruction::Out(0), location!());
    thonk.insert_instruction(Instruction::HaltAndCatchFire, location!());

    Ok(None)
}
