use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    print: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let print = lu_dog.exhume_x_print(print).unwrap();
    let expr = s_read!(print).r32_expression(&lu_dog)[0].clone();
    compile_expression(&expr, thonk, context, get_span(&expr, &lu_dog))?;
    thonk.add_instruction(Instruction::Out(0), location!());

    Ok(())
}
