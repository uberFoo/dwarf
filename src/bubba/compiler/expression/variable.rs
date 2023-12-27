use crate::{
    bubba::{
        compiler::{CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_variable_expression(expr).unwrap();
    let expr = s_read!(expr);
    let name = expr.name.clone();

    if let Some(index) = context.get_symbol(&name) {
        thonk.add_instruction(Instruction::FetchLocal(index));
    } else {
        // We are here because we need to look up a function.
        thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::new_thonk(name))));
    }

    Ok(())
}
