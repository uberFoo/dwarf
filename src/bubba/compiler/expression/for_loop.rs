use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub(in crate::bubba::compiler) fn compile(
    for_loop: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let for_loop = lu_dog.exhume_for_loop(for_loop).unwrap();
    let for_loop = s_read!(for_loop);
    let ident = for_loop.ident.to_owned();
    let body = lu_dog.exhume_expression(&for_loop.block).unwrap();
    let list = lu_dog.exhume_expression(&for_loop.expression).unwrap();

    compile_expression(&list, thonk, context)?;

    context.push_symbol_table();
    let mut inner_thonk = CThonk::new(format!("for_{}", ident));

    inner_thonk.increment_frame_size();
    let index = context.insert_symbol(ident.clone());
    compile_expression(&body, &mut inner_thonk, context)?;
    let fp = inner_thonk.get_frame_size();
    for _ in 0..fp {
        thonk.increment_frame_size();
    }

    // Store the starting value
    thonk.add_instruction(Instruction::StoreLocal(index));

    let top_of_loop = thonk.get_instruction_card() as isize;

    thonk.append(inner_thonk);

    // Duplicate the range end so that we can compare against it.
    thonk.add_instruction(Instruction::Dup);

    // Increment the index
    thonk.add_instruction(Instruction::FetchLocal(index));
    thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Integer(1))));
    thonk.add_instruction(Instruction::Add);
    thonk.add_instruction(Instruction::Dup);
    thonk.add_instruction(Instruction::StoreLocal(index));

    // Test the index against the length of the list
    thonk.add_instruction(Instruction::TestLessThanOrEqual);

    // go do it again if index is < end.
    thonk.add_instruction(Instruction::JumpIfFalse(
        top_of_loop - thonk.get_instruction_card() as isize - 1,
    ));

    context.pop_symbol_table();

    Ok(())
}
