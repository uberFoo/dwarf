use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ExpressionEnum,
    s_read, SarzakStorePtr,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();

    compile_expression(&scrutinee, thonk, context)?;

    for pattern in patterns {
        let pattern = s_read!(pattern);
        let match_expr = pattern.r87_expression(&lu_dog)[0].clone();
        let expr = pattern.r92_expression(&lu_dog)[0].clone();

        thonk.add_instruction(Instruction::Dup);

        // Do we need to create a new symbol table for each match arm?
        if let ExpressionEnum::VariableExpression(ref id) = s_read!(match_expr).subtype {
            let var = lu_dog.exhume_variable_expression(id).unwrap();
            let idx = context.insert_symbol(s_read!(var).name.clone());
            thonk.increment_frame_size();

            thonk.add_instruction(Instruction::Dup);
            thonk.add_instruction(Instruction::StoreLocal(idx));
        }

        compile_expression(&match_expr, thonk, context)?;
        thonk.add_instruction(Instruction::TestEq);

        // Compile the block if we match.
        context.push_symbol_table();
        let mut match_thonk = CThonk::new("match".to_owned());

        compile_expression(&expr, &mut match_thonk, context)?;
        let fp = match_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        let match_len = match_thonk.get_instruction_card() as isize;
        context.pop_symbol_table();

        // Jump over the matching block if we don't match.
        thonk.add_instruction(Instruction::JumpIfFalse(match_len + 1));

        // Insert the compiled matching block
        thonk.append(match_thonk);

        // Return if we matched.
        thonk.add_instruction(Instruction::Return);
    }

    thonk.add_instruction(Instruction::HaltAndCatchFire);

    Ok(())
}
