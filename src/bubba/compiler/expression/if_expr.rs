use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    log::debug!(target: "instr", "{}:{}:{}", file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_x_if(expr).unwrap();
    let expr = s_read!(expr);

    let cond_expr = lu_dog.exhume_expression(&expr.test).unwrap();
    let cond_expr_span = get_span(&cond_expr, &lu_dog);
    compile_expression(&cond_expr, thonk, context, cond_expr_span)?;

    thonk.add_instruction(
        Instruction::Push(new_ref!(Value, Value::Boolean(true))),
        location!(),
    );
    thonk.add_instruction(Instruction::TestEq, location!());

    // Compile the false block
    let false_thonk = if let Some(ref expr) = expr.false_block {
        context.push_symbol_table();
        let mut false_thonk = CThonk::new("if_false".to_owned());
        let block = lu_dog.exhume_expression(expr).unwrap();
        let block_span = get_span(&block, &lu_dog);
        compile_expression(&block, &mut false_thonk, context, block_span)?;
        let fp = false_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        context.pop_symbol_table();
        Some(false_thonk)
    } else {
        None
    };

    // Compile the true block.
    context.push_symbol_table();
    let mut true_thonk = CThonk::new("if_true".to_owned());
    let block = lu_dog.exhume_block(&expr.true_block).unwrap();
    let block = s_read!(block).r15_expression(&lu_dog)[0].clone();
    let block_span = get_span(&block, &lu_dog);
    compile_expression(&block, &mut true_thonk, context, block_span)?;

    let fp = true_thonk.get_frame_size();
    for _ in 0..fp {
        thonk.increment_frame_size();
    }
    let true_block_len = true_thonk.get_instruction_card() as isize;
    context.pop_symbol_table();

    thonk.add_instruction(Instruction::JumpIfFalse(true_block_len + 1), location!());
    thonk.append(true_thonk);
    if let Some(false_thonk) = &false_thonk {
        thonk.add_instruction(
            Instruction::Jump(false_thonk.get_instruction_card() as isize),
            location!(),
        );
    }
    if let Some(false_thonk) = false_thonk {
        thonk.append(false_thonk);
    }

    Ok(())
}
