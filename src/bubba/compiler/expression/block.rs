use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    bubba::{
        compiler::{compile_statement, CThonk, Context, Result, EMPTY},
        Instruction,
    },
    lu_dog::ValueType,
    s_read, SarzakStorePtr, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(thonk, context)))]
pub(in crate::bubba::compiler) fn compile(
    block: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}:\n  --> {}:{}:{}", POP_CLR.paint("block"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let empty = context.get_type(EMPTY).unwrap().clone();
    let lu_dog = s_read!(lu_dog);

    let block = lu_dog.exhume_block(block).unwrap();
    let block = s_read!(block);
    let a_sink = block.a_sink;
    if a_sink {
        let name = format!("{}", Uuid::new_v4());
        let mut inner_thonk = CThonk::new(name.clone());

        context.insert_lambda(name.clone(), 0);
        let stmts = block.r18_statement(&lu_dog);
        if !stmts.is_empty() {
            context.push_scope();
            let mut next = block.r71_statement(&lu_dog)[0].clone();
            let mut ty;

            loop {
                ty = compile_statement(&next, &mut inner_thonk, context)?;
                // compile_statement(&next, thonk, context)?;

                if let Some(ref id) = s_read!(next.clone()).next {
                    next = lu_dog.exhume_statement(id).unwrap();
                } else {
                    break;
                }
            }
            inner_thonk.insert_instruction(Instruction::Return, location!());

            context.pop_scope();
            context.get_program().add_thonk(inner_thonk.into());

            thonk.insert_instruction(Instruction::MakeLambdaPointer(name, 0), location!());
            thonk.insert_instruction(Instruction::CreateTask(0), location!());

            Ok(ty)
        } else {
            thonk.insert_instruction(
                Instruction::Push("Empty function body.".into()),
                location!(),
            );
            thonk.insert_instruction(Instruction::Push((0..0).into()), location!());
            thonk.insert_instruction(Instruction::HaltAndCatchFire, location!());
            Ok(Some(empty))
        }
    } else {
        let stmts = block.r18_statement(&lu_dog);
        if !stmts.is_empty() {
            context.push_scope();
            let mut next = block.r71_statement(&lu_dog)[0].clone();
            let mut ty;

            loop {
                ty = compile_statement(&next, thonk, context)?;
                // compile_statement(&next, thonk, context)?;

                if let Some(ref id) = s_read!(next.clone()).next {
                    next = lu_dog.exhume_statement(id).unwrap();
                } else {
                    break;
                }
            }
            context.pop_scope();

            Ok(ty)
        } else {
            thonk.insert_instruction(
                Instruction::Push("Empty function body.".into()),
                location!(),
            );
            thonk.insert_instruction(Instruction::Push((0..0).into()), location!());
            thonk.insert_instruction(Instruction::HaltAndCatchFire, location!());
            Ok(Some(empty))
        }
    }
}
