use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{eval_statement, Context},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    block_id: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let block = s_read!(lu_dog).exhume_block(block_id).unwrap();
    let stmts = s_read!(block).r18_statement(&s_read!(lu_dog));

    if !stmts.is_empty() {
        context.memory().push_frame();
        let mut value;
        let mut next = s_read!(block).r71_statement(&s_read!(lu_dog))[0].clone();

        loop {
            value = eval_statement(next.clone(), context, vm).map_err(|e| {
                context.memory().pop_frame();
                e
            })?;

            if let Some(ref id) = s_read!(next.clone()).next {
                next = s_read!(lu_dog).exhume_statement(id).unwrap();
            } else {
                break;
            }
        }

        // Clean up
        context.memory().pop_frame();

        Ok(value)
    } else {
        Ok(new_ref!(Value, Value::Empty))
    }
}
