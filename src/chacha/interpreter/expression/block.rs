#[cfg(feature = "async")]
use tracing::{debug_span, Instrument};

use crate::{
    chacha::error::Result,
    interpreter::{eval_statement, Context},
    lu_dog::Block,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(block_id: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let block = s_read!(lu_dog).exhume_block(block_id).unwrap();

    #[cfg(feature = "async")]
    {
        if s_read!(block).a_sink {
            let mut cloned_context = context.clone();
            let span = debug_span!("async block", target = "async");
            let future = async move { eval_inner(block, &mut cloned_context) }.instrument(span);
            let task = context.worker().unwrap().create_task(future).unwrap();

            let value = new_ref!(
                Value,
                Value::Future {
                    name: "block".to_owned(),
                    task: Some(task),
                    executor: context.executor().clone()
                }
            );

            Ok(value)
        } else {
            eval_inner(block, context)
        }
    }
    #[cfg(not(feature = "async"))]
    eval_inner(block, context)
}

pub fn eval_inner(block: RefType<Block>, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let stmts = s_read!(block).r18_statement(&s_read!(lu_dog));

    if !stmts.is_empty() {
        context.memory().push_frame();
        let mut value;
        let mut next = s_read!(block).r71_statement(&s_read!(lu_dog))[0].clone();

        loop {
            value = eval_statement(next.clone(), context).map_err(|e| {
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
