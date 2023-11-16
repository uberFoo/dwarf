#[cfg(feature = "async")]
use tracing::{debug_span, Instrument};

use crate::{
    chacha::{
        error::{ChaChaError, Result},
        vm::VM,
    },
    interpreter::{eval_statement, Context},
    lu_dog::Block,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

#[cfg(feature = "async")]
use super::Executor;

#[cfg(feature = "async")]
use uberfoo_async::AsyncTask;

pub fn eval<'a>(
    block_id: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM<'a>,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let block = s_read!(lu_dog).exhume_block(block_id).unwrap();

    #[cfg(feature = "async")]
    {
        if s_read!(block).a_sink {
            let mut cloned_context = context.clone();
            let span = debug_span!("async block", target = "async");
            let future = async move {
                let mem = cloned_context.memory().clone();
                let mut vm = VM::new(&mem);
                eval_inner(block, &mut cloned_context, &mut vm)
            }
            .instrument(span);

            // let task = ChaChaTask::new(Executor::global(), future);
            let task = AsyncTask::new(
                "block".to_owned(),
                Executor::at_index(context.executor_index()),
                future,
            );

            // let task = context.executor().spawn(future);

            let future = new_ref!(Value, Value::Future("block".to_owned(), Some(task)));

            // Stash the future away so that it doesn't get dropped when it's done running.
            // context.executor().park_value(future.clone());

            Ok(future)
        } else {
            eval_inner(block, context, vm)
        }
    }
    #[cfg(not(feature = "async"))]
    eval_inner(block, context, vm)
}

pub fn eval_inner(
    block: RefType<Block>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

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
