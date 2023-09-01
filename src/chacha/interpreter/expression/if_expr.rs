use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_if_expression(
    expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let expr = s_read!(lu_dog).exhume_x_if(expr).unwrap();
    let expr = s_read!(expr);
    debug!("ExpressionEnum::XIf {expr:?}");

    let cond_expr = s_read!(lu_dog).exhume_expression(&expr.test).unwrap();

    let cond = eval_expression(cond_expr, context, vm)?;
    debug!("ExpressionEnum::XIf conditional {cond:?}");

    let cond = s_read!(cond);
    Ok(if (&*cond).try_into()? {
        // Evaluate the true block
        let block = s_read!(lu_dog).exhume_block(&expr.true_block).unwrap();
        let block = s_read!(block).r15_expression(&s_read!(lu_dog))[0].clone();

        eval_expression(block, context, vm)?
    } else {
        debug!("ExpressionEnum::XIf else");
        if let Some(expr) = &expr.false_block {
            debug!("ExpressionEnum::XIf false block");
            // Evaluate the false block
            let block = s_read!(lu_dog).exhume_block(expr).unwrap();
            let block = s_read!(block).r15_expression(&s_read!(lu_dog))[0].clone();

            eval_expression(block, context, vm)?
        } else {
            new_ref!(Value, Value::Empty)
        }
    })
}
