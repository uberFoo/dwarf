use ansi_term::Colour;

use crate::{
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_if_expression(expr: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_x_if(expr).unwrap();
    let expr = s_read!(expr);
    debug!("ExpressionEnum::XIf {expr:?}");

    let cond_expr = lu_dog.exhume_expression(&expr.test).unwrap();

    let cond = eval_expression(cond_expr, context)?;
    debug!("ExpressionEnum::XIf conditional {cond:?}");

    let cond = s_read!(cond);
    Ok(if (&*cond).try_into()? {
        // Evaluate the true block
        let block = lu_dog.exhume_block(&expr.true_block).unwrap();
        let block = s_read!(block).r15_expression(&lu_dog)[0].clone();

        eval_expression(block, context)?
    } else {
        debug!("ExpressionEnum::XIf else");
        if let Some(expr) = &expr.false_block {
            debug!("ExpressionEnum::XIf false block");
            // Evaluate the false block
            let expr = lu_dog.exhume_expression(expr).unwrap();

            eval_expression(expr, context)?
        } else {
            new_ref!(Value, Value::Empty)
        }
    })
}
