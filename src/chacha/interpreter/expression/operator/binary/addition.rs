use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{Expression, Operator, ValueType},
    new_ref, s_read, NewRef, RefType, Value,
};

pub fn eval_addition(
    lhs_expr: &RefType<Expression>,
    operator: &RefType<Operator>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lhs = eval_expression(lhs_expr.clone(), context, vm)?;
    let rhs = {
        let rhs = s_read!(operator).rhs.unwrap();
        let rhs = s_read!(context.lu_dog_heel())
            .exhume_expression(&rhs)
            .unwrap();
        eval_expression(rhs, context, vm)?
    };

    let value = s_read!(lhs).clone() + s_read!(rhs).clone();

    debug!("addition {lhs:?} + {rhs:?} = {value:?}");

    Ok(new_ref!(Value, value))
}
