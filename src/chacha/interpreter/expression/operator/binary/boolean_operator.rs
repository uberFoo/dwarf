use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{BooleanOperatorEnum, Expression, Operator},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_boolean_operator(
    op: &SarzakStorePtr,
    lhs_expr: &RefType<Expression>,
    operator: &RefType<Operator>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let lhs = eval_expression(lhs_expr.clone(), context, vm)?;
    let rhs = {
        let rhs = s_read!(operator).rhs.unwrap();
        let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
        eval_expression(rhs, context, vm)?
    };
    let boolean_operator = s_read!(lu_dog).exhume_boolean_operator(op).unwrap();
    let boolean_operator = s_read!(boolean_operator);

    debug!(
        "Evaluating boolean operator: {:?}",
        boolean_operator.subtype
    );

    match &boolean_operator.subtype {
        BooleanOperatorEnum::And(_) => {
            let value = Value::Boolean(
                (&*s_read!(lhs)).try_into().unwrap() && (&*s_read!(rhs)).try_into().unwrap(),
            );
            Ok(new_ref!(Value, value))
        }
        BooleanOperatorEnum::Or(_) => {
            let value = Value::Boolean(
                (&*s_read!(lhs)).try_into().unwrap() || (&*s_read!(rhs)).try_into().unwrap(),
            );
            Ok(new_ref!(Value, value))
        }
    }
}
