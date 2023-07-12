use ansi_term::Colour;

use crate::{
    chacha::vm::VM,
    interpreter::{debug, function, Context},
    lu_dog::{Expression, OperatorEnum, ValueType},
    s_read, RefType, Result, SarzakStorePtr, Value,
};

pub mod binary;
pub mod comparison;
pub mod unary;

pub fn eval_operator(
    operator: &SarzakStorePtr,
    expression: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();

    let operator = s_read!(lu_dog).exhume_operator(operator).unwrap();
    let lhs_expr = s_read!(lu_dog)
        .exhume_expression(&s_read!(operator).lhs)
        .unwrap();

    debug!("operator {operator:?}");

    let match_expr = s_read!(operator);
    match &match_expr.subtype {
        OperatorEnum::Binary(ref binary) => {
            binary::eval_binary(binary, &lhs_expr, &operator, expression, context, vm)
        }
        OperatorEnum::Comparison(ref comp) => {
            comparison::eval_comparison(comp, &lhs_expr, &operator, context, vm)
        }
        OperatorEnum::Unary(ref unary) => unary::eval_unary(unary, &lhs_expr, context, vm),
    }
}
