use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, function, Context},
    lu_dog::{BinaryEnum, Expression, Operator},
    s_read, RefType, SarzakStorePtr, Value,
};

pub mod addition;
pub mod assignment;
pub mod boolean_operator;
pub mod division;
pub mod multiplication;
pub mod subtraction;

pub fn eval(
    binary: &SarzakStorePtr,
    lhs_expr: &RefType<Expression>,
    operator: &RefType<Operator>,
    expression: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let binary = s_read!(lu_dog).exhume_binary(binary).unwrap();
    let binary = s_read!(binary);

    debug!("Evaluating binary operator: {:?}", binary.subtype);

    match &binary.subtype {
        BinaryEnum::Addition(_) => addition::eval_addition(lhs_expr, operator, context, vm),
        BinaryEnum::Assignment(_) => {
            assignment::eval_assignment(lhs_expr, operator, expression, context, vm)
        }
        BinaryEnum::BooleanOperator(ref op) => {
            boolean_operator::eval_boolean_operator(op, lhs_expr, operator, context, vm)
        }
        BinaryEnum::Division(_) => division::eval_division(lhs_expr, operator, context, vm),
        BinaryEnum::Subtraction(_) => {
            subtraction::eval_subtraction(lhs_expr, operator, context, vm)
        }
        BinaryEnum::Multiplication(_) => {
            multiplication::eval_multiplication(lhs_expr, operator, context, vm)
        }
    }
}
