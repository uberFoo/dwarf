use ansi_term::Colour;

use crate::{
    chacha::vm::VM,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{Expression, UnaryEnum, ValueType},
    new_ref, s_read, NewRef, RefType, Result, SarzakStorePtr, Value,
};

pub fn eval_unary(
    unary: &SarzakStorePtr,
    lhs_expr: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();

    let (lhs, lhs_ty) = eval_expression(lhs_expr.clone(), context, vm)?;
    let unary = s_read!(lu_dog).exhume_unary(unary).unwrap();

    debug!("unary {unary:?}");

    let unary = s_read!(unary);
    match &unary.subtype {
        //
        // Negation
        //
        UnaryEnum::Negation(_) => {
            let value = -s_read!(lhs).clone();

            Ok((new_ref!(Value, value), lhs_ty))
        }
        UnaryEnum::Not(_) => {
            let value = !s_read!(lhs).clone();

            Ok((new_ref!(Value, value), lhs_ty))
        }
    }
}
