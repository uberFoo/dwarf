use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{Expression, UnaryEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    unary: &SarzakStorePtr,
    lhs_expr: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let lhs = eval_expression(lhs_expr.clone(), context, vm)?;
    let unary = s_read!(lu_dog).exhume_unary(unary).unwrap();

    debug!("unary {unary:?}");

    let unary = s_read!(unary);
    match &unary.subtype {
        //
        // Negation
        //
        UnaryEnum::Negation(_) => {
            let value = -s_read!(lhs).clone();

            Ok(new_ref!(Value, value))
        }
        UnaryEnum::Not(_) => {
            let value = !s_read!(lhs).clone();

            Ok(new_ref!(Value, value))
        }
    }
}
