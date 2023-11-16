use ansi_term::Colour;
use snafu::prelude::*;

use crate::{
    chacha::error::{Result, VariableNotFoundSnafu},
    interpreter::{debug, function, Context},
    lu_dog::Expression,
    s_read, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    expr: &SarzakStorePtr,
    expression: &RefType<Expression>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    // let expr = s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
    // let name = s_read!(expr).name.clone();
    // debug!("ExpressionEnum::VariableExpression expr: {:?}", expr);

    let name = {
        let expr = s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
        let name = s_read!(expr).name.clone();
        name
    };
    let value = context.memory().get(&name);

    ensure!(value.is_some(), {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let read = s_read!(span);
        let span = read.start as usize..read.end as usize;
        let var = name;
        VariableNotFoundSnafu { var, span }
    });

    let value = value.unwrap();
    debug!(
        "ExpressionEnum::VariableExpression value: {}",
        s_read!(value)
    );
    Ok(value)
}
