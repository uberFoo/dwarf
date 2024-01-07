use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{ComparisonEnum, Expression, Operator},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    comp: &SarzakStorePtr,
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
    let comp = s_read!(lu_dog).exhume_comparison(comp).unwrap();

    debug!("eval_comparison: {lhs:?} {comp:?} {rhs:?}");

    let comp = s_read!(comp);
    match comp.subtype {
        ComparisonEnum::Equal(_) => {
            let value = *s_read!(lhs) == *s_read!(rhs);
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
        ComparisonEnum::GreaterThan(_) => {
            let value = s_read!(lhs).gt(&s_read!(rhs));
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
        ComparisonEnum::GreaterThanOrEqual(_) => {
            let value = s_read!(lhs).gte(&s_read!(rhs));
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
        ComparisonEnum::LessThan(_) => {
            let value = s_read!(lhs).lt(&s_read!(rhs));
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
        ComparisonEnum::LessThanOrEqual(_) => {
            let value = s_read!(lhs).lte(&s_read!(rhs));
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
        ComparisonEnum::NotEqual(_) => {
            let value = *s_read!(lhs) != *s_read!(rhs);
            let value = Value::Boolean(value);

            Ok(new_ref!(Value, value))
        }
    }
}
