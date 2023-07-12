use ansi_term::Colour;

use crate::{
    chacha::vm::VM,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{ComparisonEnum, Expression, Operator, ValueType},
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, Result, SarzakStorePtr, Value,
};

pub fn eval_comparison(
    comp: &SarzakStorePtr,
    lhs_expr: &RefType<Expression>,
    operator: &RefType<Operator>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();

    let (lhs, _lhs_ty) = eval_expression(lhs_expr.clone(), context, vm)?;
    let rhs = {
        let rhs = s_read!(operator).rhs.unwrap();
        let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
        eval_expression(rhs, context, vm)?
    };
    let comp = s_read!(lu_dog).exhume_comparison(comp).unwrap();

    debug!("eval_comparison: {lhs:?} {comp:?} {rhs:?}");

    let comp = s_read!(comp);
    match &comp.subtype {
        ComparisonEnum::Equal(_) => {
            let value = *s_read!(lhs) == *s_read!(rhs.0);
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        ComparisonEnum::GreaterThan(_) => {
            let value = s_read!(lhs).gt(&s_read!(rhs.0));
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        ComparisonEnum::GreaterThanOrEqual(_) => {
            let value = s_read!(lhs).gte(&s_read!(rhs.0));
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        ComparisonEnum::LessThan(_) => {
            let value = s_read!(lhs).lt(&s_read!(rhs.0));
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        ComparisonEnum::LessThanOrEqual(_) => {
            let value = s_read!(lhs).lte(&s_read!(rhs.0));
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
        ComparisonEnum::NotEqual(_) => {
            let value = *s_read!(lhs) != *s_read!(rhs.0);
            let value = Value::Boolean(value);
            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

            Ok((new_ref!(Value, value), ty))
        }
    }
}
