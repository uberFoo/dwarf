use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    lu_dog::{Expression, ExpressionEnum, FieldAccessTargetEnum, Operator},
    new_ref, s_read, s_write, NewRef, RefType, Value,
};

pub fn eval_assignment(
    lhs_expr: &RefType<Expression>,
    operator: &RefType<Operator>,
    _expression: &RefType<Expression>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    debug!("Evaluating assignment lhs: {lhs_expr:?}");

    // Type checking has already been handled by the compiler.
    match &s_read!(lhs_expr).subtype {
        ExpressionEnum::FieldAccess(field) => {
            let rhs = {
                let rhs = s_read!(operator).rhs.unwrap();
                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                eval_expression(rhs, context)?
            };
            let field = s_read!(lu_dog).exhume_field_access(field).unwrap();

            let fat = &s_read!(field).r65_field_access_target(&s_read!(lu_dog))[0];
            let field_name = match s_read!(fat).subtype {
                FieldAccessTargetEnum::EnumField(ref field) => {
                    let field = s_read!(lu_dog).exhume_enum_field(field).unwrap();
                    let field = s_read!(field);
                    field.name.to_owned()
                }
                FieldAccessTargetEnum::Field(ref field) => {
                    let field = s_read!(lu_dog).exhume_field(field).unwrap();
                    let field = s_read!(field);
                    field.name.to_owned()
                }
                FieldAccessTargetEnum::Function(_) => {
                    return Err(ChaChaError::BadnessHappened {
                        message: "Attempt to assign to function".to_owned(),
                        location: location!(),
                    })
                }
            };

            debug!("field access: rhs: {rhs:?}, field: {field_name}");

            let expr = &s_read!(field).expression;
            let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
            let value = eval_expression(expr, context)?;

            debug!("value: {value:?}");

            let value = s_read!(value);
            match &*value {
                Value::ProxyType {
                    module,
                    obj_ty: _,
                    id: _,
                    plugin: ref proxy,
                } => {
                    let mut proxy = s_write!(proxy);
                    let result = proxy.invoke_func(
                        module.as_str().into(),
                        "self".into(),
                        "set_field_value".into(),
                        vec![
                            Value::String(field_name.clone()).into(),
                            (*s_read!(rhs)).clone().into(),
                        ]
                        .into(),
                    );
                    if let Err(e) = result.into() {
                        // 🚧 This needs it's own error. Lazy me.
                        return Err(ChaChaError::BadnessHappened {
                            message: format!("{e}"),
                            location: location!(),
                        });
                    }
                }
                Value::Struct(value) => {
                    s_write!(value).set_field_value(&field_name, rhs);
                }
                // 🚧 This needs it's own error.
                _value => {
                    return Err(ChaChaError::BadnessHappened {
                        message: "Attempt to assign to non-struct".to_owned(),
                        location: location!(),
                    })
                }
            }

            Ok(new_ref!(Value, Value::Empty))
        }
        ExpressionEnum::TypeCast(expr) => {
            let rhs = {
                let rhs = s_read!(operator).rhs.unwrap();
                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                eval_expression(rhs, context)?
            };
            let expr = s_read!(lu_dog).exhume_type_cast(expr).unwrap();
            let expr = s_read!(expr).r68_expression(&s_read!(lu_dog))[0].clone();
            // We are going to assume that the lhs, that we just read, is
            // in fact a variable expression. I honestly don't know what else
            // it could be, since LHS expression manipulation is weird. I
            // don't think it really makes any sense, but maybe I'm wrong.
            let result = if let ExpressionEnum::VariableExpression(ref id) = s_read!(expr).subtype {
                let expr = s_read!(lu_dog).exhume_variable_expression(id).unwrap();
                let name = s_read!(expr).name.clone();
                let value = context.memory().get(&name).unwrap();
                let mut value = s_write!(value);
                *value = s_read!(rhs).clone();
                Ok(rhs)
            } else {
                Err(ChaChaError::BadnessHappened {
                    message: "Attempt to assign to non-variable".to_owned(),
                    location: location!(),
                })
            };

            #[allow(clippy::let_and_return)]
            result
        }
        // 🚧 I'm sort of duplicating work here. It's not exactly the same
        // as the general expression handling code, but I think it's close
        // enough that I could make it work if I wanted to. And so I should.
        //
        // Hm. I've already processed the lhs above, and I'm basically doing
        // it again here.
        ExpressionEnum::VariableExpression(expr) => {
            let rhs = {
                let rhs = s_read!(operator).rhs.unwrap();
                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                eval_expression(rhs, context)?
            };
            let expr = s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
            let expr = s_read!(expr);
            let name = expr.name.clone();
            let value = context.memory().get(&name).unwrap();
            let mut value = s_write!(value);
            *value = s_read!(rhs).clone();
            Ok(rhs)
        }
        lhs => Err(ChaChaError::BadnessHappened {
            message: format!("Bad LHS in assignment: {lhs:?}"),
            location: location!(),
        }),
    }

    // Ok((lhs, lhs_ty))
}
