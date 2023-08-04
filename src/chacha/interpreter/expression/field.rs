use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    lu_dog::{FieldAccessTargetEnum, ValueType},
    new_ref, s_read, s_write, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_field_access(
    field: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let field = s_read!(lu_dog).exhume_field_access(field).unwrap();
    let fat = &s_read!(field).r65_field_access_target(&s_read!(lu_dog))[0];
    let field_name = match s_read!(fat).subtype {
        FieldAccessTargetEnum::Field(ref field) => {
            let field = s_read!(lu_dog).exhume_field(field).unwrap();
            let field = s_read!(field);
            field.name.to_owned()
        }
        FieldAccessTargetEnum::Function(ref func) => {
            let func = s_read!(lu_dog).exhume_function(func).unwrap();
            let func = s_read!(func);
            func.name.to_owned()
        }
    };

    debug!("FieldAccess field_name: {field_name}");

    // What we're doing below is actually dereferencing a pointer. I wonder
    // if there is a way to make this less confusing and error prone? A
    // macro wouldn't work because the pointer is stored under various
    // names. So it would be a function on the referrer. Like relationship
    // navigation, actually.
    //      `let expr = field.expression(lu_dog).unwrap()`
    // Something like that.
    // A macro could maybe do it, if we pass the name of the field storing
    // the pointer, actually.
    //
    let expr = &s_read!(field).expression;
    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
    // dereference!(field, expression, lu_dog);

    let value = {
        let result = eval_expression(expr, context, vm)?;
        result.0
    };
    // let mut value = s_write!(value);
    // 🚧 This feels dirty. Some thought in necessary...
    let mut value = (*s_read!(value)).clone();
    match &mut value {
        Value::ProxyType((_, ref mut proxy)) => {
            match proxy
                .invoke_func(
                    "self".into(),
                    "get_field_value".into(),
                    vec![Value::String(field_name.clone()).into()].into(),
                )
                .into()
            {
                Ok(value) => {
                    let value: Value = value.into();
                    let ty = value.get_type(&s_read!(lu_dog), &s_read!(context.sarzak_heel()));

                    Ok((new_ref!(Value, value), ty))
                }
                Err(e) => {
                    // 🚧 This needs it's own error. Lazy me.
                    Err(ChaChaError::BadJuJu {
                        message: format!("{e}: `{field_name}`"),
                        location: location!(),
                    })
                }
            }
        }
        Value::UserType(value) => {
            let value = s_read!(value);
            let value = value.get_field_value(field_name).unwrap();
            let ty = s_read!(value).get_type(&s_read!(lu_dog), &s_read!(context.sarzak_heel()));

            Ok((value.clone(), ty))
        }
        // 🚧 This needs it's own error. Lazy me.
        _ => Err(ChaChaError::BadJuJu {
            message: "Bad value in field access".to_owned(),
            location: location!(),
        }),
    }
}

pub fn eval_field_expression(
    field_expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let field_expr = s_read!(lu_dog).exhume_field_expression(field_expr).unwrap();
    let expr = s_read!(field_expr).r38_expression(&s_read!(lu_dog))[0].clone();
    eval_expression(expr, context, vm)
}
