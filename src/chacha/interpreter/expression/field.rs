use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    lu_dog::{FieldAccessTargetEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub mod field_access {
    use super::*;

    pub fn eval(
        field: &SarzakStorePtr,
        context: &mut Context,
        vm: &mut VM,
    ) -> Result<(RefType<Value>, RefType<ValueType>)> {
        let lu_dog = context.lu_dog_heel().clone();

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
            FieldAccessTargetEnum::Function(ref func) => {
                let func = s_read!(lu_dog).exhume_function(func).unwrap();
                let func = s_read!(func);
                func.name.to_owned()
            }
        };

        debug!("field_name: {field_name}");

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

        debug!("expression: {expr:?}");

        let value = {
            let result = eval_expression(expr, context, vm)?;
            result.0
        };

        debug!("value: {value:?}");

        // 🚧 This feels dirty. Some thought is necessary...
        // I should maybe document what I'm doing. It had something to do with the
        // plug-in stuff.
        let mut value = (*s_read!(value)).clone();
        match &mut value {
            Value::ProxyType((module, _, ref mut proxy)) => {
                match proxy
                    .invoke_func(
                        module.as_str().into(),
                        "self".into(),
                        "get_field_value".into(),
                        vec![Value::String(field_name.clone()).into()].into(),
                    )
                    .into()
                {
                    Ok(value) => {
                        debug!("ProxyType return value: {value:?}");

                        let value: Value = value.into();
                        let ty = value.get_type(&s_read!(context.sarzak_heel()), &s_read!(lu_dog));

                        debug!("ProxyType value: {value:?}, type: {ty:?}");

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
            Value::Struct(value) => {
                let value = s_read!(value);
                let value = value.get_field_value(&field_name).unwrap();
                let ty = {
                    let lu_dog = s_read!(lu_dog);
                    s_read!(value).get_type(&s_read!(context.sarzak_heel()), &lu_dog)
                };

                Ok((value.clone(), ty))
            }
            // 🚧 This needs it's own error. Lazy me.
            bad => Err(ChaChaError::BadJuJu {
                message: format!("Bad value ({bad}) in field access"),
                location: location!(),
            }),
        }
    }
}

pub mod field_expression {
    use super::*;

    pub fn eval(
        field_expr: &SarzakStorePtr,
        context: &mut Context,
        vm: &mut VM,
    ) -> Result<(RefType<Value>, RefType<ValueType>)> {
        let lu_dog = context.lu_dog_heel().clone();

        let field_expr = s_read!(lu_dog).exhume_field_expression(field_expr).unwrap();
        let expr = s_read!(field_expr).r38_expression(&s_read!(lu_dog))[0].clone();
        eval_expression(expr, context, vm)
    }
}
