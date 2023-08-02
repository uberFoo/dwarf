use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    lu_dog::{FieldAccessTargetEnum, ValueType},
    s_read, RefType, SarzakStorePtr, Value,
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

    let (value, _ty) = eval_expression(expr, context, vm)?;
    let value = s_read!(value);
    match &*value {
        Value::ProxyType(value) => {
            unimplemented!();
            // let value = s_read!(value);
            // let value = value.get_attr_value(&field_name)?;
            // let ty = s_read!(value).get_type(&s_read!(lu_dog));

            // Ok((value, ty))
        }
        Value::UserType(value) => {
            let value = s_read!(value);
            let value = value.get_field_value(field_name).unwrap();
            let ty = s_read!(value).get_type(&s_read!(lu_dog));

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
