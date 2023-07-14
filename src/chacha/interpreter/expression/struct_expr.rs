use ansi_term::Colour;
use snafu::{location, prelude::*, Location};

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{
        debug, eval_expression, function, typecheck, Context, NoSuchFieldSnafu, PrintableValueType,
        UserType,
    },
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_struct_expression(
    expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let expr = s_read!(lu_dog).exhume_struct_expression(expr).unwrap();
    let field_exprs = s_read!(expr).r26_field_expression(&s_read!(lu_dog));

    // Get name, value and type for each field expression.
    let field_exprs = field_exprs
        .iter()
        .map(|f| {
            let expr = s_read!(f).r15_expression(&s_read!(lu_dog))[0].clone();
            let (value, ty) = eval_expression(expr.clone(), context, vm)?;
            debug!(
                "StructExpression field value: {}, type: {:?}",
                s_read!(value),
                s_read!(ty)
            );
            Ok((s_read!(f).name.clone(), ty, value, expr))
        })
        .collect::<Result<Vec<_>>>()?;

    let woog_struct = s_read!(expr).r39_woog_struct(&s_read!(lu_dog))[0].clone();
    let ty = s_read!(woog_struct).r1_value_type(&s_read!(lu_dog))[0].clone();
    let fields = s_read!(woog_struct).r7_field(&s_read!(lu_dog));

    // 🚧 Don't I do this in the extruder? Can't I?
    // Type checking fields here
    let ty_name = PrintableValueType(&ty, context);
    let mut user_type = UserType::new(ty_name.to_string(), &ty);
    let lu_dog = s_read!(lu_dog);
    for (name, ty, value, expr) in field_exprs {
        if let Some(field) = fields.iter().find(|f| s_read!(f).name == name) {
            let struct_ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();

            let x_value = &s_read!(expr).r11_x_value(&lu_dog)[0];
            let span = &s_read!(x_value).r63_span(&lu_dog)[0];

            typecheck(&struct_ty, &ty, span, location!(), context)?;

            // This is where we add the attribute value to the user type.
            user_type.add_attr(&name, value);
        } else {
            let x_value = &s_read!(expr).r11_x_value(&lu_dog)[0];
            let span = &s_read!(x_value).r63_span(&lu_dog)[0];
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            ensure!(
                false,
                NoSuchFieldSnafu {
                    field: name.to_owned(),
                    span,
                }
            );
        }
    }

    Ok((
        new_ref!(Value, Value::UserType(new_ref!(UserType, user_type))),
        ty,
    ))
}
