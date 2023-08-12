use ansi_term::Colour;

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, Context, PrintableValueType, UserType},
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

    // This is where we decide if we are going to create a user type or a proxy
    // type. They are identical until now. This is because until now they are
    // simply declarations. This is reifying the type and we have a decision to
    // make. The difference is the presence of a pointer to Object across R4.
    // if let Some(object) = s_read!(woog_struct).r4_object(&s_read!()).get(0) {
    //     let ty_name = PrintableValueType(&ty, context);
    //     let mut user_type = ProxyType::new(ty_name.to_string(), &ty);
    //     for (name, _ty, value, _expr) in field_exprs {
    //         user_type.add_attr(&name, value);
    //     }

    //     Ok((
    //         new_ref!(Value, Value::UserType(new_ref!(UserType, user_type))),
    //         ty,
    //     ))
    // } else {
    let ty_name = PrintableValueType(&ty, context);
    let mut user_type = UserType::new(ty_name.to_string(), &ty);
    for (name, _ty, value, _expr) in field_exprs {
        user_type.define_field(&name, value);
    }

    Ok((
        new_ref!(Value, Value::UserType(new_ref!(UserType, user_type))),
        ty,
    ))
    // }
}
