use ansi_term::Colour;

use crate::{
    chacha::{
        error::Result,
        value::EnumFieldVariant,
        value::{UserEnum, UserStruct},
        vm::VM,
    },
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::EnumFieldEnum,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    enum_field: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    debug!("eval enum_field: {enum_field}");

    let lu_dog = context.lu_dog_heel().clone();

    let field = s_read!(lu_dog).exhume_enum_field(enum_field).unwrap();
    let field = s_read!(field);
    let woog_enum = &field.r88_enumeration(&s_read!(lu_dog))[0];
    let woog_enum = s_read!(woog_enum);
    let ty = woog_enum.r1_value_type(&s_read!(lu_dog))[0].clone();

    let value = match field.subtype {
        EnumFieldEnum::Plain(_) => new_ref!(
            Value,
            Value::EnumVariant(EnumFieldVariant::Plain(field.name.to_string()))
        ),
        EnumFieldEnum::StructField(ref sf) => {
            let struct_field = s_read!(lu_dog).exhume_struct_field(sf).unwrap();
            let struct_field = s_read!(struct_field);
            let expr = struct_field.expression.unwrap();
            let expr = s_read!(lu_dog).exhume_expression(&expr).unwrap();
            let value = eval_expression(expr, context, vm)?;
            let value = s_read!(value);
            if let Value::Struct(struct_value) = &*value {
                let struct_value = s_read!(struct_value);
                new_ref!(
                    Value,
                    Value::EnumVariant(EnumFieldVariant::Struct(
                        field.name.to_owned(),
                        new_ref!(UserStruct, struct_value.clone())
                    ))
                )
            } else {
                unreachable!()
            }
        }
        EnumFieldEnum::TupleField(ref tf) => {
            let tuple = s_read!(lu_dog).exhume_tuple_field(tf).unwrap();
            let _ty = s_read!(tuple).r86_value_type(&s_read!(lu_dog))[0].clone();
            // 🚧 Note that indexing into this is doing an implicit unwrap on
            // the optional expression relationship.
            let expr = s_read!(tuple).r90_expression(&s_read!(lu_dog))[0].clone();
            let value = eval_expression(expr, context, vm)?;
            new_ref!(
                Value,
                Value::EnumVariant(EnumFieldVariant::Tuple(field.name.to_owned(), value))
            )
        }
    };

    let user_enum = UserEnum::new(woog_enum.name.clone(), &ty, value);
    let user_enum = new_ref!(UserEnum, user_enum);

    Ok(new_ref!(Value, Value::Enum(user_enum)))
}
