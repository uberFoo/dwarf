use crate::{
    chacha::{error::Result, value::UserEnum, vm::VM},
    interpreter::Context,
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_enum_field(
    enum_field: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let field = s_read!(lu_dog).exhume_enum_field(enum_field).unwrap();
    let field = s_read!(field);
    let woog_enum = field.r88_enumeration(&s_read!(lu_dog))[0].clone();
    let woog_enum = s_read!(woog_enum);
    let ty = s_read!(lu_dog)
        .iter_value_type()
        .find(|ty| {
            if let ValueTypeEnum::Enumeration(id) = s_read!(ty).subtype {
                id == woog_enum.id
            } else {
                false
            }
        })
        .unwrap();

    let user_enum = UserEnum::new(
        woog_enum.name.clone(),
        &ty,
        new_ref!(Value, Value::String(format!("{}", field.name))),
    );
    let user_enum = new_ref!(UserEnum, user_enum);

    Ok((new_ref!(Value, Value::Enum(user_enum)), ty))
}
