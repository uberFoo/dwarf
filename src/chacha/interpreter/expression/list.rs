use ansi_term::Colour;

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::{List, ValueType},
    new_ref, s_read, s_write, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_list_element(
    element: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    debug!("list element {element:?}");

    let element = s_read!(lu_dog).exhume_list_element(element).unwrap();
    let element = s_read!(element);
    let expr = element.r55_expression(&s_read!(lu_dog))[0].clone();
    eval_expression(expr, context, vm)
}

pub fn eval_list_expression(
    list: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let list = s_read!(lu_dog).exhume_list_expression(list).unwrap();
    let list = s_read!(list);

    if let Some(ref element) = list.elements {
        // 🚧 This seems like the sort of thing that can be sorted out by the
        // extruder.
        // This is the first element in the list. We need to give this list
        // a type, and I'm going to do the easy thing here and take the type
        // to be whatever the first element evaluates as.
        let element = s_read!(lu_dog).exhume_list_element(element).unwrap();
        let element = s_read!(element);

        let expr = element.r15_expression(&s_read!(lu_dog))[0].clone();

        let ty = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
        let ty = s_read!(ty).r24_value_type(&s_read!(lu_dog))[0].clone();

        let value = eval_expression(expr, context, vm)?;

        let mut values = vec![value.clone()];

        let mut next = element.next;
        while let Some(ref id) = next {
            let element = s_read!(lu_dog).exhume_list_element(id).unwrap();
            let element = s_read!(element);
            let expr = element.r15_expression(&s_read!(lu_dog))[0].clone();
            let value = eval_expression(expr, context, vm)?;
            values.push(value);
            next = element.next;
        }

        Ok(new_ref!(Value, Value::Vector { ty, inner: values }))
    } else {
        let ty = ValueType::new_empty(true, &mut s_write!(lu_dog));

        Ok(new_ref!(Value, Value::Vector { ty, inner: vec![] }))
    }
}
