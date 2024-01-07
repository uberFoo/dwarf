use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    bubba::VM,
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    for_loop: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let for_loop = s_read!(lu_dog).exhume_for_loop(for_loop).unwrap();
    let for_loop = s_read!(for_loop);
    let ident = for_loop.ident.to_owned();

    let block = s_read!(lu_dog).exhume_expression(&for_loop.block).unwrap();
    let list = s_read!(lu_dog)
        .exhume_expression(&for_loop.expression)
        .unwrap();

    let list = eval_expression(list, context, vm)?;
    let list = if let Value::Vector { ty: _, inner: vec } = &s_read!(list).clone() {
        vec.to_owned()
    } else if let Value::String(str) = &*s_read!(list) {
        str.chars()
            .map(|c| new_ref!(Value, Value::Char(c)))
            .collect()
    } else if let Value::Range(range) = &*s_read!(list) {
        let mut vec = Vec::new();
        for i in range.start..range.end {
            vec.push(new_ref!(Value, Value::Integer(i)));
        }
        vec
    } else {
        return Err(ChaChaError::BadnessHappened {
            message: "For loop expression is not a list".to_owned(),
            location: location!(),
        });
    };

    debug!("for loop {ident} in {list:?}");

    context.memory().push_frame();
    for item in list {
        context.memory().insert(ident.clone(), item);
        let expr_ty = eval_expression(block.clone(), context, vm);
        match expr_ty {
            Ok(_) => {}
            Err(e) => {
                context.memory().pop_frame();
                return Err(e);
            }
        }
    }
    context.memory().pop_frame();

    let result = Ok(new_ref!(Value, Value::Empty));

    #[allow(clippy::let_and_return)]
    result
}
