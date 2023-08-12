use ansi_term::Colour;

use crate::{
    chacha::error::Result,
    interpreter::{debug, function, Context, RUNNING, STEPPING},
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, Value,
};

pub fn eval_debugger(context: &mut Context) -> Result<(RefType<Value>, RefType<ValueType>)> {
    debug!("StatementEnum::Debugger");
    let mut running = RUNNING.lock();
    *running = false;
    *STEPPING.lock() = true;
    Ok((
        new_ref!(Value, Value::Empty),
        Value::Empty.get_type(
            &s_read!(context.sarzak_heel()),
            &s_read!(context.lu_dog_heel()),
        ),
    ))
}
