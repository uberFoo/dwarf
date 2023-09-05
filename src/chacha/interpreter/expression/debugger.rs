use ansi_term::Colour;

use crate::{
    chacha::error::Result,
    interpreter::{debug, function, Context, RUNNING, STEPPING},
    new_ref, NewRef, RefType, Value,
};

pub fn eval(_context: &mut Context) -> Result<RefType<Value>> {
    debug!("StatementEnum::Debugger");
    let mut running = RUNNING.lock();
    *running = false;
    *STEPPING.lock() = true;
    Ok(new_ref!(Value, Value::Empty))
}
