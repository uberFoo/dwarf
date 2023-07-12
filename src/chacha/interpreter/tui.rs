#![cfg(not(any(feature = "single", feature = "single-vec")))]
use std::thread;

use ansi_term::Colour;
use crossbeam::channel::{unbounded, Receiver, Sender};
use snafu::{location, Location};

use crate::{
    chacha::vm::VM,
    dwarf::{inter_statement, parse_line, Context as ExtruderContext},
    interpreter::{
        debug, eval_statement, function, Context, DebuggerControl, DebuggerStatus, CVAR, RUNNING,
        STEPPING,
    },
    lu_dog::DwarfSourceFile,
    new_ref, s_read, s_write, NewRef, RefType,
};

pub fn start_tui_repl(mut context: Context) -> (Sender<DebuggerControl>, Receiver<DebuggerStatus>) {
    use std::time::Duration;

    use crossbeam::channel::RecvTimeoutError;

    let (to_ui_write, to_ui_read) = unbounded();
    let (from_ui_write, from_ui_read) = unbounded();
    // let (from_worker_write, from_worker_read) = unbounded();
    let (to_worker_write, to_worker_read) = unbounded();

    context.set_debug_status_writer(to_ui_write.clone());
    let std_out = context.std_out_recv().clone();

    // Control thread
    //
    // This one listens for events from the debugger (to set breakpoints, etc.).
    // It communicates with the worker thread via mutexes and the condition
    // variable.
    thread::Builder::new()
        .name("control".into())
        .spawn(move || loop {
            match from_ui_read.recv_timeout(Duration::from_millis(10)) {
                Ok(DebuggerControl::SetBreakpoint(character)) => {
                    debug!("Setting breakpoint at character {character}");
                }
                Ok(DebuggerControl::ExecuteInput(input)) => {
                    debug!("Executing input: {input}");
                    to_worker_write.send(input).unwrap();
                }
                Ok(DebuggerControl::StepInto) => {
                    debug!("Debugger StepInto");
                    *RUNNING.lock() = true;
                    CVAR.notify_all();
                }
                Ok(DebuggerControl::StepOver) => {
                    debug!("Debugger StepOver");
                }
                Ok(DebuggerControl::Run) => {
                    debug!("Debugger Run");
                    *STEPPING.lock() = false;
                    *RUNNING.lock() = true;
                    CVAR.notify_all();
                }
                Ok(DebuggerControl::Stop) => {
                    debug!("Debugger Stop");
                    break;
                }
                Err(RecvTimeoutError::Timeout) => {}
                Err(_) => {
                    debug!("Debugger control thread exiting");
                    break;
                }
            };
        })
        .unwrap();

    // Stdout thread
    //
    // Really? Another fucking thread?
    let to_ui = to_ui_write.clone();
    thread::Builder::new()
        .name("stdout".into())
        .spawn(move || loop {
            match std_out.recv_timeout(Duration::from_millis(10)) {
                Ok(output) => {
                    to_ui.send(DebuggerStatus::StdOut(output)).unwrap();
                }
                Err(RecvTimeoutError::Timeout) => {}
                Err(_) => {
                    debug!("Debugger control thread exiting");
                    break;
                }
            }
        })
        .unwrap();

    // Worker thread
    //
    // This guy listens for statements and executes them. It relies on the state
    // of the condition variable and mutexes to know how to behave.
    thread::Builder::new()
        .name("worker".into())
        // .stack_size(128 * 1024)
        .spawn(move || {
            let stack = &mut context.memory();
            let vm_stack = stack.clone();
            let mut vm = VM::new(&vm_stack);

            loop {
                match to_worker_read.recv_timeout(Duration::from_millis(10)) {
                    Ok(input) => match parse_line(&input) {
                        Ok(None) => {}
                        Ok(Some((stmt, _span))) => {
                            let lu_dog = context.lu_dog_heel();
                            let block = context.block();
                            let sarzak = context.sarzak_heel();
                            let models = context.models();

                            let stmt = {
                                let mut lu_dog = s_write!(lu_dog);
                                match inter_statement(
                                    &new_ref!(crate::dwarf::Statement, stmt),
                                    block,
                                    &mut ExtruderContext {
                                        location: location!(),
                                        struct_fields: Vec::new(),
                                        check_types: true,
                                        source: DwarfSourceFile::new(input, &mut lu_dog),
                                        models: &s_read!(models),
                                        sarzak: &s_read!(sarzak),
                                    },
                                    &mut lu_dog,
                                ) {
                                    Ok(stmt) => stmt.0,
                                    Err(e) => {
                                        to_ui_write
                                            .send(DebuggerStatus::Error(format!("{:?}", e)))
                                            .unwrap();
                                        continue;
                                    }
                                }
                            };

                            match eval_statement(stmt.0, &mut context, &mut vm) {
                                Ok((value, ty)) => {
                                    to_ui_write
                                        .send(DebuggerStatus::Stopped(value, ty))
                                        .unwrap();
                                }
                                Err(e) => {
                                    to_ui_write
                                        .send(DebuggerStatus::Error(format!("{:?}", e)))
                                        .unwrap();
                                }
                            }
                        }
                        Err(e) => {
                            to_ui_write.send(DebuggerStatus::Error(e)).unwrap();
                        }
                    },
                    Err(RecvTimeoutError::Timeout) => {}
                    Err(_) => {
                        debug!("Worker thread exiting");
                        break;
                    }
                }
            }
        })
        .unwrap();

    (from_ui_write, to_ui_read)
}
