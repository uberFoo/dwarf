use std::{
    collections::VecDeque,
    fmt,
    io::Write,
    ops::Range,
    path::{Path},
    thread,
    time::Instant,
};

use ansi_term::Colour::{self, RGB};
use ansi_term::{ANSIString, ANSIStrings};
use circular_queue::CircularQueue;
use crossbeam::channel::{unbounded, Receiver, Sender};
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use lazy_static::lazy_static;
use log::{self, log_enabled, Level::Debug};

use parking_lot::{Condvar, Mutex};

// use rayon::prelude::*;
use snafu::{location, prelude::*, Location};
use tracy_client::{span, Client};
use uuid::Uuid;

use crate::{
    chacha::{
        memory::{Memory, MemoryUpdateMessage},
        vm::{CallFrame, Instruction, Thonk, VM},
    },
    dwarf::{inter_statement, parse_line},
    lu_dog::{
        Argument, Binary, Block, BooleanLiteral, BooleanOperator, CallEnum, Comparison,
        DwarfSourceFile, Expression, FieldAccessTarget, Function, Import, List, Literal,
        LocalVariable, ObjectStore as LuDogStore, OperatorEnum, Span, Statement, StatementEnum,
        Unary, ValueType, Variable, WoogOptionEnum, XValue, XValueEnum,
    },
    new_ref, s_read, s_write,
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
    value::{StoreProxy, UserType},
    ChaChaError, DwarfInteger, Error, NewRef, NoSuchFieldSnafu, NoSuchStaticMethodSnafu, RefType,
    Result, TypeMismatchSnafu, UnimplementedSnafu, Value, VariableNotFoundSnafu,
    WrongNumberOfArgumentsSnafu,
};

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}

// macro_rules! debug {
//     ($($arg:tt)*) => {
//         log::debug!(
//             target: "interpreter",
//             "{}: {}\n  --> {}:{}:{}",
//             Colour::Cyan.dimmed().italic().paint(function!()),
//             format_args!($($arg)*),
//             file!(),
//             line!(),
//             column!()
//         );
//     };
// }

// macro_rules! error {
//     ($($arg:tt)*) => {
//         log::error!(
//             target: "compiler",
//             "{}: {}\n  --> {}:{}:{}",
//             Colour::Red.dimmed().italic().paint(function!()),
//             format_args!($($arg)*),
//             file!(),
//             line!(),
//             column!()
//         );
//     };
// }

macro_rules! debug {
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::debug!(
                target: "interpreter",
                "{}: {} --> {:?}\n  --> {}:{}:{}",
                Colour::Green.dimmed().italic().paint(function!()),
                Colour::Yellow.underline().paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:literal) => {
        log::debug!(
            target: "interpreter",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            $arg,
            file!(),
            line!(),
            column!())
    };
    ($arg:expr) => {
        log::debug!(
            target: "interpreter",
            "{}: {:?}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            $arg,
            file!(),
            line!(),
            column!())
    };
}

macro_rules! error {
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::error!(
                target: "interpreter",
                "{}: {} --> {:?}\n  --> {}:{}:{}",
                Colour::Green.dimmed().italic().paint(function!()),
                Colour::Red.underline().paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:literal) => {
        log::error!(
            target: "interpreter",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            Colour::Red.underline().paint($arg),
            file!(),
            line!(),
            column!())
    };
    ($arg:expr) => {
        log::error!(
            target: "interpreter",
            "{}: {:?}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            Colour::Ref.underline().paint($arg),
            file!(),
            line!(),
            column!())
    };
}

macro_rules! no_debug {
    ($arg:expr) => {
        log::debug!("{}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
            "{} --> {}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

macro_rules! trace {
    ($arg:expr) => {
        log::trace!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::trace!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

// const VM: OnceCell<VM> = OnceCell::new();
// const CTX: OnceCell<Context> = OnceCell::new();

const TIMING_COUNT: usize = 1_000;

lazy_static! {
    static ref RUNNING: Mutex<bool> = Mutex::new(true);
    static ref CVAR: Condvar = Condvar::new();
    pub(crate) static ref STEPPING: Mutex<bool> = Mutex::new(false);
}

pub fn initialize_interpreter_paths<P: AsRef<Path>>(_lu_dog_path: P) -> Result<Context, Error> {
    unimplemented!();
    // let sarzak =
    //     SarzakStore::from_bincode(SARZAK_MODEL).map_err(|e| ChaChaError::Store { source: e })?;

    // // This will always be a lu-dog -- it's basically compiled dwarf source.
    // let lu_dog = LuDogStore::load_bincode(lu_dog_path.as_ref())
    //     .map_err(|e| ChaChaError::Store { source: e })?;

    // initialize_interpreter(sarzak, lu_dog, Some(lu_dog_path.as_ref()))
}

/// Initialize the interpreter
///
/// The interpreter requires two domains to operate. The first is the metamodel:
/// sarzak. The second is the compiled dwarf file.
///
/// So the metamodel is dumb. I think we use it for looking up the id of the UUID
/// type. Otherwise, I don't think it's used.

/// So, It's also used for looking up a bool type, and when printing value types.
/// I sort of want to keep it, but bundle it with the compiled dwarf file.
///
/// Requiring a compiled dwarf file is also sort of stupid. I think we could just
/// create an empty LuDog as our internal state.
pub fn initialize_interpreter<P: AsRef<Path>>(
    sarzak: SarzakStore,
    mut lu_dog: LuDogStore,
    _lu_dog_path: Option<P>,
) -> Result<Context, Error> {
    // Initialize the stack with stuff from the compiled source.
    let block = Block::new(Uuid::new_v4(), None, &mut lu_dog);
    let (mut stack, receiver) = Memory::new();

    // Insert the functions in the root frame.
    let funcs = lu_dog.iter_function().collect::<Vec<_>>();

    for func in funcs {
        let imp = s_read!(func).r9_implementation(&lu_dog);
        if imp.is_empty() {
            let name = s_read!(func).name.clone();
            let value = Value::Function(func.clone());

            // Build the local in the AST.
            let local = LocalVariable::new(Uuid::new_v4(), &mut lu_dog);
            let var = Variable::new_local_variable(name.clone(), &local, &mut lu_dog);
            let _value = XValue::new_variable(
                &block,
                &ValueType::new_function(&func, &mut lu_dog),
                &var,
                &mut lu_dog,
            );

            log::trace!("inserting local function {}", name);
            stack.insert(name, new_ref!(Value, value));
        }
    }

    // Insert static methods for each struct. They go into the meta table.
    for user_type in lu_dog.iter_woog_struct() {
        let user_type = s_read!(user_type);
        // Create a meta table for each struct.
        debug!("inserting meta table {}", user_type.name);
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation(&lu_dog);
        if !impl_.is_empty() {
            // For each function in the impl, insert the function. I should probably
            // check and only insert the static functions.
            // 🚧 Only insert the static functions
            for func in s_read!(impl_[0]).r9_function(&lu_dog) {
                debug!("inserting static function {}", s_read!(func).name);
                stack.insert_meta(
                    &user_type.name,
                    s_read!(func).name.to_owned(),
                    new_ref!(Value, Value::Function(func.clone(),)),
                )
            }
        }
    }

    if let Some(_id) = lu_dog.exhume_woog_struct_id_by_name("Complex") {
        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("norm_squared".to_string());

        // This is the function we are coding.
        // fn norm_squared(self: Complex) -> float {
        //     // This would be a direct translation on the assembly
        //     // below. But it's not what I want.
        //     asm!(
        //         "fetch 0",
        //         "push \"re\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "fetch 0",
        //         "push \"im\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "add",
        //         "ret"
        //     );
        //
        //     let result:Complex;
        //
        //     asm!(
        //         "fetch {self}",
        //         "push \"re\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "fetch {self}",
        //         "push \"im\"",
        //         "field",
        //         "dup",
        //         "mul",
        //         "add",
        //         "pop {result}"
        //     );
        //
        //     result
        //
        //     //self.re * self.re + self.im * self.im
        // }

        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // dup
        thonk.add_instruction(Instruction::Dup);
        // mul
        thonk.add_instruction(Instruction::Mul);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // dup
        thonk.add_instruction(Instruction::Dup);
        // mul
        thonk.add_instruction(Instruction::Mul);
        // add
        thonk.add_instruction(Instruction::Add);
        thonk.add_instruction(Instruction::Return);

        let slot = stack.reserve_thonk_slot();
        stack.insert_thonk(thonk, slot);

        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("add".to_string());

        // This is the function we are coding.
        // fn add(self: Complex, other: Complex) -> Complex {
        //     Complex {
        //         re: self.re + other.re,
        //         im: self.im + other.im,
        //     }
        // }

        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // Get the second parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(1));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // add
        thonk.add_instruction(Instruction::Add);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        // Write field
        thonk.add_instruction(Instruction::FieldWrite);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // Get the second parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(1));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // add
        thonk.add_instruction(Instruction::Add);
        // Get the first parameter off the stack
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        // Write field
        thonk.add_instruction(Instruction::FieldWrite);
        // // new
        // let ty = lu_dog.exhume_value_type(&id).unwrap();
        // thonk.add_instruction(Instruction::NewUserType("Complex".to_string(), ty, 2));
        thonk.add_instruction(Instruction::Return);

        let slot = stack.reserve_thonk_slot();
        stack.insert_thonk(thonk, slot);

        // Hack to try to get mandelbrot running faster...
        let mut thonk = Thonk::new("square".to_string());

        // This is the function we are coding.
        // fn square(self: Complex) -> Complex {
        //     Complex {
        //         re: self.re * self.re - self.im * self.im,
        //         im: 2.0 * self.re * self.im,
        //     }
        // }

        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // dup
        thonk.add_instruction(Instruction::Dup);
        // mul
        thonk.add_instruction(Instruction::Mul);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // dup
        thonk.add_instruction(Instruction::Dup);
        // mul
        thonk.add_instruction(Instruction::Mul);
        // sub
        thonk.add_instruction(Instruction::Subtract);
        // push {fp + 0}
        //  this is the one for write
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        // 2.0 * self.re * self.im
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "re"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "re".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // Get the parameter off the stack
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        //
        // field
        thonk.add_instruction(Instruction::FieldRead);
        // push 2.0
        thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.0.into())));
        // mul
        thonk.add_instruction(Instruction::Mul);
        // mul
        thonk.add_instruction(Instruction::Mul);
        // push {fp + 0}
        thonk.add_instruction(Instruction::PushLocal(0));
        // push "im"
        thonk.add_instruction(Instruction::Push(new_ref!(Value, "im".into())));
        thonk.add_instruction(Instruction::FieldWrite);
        thonk.add_instruction(Instruction::FieldWrite);
        // // new
        // let ty = lu_dog.exhume_value_type(&id).unwrap();
        // thonk.add_instruction(Instruction::NewUserType("Complex".to_string(), ty, 2));

        thonk.add_instruction(Instruction::Return);

        let slot = stack.reserve_thonk_slot();
        stack.insert_thonk(thonk, slot);
    }

    let (std_out_send, std_out_recv) = unbounded();

    Client::start();

    Ok(Context {
        prompt: format!("{} ", Colour::Blue.normal().paint("道:>")),
        memory: stack,
        block,
        lu_dog: new_ref!(LuDogStore, lu_dog),
        sarzak: new_ref!(SarzakStore, sarzak),
        models: new_ref!(Vec<SarzakStore>, Vec::new()),
        mem_update_recv: receiver,
        std_out_send,
        std_out_recv,
        debug_status_writer: None,
        // obj_file_path: lu_dog_path.map(|p| p.as_ref().to_owned()),
        timings: CircularQueue::with_capacity(TIMING_COUNT),
        expr_count: 0,
        func_calls: 0,
        args: None,
    })
}

fn eval_function_call(
    func: RefType<Function>,
    args: &[RefType<Argument>],
    arg_check: bool,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();
    context.func_calls += 1;

    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", context.memory);

    span!("eval_function_call");

    let func = s_read!(func);
    let block = s_read!(lu_dog).exhume_block(&func.block).unwrap();
    // let stmts = s_read!(block).r18_statement(&s_read!(lu_dog));
    let has_stmts = !s_read!(block).r18_statement(&s_read!(lu_dog)).is_empty();

    // if !stmts.is_empty() {
    // if !s_read!(block).r18_statement(&s_read!(lu_dog)).is_empty() {
    if has_stmts {
        // Collect timing info
        let now = Instant::now();
        let expr_count_start = context.expr_count;

        context.memory.push_frame();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&s_read!(lu_dog));

        // 🚧 I'd really like to see the source code printed out, with the function
        // call highlighted.
        // And can't we catch this is the compiler?
        ensure!(
            params.len() == args.len(),
            WrongNumberOfArgumentsSnafu {
                expected: params.len(),
                got: args.len()
            }
        );

        let params = if !params.is_empty() {
            let mut params = Vec::with_capacity(params.len());
            let mut next = func
                // .clone()
                .r13_parameter(&s_read!(lu_dog))
                .iter()
                .find(|p| s_read!(p).r14c_parameter(&s_read!(lu_dog)).is_empty())
                .unwrap()
                .clone();

            loop {
                let var = s_read!(s_read!(next).r12_variable(&s_read!(lu_dog))[0]).clone();
                let value = s_read!(var.r11_x_value(&s_read!(lu_dog))[0]).clone();
                let ty = value.r24_value_type(&s_read!(lu_dog))[0].clone();
                params.push((var.name.clone(), ty.clone()));

                let next_id = { s_read!(next).next };
                if let Some(ref id) = next_id {
                    next = s_read!(lu_dog).exhume_parameter(id).unwrap();
                } else {
                    break;
                }
            }

            params
        } else {
            Vec::new()
        };

        let arg_values = if !args.is_empty() {
            let mut arg_values = Vec::with_capacity(args.len());
            let mut next = args
                .iter()
                .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                .unwrap()
                .clone();

            loop {
                let expr = s_read!(lu_dog)
                    .exhume_expression(&s_read!(next).expression)
                    .unwrap();
                let (value, ty) = eval_expression(expr.clone(), context, vm)?;
                arg_values.push((expr, value, ty));

                let next_id = { s_read!(next).next };
                if let Some(ref id) = next_id {
                    next = s_read!(lu_dog).exhume_argument(id).unwrap();
                } else {
                    break;
                }
            }

            arg_values
        } else {
            Vec::new()
        };

        let zipped = params.into_iter().zip(arg_values);
        for ((name, param_ty), (expr, value, arg_ty)) in zipped {
            debug!("type check name", name);
            debug!("type check param_ty", param_ty);
            debug!("type check value", value);
            debug!("type check arg_ty", arg_ty);

            if arg_check {
                let x_value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(x_value).r63_span(&s_read!(lu_dog))[0];

                typecheck(&param_ty, &arg_ty, span, context)?;
            }

            context.memory.insert(name.clone(), value);
        }

        let mut value = new_ref!(Value, Value::Empty);
        let mut ty = ValueType::new_empty(&s_read!(lu_dog));
        // This is a pain.
        // Find the first statement, by looking for the one with no previous statement.
        // let mut next = stmts
        //     .iter()
        //     .find(|s| s_read!(s).r17c_statement(&s_read!(lu_dog)).is_empty())
        //     .unwrap()
        //     .clone();
        if let Some(ref id) = s_read!(block).statement {
            let mut next = s_read!(lu_dog).exhume_statement(id).unwrap();

            loop {
                let result = eval_statement(next.clone(), context, vm).map_err(|e| {
                    // This is cool, if it does what I think it does. We basically
                    // get the opportunity to look at the error, and do stuff with
                    // it, and then let it contitue on as if nothing happened.
                    //
                    // Anyway, we need to clean up the stack frame if there was an
                    // error. I'm also considering abusing the error type to pass
                    // through that we hit a return expression. I'm thinknig more
                    // and more that this is a Good Idea. Well, maybe just a good
                    // idea. We can basically just do an early, successful return.
                    //
                    // Well, that doesn't work: return applies to the closure.
                    context.memory.pop_frame();

                    // if let ChaChaError::Return { value } = &e {
                    //     let ty = value.get_type(&s_read!(lu_dog));
                    //     return Ok((value, ty));
                    // }

                    // Err(e)
                    e
                });

                if let Err(ChaChaError::Return { value, ty }) = &result {
                    return Ok((value.clone(), ty.clone()));
                }

                (value, ty) = result?;

                if let Some(ref id) = s_read!(next.clone()).next {
                    next = s_read!(lu_dog).exhume_statement(id).unwrap();
                } else {
                    break;
                }
            }
        }

        // Clean up
        context.memory.pop_frame();
        let elapsed = now.elapsed();
        // Counting 10k expressions per second
        let eps =
            (context.expr_count - expr_count_start) as f64 / elapsed.as_micros() as f64 * 10.0;
        context.timings.push(eps);

        Ok((value, ty))
    } else {
        Ok((
            new_ref!(Value, Value::Empty),
            ValueType::new_empty(&s_read!(lu_dog)),
        ))
    }
}

fn chacha_print<S: AsRef<str>>(result: S, _context: &mut Context) -> Result<()> {
    let result_style = Colour::Green.bold();
    cfg_if::cfg_if! {
        if #[cfg(feature = "print-std-out")] {
            print!("{}", result_style.paint(result.as_ref()));
            std::io::stdout().flush().unwrap();
        } else {
            context
                .std_out_send
                .send(format!("{}", result_style.paint(result.as_ref())))
                .context(crate::InternalCompilerChannelSnafu {
                    message: "error writing to std out queue".to_owned(),
                })?;
        }
    }
    Ok(())
}

fn eval_expression(
    expression: RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();
    let sarzak = context.sarzak.clone();

    // Timing goodness
    context.expr_count += 1;

    debug!("expression", expression);
    trace!("stack", context.memory);

    // context.tracy.span(span_location!("eval_expression"), 0);
    // context
    //     .tracy
    //     .non_continuous_frame(frame_name!("eval_expression"));
    span!("eval_expression");

    {
        let mut running = RUNNING.lock();
        if !*running {
            if let Some(sender) = &context.debug_status_writer {
                let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                debug!("value", value);

                let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                let read = s_read!(span);
                let span = read.start as usize..read.end as usize;
                sender.send(DebuggerStatus::Paused(span)).unwrap();
            }
            debug!("waiting");
            CVAR.wait(&mut running);
            debug!("notified");
        }

        if *STEPPING.lock() {
            debug!("stepping");
            *running = false;
        }

        debug!("running");
    }

    if log_enabled!(Debug) {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let read = s_read!(span);
        let span = read.start as usize..read.end as usize;
        let source = s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
        let source = s_read!(source);
        let source = &source.source;
        error!("{}", source[span].to_owned());
    }

    match *s_read!(expression) {
        //
        // Block
        //
        Expression::Block(ref block) => {
            let block = s_read!(lu_dog).exhume_block(block).unwrap();
            let stmts = s_read!(block).r18_statement(&s_read!(lu_dog));

            if !stmts.is_empty() {
                context.memory.push_frame();
                let mut value;
                let mut ty;
                // This is a pain.
                // Find the first statement, by looking for the one with no previous statement.
                // let mut next = stmts
                //     .iter()
                //     .find(|s| s_read!(s).r17c_statement(&s_read!(lu_dog)).is_empty())
                //     .unwrap()
                //     .clone();
                let mut next = s_read!(block).r71_statement(&s_read!(lu_dog))[0].clone();

                loop {
                    let result = eval_statement(next.clone(), context, vm).map_err(|e| {
                        // This is cool, if it does what I think it does. We basically
                        // get the opportunity to look at the error, and do stuff with
                        // it, and then let it contitue on as if nothing happened.
                        //
                        // Anyway, we need to clean up the stack frame if there was an
                        // error. I'm also considering abusing the error type to pass
                        // through that we hit a return expression. I'm thinknig more
                        // and more that this is a Good Idea. Well, maybe just a good
                        // idea. We can basically just do an early, successful return.
                        //
                        // Well, that doesn't work: return applies to the closure.
                        context.memory.pop_frame();

                        // if let ChaChaError::Return { value } = &e {
                        //     let ty = value.get_type(&s_read!(lu_dog));
                        //     return Ok((value, ty));
                        // }

                        // Err(e)
                        e
                    });

                    // if let Err(ChaChaError::Return { value, ty }) = &result {
                    //     return Ok((value.clone(), ty.clone()));
                    // }

                    (value, ty) = result?;

                    if let Some(ref id) = s_read!(next.clone()).next {
                        next = s_read!(lu_dog).exhume_statement(id).unwrap();
                    } else {
                        break;
                    }
                }

                // Clean up
                context.memory.pop_frame();

                Ok((value, ty))
            } else {
                Ok((
                    new_ref!(Value, Value::Empty),
                    ValueType::new_empty(&s_read!(lu_dog)),
                ))
            }
        }
        //
        // Call
        //
        Expression::Call(ref call) => {
            let call = s_read!(lu_dog).exhume_call(call).unwrap();
            debug!("call", call);
            let args = s_read!(call).r28_argument(&s_read!(lu_dog));
            debug!("args", args);
            // error!("arg_check", s_read!(call).arg_check);

            // This optional expression is the LHS of the call.
            let (value, ty) = if let Some(ref expr) = s_read!(call).expression {
                let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let (value, ty) = eval_expression(expr, context, vm)?;
                debug!("Expression::Call LHS value", s_read!(value));
                debug!("Expression::Call LHS ty", ty);

                let mut tuple_from_value = || -> Result<(RefType<Value>, RefType<ValueType>)> {
                    // Below we are reading the value of the LHS, and then using that
                    // to determine what to do with the RHS.
                    let read_value = s_read!(value);
                    match &*read_value {
                        Value::Function(ref func) => {
                            let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                            debug!("Expression::Call func", func);
                            let (value, ty) = eval_function_call(
                                func,
                                &args,
                                s_read!(call).arg_check,
                                context,
                                vm,
                            )?;
                            debug!("value", value);
                            debug!("ty", ty);
                            Ok((value, ty))
                        }
                        Value::ProxyType(pt) => {
                            let ty = s_read!(lu_dog)
                                .exhume_value_type(&s_read!(pt).struct_uuid())
                                .unwrap();
                            Ok((value.clone(), ty))
                        }
                        Value::UserType(ut) => Ok((value.clone(), s_read!(ut).get_type().clone())),
                        value_ => {
                            let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                            debug!("value", value);

                            let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                            let read = s_read!(span);
                            let span = read.start as usize..read.end as usize;

                            return Err(ChaChaError::NotAFunction {
                                value: value_.to_owned(),
                                span,
                            });
                        }
                    }
                };

                // First we need to check the type of the LHS to see if there are
                // any instance methods on the type. This seems weird. I'm not sure
                // where to put it in my brain just yet.
                // But basically it comes down to allowing things like
                // `x = 1; x.to_string();` -- Not sure copilot wrote that, but it looks interesting.
                // `"dwarf".len()`
                // Or iterators things like
                // `[1, 2, 3].iter().map(|x| x + 1)`
                // `["hello", "I", "am", "dwarf!"].sort();
                let x = match &*s_read!(ty) {
                    ValueType::Ty(ref id) => {
                        let _ty = s_read!(sarzak).exhume_ty(id).unwrap().clone();
                        match &_ty {
                            Ty::SString(_) => (value, ty.clone()),
                            _ => tuple_from_value()?,
                        }
                    }
                    _ => tuple_from_value()?,
                };
                x
            } else {
                (
                    new_ref!(Value, Value::Empty),
                    ValueType::new_empty(&s_read!(lu_dog)),
                )
            };

            // So we need to figure out the type that this is being called upon.
            let call_result = match (&s_read!(call).subtype, value, ty) {
                (CallEnum::MacroCall(_), _, _) => unimplemented!(),
                //
                // FunctionCall
                //
                // We already handled this above.
                (CallEnum::FunctionCall(_), value, ty) => Ok((value, ty)),
                //
                // MethodCall
                //
                (CallEnum::MethodCall(meth), value, ty) => {
                    let meth = s_read!(lu_dog).exhume_method_call(meth).unwrap();
                    let meth = &s_read!(meth).name;
                    debug!("MethodCall method", meth);
                    debug!("MethodCall value", value);
                    debug!("MethodCall type", ty);

                    match &*s_read!(value) {
                        Value::ProxyType(proxy_type) => {
                            let mut arg_values = if !args.is_empty() {
                                // The VecDeque is so that I can pop off the args, and then push them
                                // back onto a queue in the same order.
                                let mut arg_values = VecDeque::with_capacity(args.len());
                                let mut next = args
                                    .iter()
                                    .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                                    .unwrap()
                                    .clone();

                                loop {
                                    let expr = s_read!(lu_dog)
                                        .exhume_expression(&s_read!(next).expression)
                                        .unwrap();
                                    let (value, _ty) = eval_expression(expr, context, vm)?;
                                    arg_values.push_back(value);

                                    let next_id = { s_read!(next).next };
                                    if let Some(ref id) = next_id {
                                        next = s_read!(lu_dog).exhume_argument(id).unwrap();
                                    } else {
                                        break;
                                    }
                                }

                                arg_values
                            } else {
                                VecDeque::new()
                            };

                            s_write!(proxy_type).call(meth, &mut arg_values)
                        }
                        Value::String(string) => match meth.as_str() {
                            "len" => {
                                let len = unicode_segmentation::UnicodeSegmentation::graphemes(
                                    string.as_str(),
                                    true,
                                )
                                .collect::<Vec<&str>>()
                                .len();
                                let ty = Ty::new_integer();
                                let ty =
                                    ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));
                                Ok((new_ref!(Value, Value::Integer(len as i64)), ty))
                            }
                            "format" => {
                                let mut arg_map = HashMap::default();
                                let mut arg_values = if !args.is_empty() {
                                    // The VecDeque is so that I can pop off the args, and then push them
                                    // back onto a queue in the same order.
                                    // Gotta do this goofy thing because we don't have a first pointer,
                                    // and thery aren't in order.
                                    let mut arg_values = VecDeque::with_capacity(args.len());
                                    let mut next = args
                                        .iter()
                                        .find(|a| {
                                            s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty()
                                        })
                                        .unwrap()
                                        .clone();

                                    loop {
                                        let expr = s_read!(lu_dog)
                                            .exhume_expression(&s_read!(next).expression)
                                            .unwrap();

                                        let source = s_read!(lu_dog)
                                            .iter_dwarf_source_file()
                                            .next()
                                            .unwrap();
                                        let source = s_read!(source);
                                        let source = &source.source;

                                        let value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];

                                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                                        let read = s_read!(span);
                                        let span = read.start as usize..read.end as usize;

                                        let key = source[span].to_owned();

                                        let (value, _ty) = eval_expression(expr, context, vm)?;
                                        arg_values.push_back(s_read!(value).to_string());

                                        arg_map.insert(key, s_read!(value).to_string());

                                        let next_id = { s_read!(next).next };
                                        if let Some(ref id) = next_id {
                                            next = s_read!(lu_dog).exhume_argument(id).unwrap();
                                        } else {
                                            break;
                                        }
                                    }

                                    arg_values
                                } else {
                                    VecDeque::new()
                                };

                                // 🚧 Oddly, the lhs is the first argument. Not somethnig that I want
                                // or even need to understand atm.
                                arg_values.pop_front();
                                dbg!(&arg_values);

                                enum State {
                                    Normal,
                                    InBrace,
                                }
                                let mut state = State::Normal;
                                let mut result = String::new();
                                let mut current = String::new();
                                for c in string.chars() {
                                    match state {
                                        State::Normal => {
                                            if c == '{' {
                                                state = State::InBrace;
                                            } else {
                                                result.push(c);
                                            }
                                        }
                                        State::InBrace => {
                                            if c == '}' {
                                                if let Ok(index) = current.parse::<usize>() {
                                                    // 🚧 Should check index bounds here.
                                                    let value = arg_values[index].clone();
                                                    result.push_str(&value);
                                                    current.clear();
                                                    state = State::Normal;
                                                // } else if let Some(value) = arg_map.get(&current) {
                                                //     result.push_str(&value);
                                                //     current.clear();
                                                //     state = State::Normal;
                                                } else {
                                                    // 🚧 this is the wrong error
                                                    return Err(ChaChaError::NoSuchMethod {
                                                        method: current.to_owned(),
                                                        span: 0..0,
                                                    });
                                                }
                                            } else {
                                                current.push(c);
                                            }
                                        }
                                    }
                                }

                                let ty = Ty::new_s_string();
                                let ty =
                                    ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));
                                Ok((new_ref!(Value, Value::String(result)), ty))
                            }
                            value_ => {
                                let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                                debug!("value", value);

                                let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                                let read = s_read!(span);
                                let span = read.start as usize..read.end as usize;

                                return Err(ChaChaError::NoSuchMethod {
                                    method: value_.to_owned(),
                                    span,
                                });
                            }
                        },
                        Value::UserType(ut) => {
                            // Below is all wrapped up to avoid a double borrow.
                            let woog_struct = {
                                let ut_read = s_read!(ut);
                                let ty = ut_read.get_type();
                                let ty = s_read!(ty);
                                if let ValueType::WoogStruct(woog_struct) = &*ty {
                                    woog_struct.clone()
                                } else {
                                    // 🚧 This should be an error.
                                    panic!("I'm trying to invoke a function on a UserType, and it's not a Struct!");
                                }
                            };

                            let woog_struct =
                                s_read!(lu_dog).exhume_woog_struct(&woog_struct).unwrap();
                            let woog_struct = s_read!(woog_struct);
                            let impl_ = &woog_struct.r8c_implementation(&s_read!(lu_dog))[0];
                            let x = if let Some(func) = s_read!(&impl_)
                                .r9_function(&s_read!(lu_dog))
                                .iter()
                                .find(|f| s_read!(f).name == *meth)
                            {
                                eval_function_call(
                                    (*func).clone(),
                                    &args,
                                    s_read!(call).arg_check,
                                    context,
                                    vm,
                                )
                            } else {
                                // Should this be an error? I don't think it's likely to happen.
                                // 🚧 Wrong! This will happen frequently and should be an error.
                                // It's can't find method on type.
                                panic!("I can't find the function: `{}` in the impl.", meth);
                            };
                            x
                        }
                        bar => panic!("need to deal with Value {:?}", bar),
                    }
                }
                //
                // StaticMethodCall
                //
                (CallEnum::StaticMethodCall(meth), _, _) => {
                    let meth = s_read!(lu_dog).exhume_static_method_call(meth).unwrap();
                    let call = s_read!(meth).r30_call(&s_read!(lu_dog))[0].clone();
                    let args = s_read!(call).r28_argument(&s_read!(lu_dog));

                    // This is for method call on a store type, and we do it out here so that we
                    // don't have to borrow stack mutably more than once.
                    let mut arg_values = if !args.is_empty() {
                        let mut arg_values = VecDeque::with_capacity(args.len());
                        let mut next = args
                            .iter()
                            .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                            .unwrap()
                            .clone();

                        loop {
                            let expr = s_read!(lu_dog)
                                .exhume_expression(&s_read!(next).expression)
                                .unwrap();
                            let value = eval_expression(expr, context, vm)?;
                            arg_values.push_back(value);

                            let next_id = { s_read!(next).next };
                            if let Some(ref id) = next_id {
                                next = s_read!(lu_dog).exhume_argument(id).unwrap();
                            } else {
                                break;
                            }
                        }

                        arg_values
                    } else {
                        VecDeque::new()
                    };

                    let ty = &s_read!(meth).ty;
                    let func = &s_read!(meth).func;
                    debug!("StaticMethodCall ty", ty);
                    debug!("StaticMethodCall func", func);

                    // This is dirty. Down and dirty...
                    if ty == "Uuid" && func == "new" {
                        let value = Value::Uuid(Uuid::new_v4());
                        let ty = Ty::new_s_uuid();
                        let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();

                        Ok((new_ref!(Value, value), ty))
                    } else if ty == "ComplexEx" {
                        match func.as_str() {
                            "norm_squared" => {
                                let (value, ty) = arg_values.pop_front().unwrap();
                                let thonk = context.memory.get_thonk(0).unwrap();
                                let mut frame = CallFrame::new(0, 0, thonk);
                                vm.push_stack(new_ref!(Value, "norm_squared".into()));
                                vm.push_stack(value);
                                let result = vm.run(&mut frame, false);
                                vm.pop_stack();
                                vm.pop_stack();
                                context.expr_count += 2;

                                Ok((result.unwrap(), ty))
                            }
                            "square" => {
                                let (value, ty) = arg_values.pop_front().unwrap();
                                let thonk = context.memory.get_thonk(2).unwrap();
                                let mut frame = CallFrame::new(0, 0, thonk);
                                vm.push_stack(new_ref!(Value, "square".into()));
                                vm.push_stack(value);
                                let result = vm.run(&mut frame, false);
                                vm.pop_stack();
                                vm.pop_stack();
                                context.expr_count += 5;

                                Ok((result.unwrap(), ty))
                            }
                            "add" => {
                                let thonk = context.memory.get_thonk(1).unwrap();
                                let mut frame = CallFrame::new(0, 0, thonk);
                                vm.push_stack(new_ref!(Value, "add".into()));
                                let (value, _ty) = arg_values.pop_front().unwrap();
                                vm.push_stack(value);
                                let (value, ty) = arg_values.pop_front().unwrap();
                                vm.push_stack(value);
                                let result = vm.run(&mut frame, false);
                                vm.pop_stack();
                                vm.pop_stack();
                                vm.pop_stack();
                                context.expr_count += 2;

                                Ok((result.unwrap(), ty))
                            }
                            method => Err(ChaChaError::NoSuchStaticMethod {
                                ty: ty.to_owned(),
                                method: method.to_owned(),
                            }),
                        }
                    } else if ty == "chacha" {
                        match func.as_str() {
                            "args" => {
                                let ty = Ty::new_s_string();
                                let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();
                                let ty = List::new(&ty, &mut s_write!(lu_dog));
                                let ty = ValueType::new_list(&ty, &mut s_write!(lu_dog));

                                if let Some(args) = &context.args {
                                    Ok((args.clone(), ty))
                                } else {
                                    Ok((new_ref!(Value, Value::Vector(Vec::new())), ty))
                                }
                            }
                            // This returns a string because that's the easy button given what
                            // I have to work with. Once I get enums into the language, I'll
                            // be able to return a proper enum.
                            "typeof" => {
                                let (_arg, ty) = arg_values.pop_front().unwrap();
                                let pvt_ty = PrintableValueType(&ty, &context);
                                let ty = Ty::new_s_string();
                                let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();
                                Ok((new_ref!(Value, pvt_ty.to_string().into()), ty))
                            }
                            "time" => {
                                // 🚧 I should be checking that there is an argument before
                                // I go unwrapping it.
                                let (func, ty) = arg_values.pop_front().unwrap();
                                let func = s_read!(func);
                                ensure!(
                                    if let Value::Function(_) = &*func {
                                        true
                                    } else {
                                        false
                                    },
                                    {
                                        // 🚧 I'm not really sure what to do about this here. It's
                                        // all really a hack for now anyway.
                                        let ty = PrintableValueType(&ty, &context);
                                        TypeMismatchSnafu {
                                            expected: "<function>".to_string(),
                                            got: ty.to_string(),
                                            span: 0..0,
                                        }
                                    }
                                );
                                let func = if let Value::Function(f) = &*func {
                                    f.clone()
                                } else {
                                    unreachable!()
                                };

                                let now = Instant::now();
                                let result = eval_function_call(func, &[], true, context, vm)?;
                                let elapsed = now.elapsed();

                                let time = format!("{:?}\n", elapsed);
                                chacha_print(time, context)?;

                                Ok(result)
                            }
                            "eps" => {
                                let timings = context.timings.iter().cloned().collect::<Vec<_>>();

                                let mean = timings.iter().sum::<f64>() / timings.len() as f64;
                                let std_dev =
                                    timings.iter().map(|x| (x - mean).powi(2)).sum::<f64>()
                                        / timings.len() as f64;
                                let median = timings[timings.len() / 2];

                                let result = format!(
                                    "expressions (10k)/sec (mean/std_dev/median): {:.1} / {:.1} / {:.1}\n",
                                    mean,
                                    std_dev,
                                    median
                                );
                                chacha_print(result, context)?;

                                Ok((
                                    new_ref!(Value, Value::Empty),
                                    ValueType::new_empty(&s_read!(lu_dog)),
                                ))
                            }
                            "assert_eq" => {
                                let lhs = arg_values.pop_front().unwrap().0;
                                let rhs = arg_values.pop_front().unwrap().0;

                                let value = Value::Boolean(*s_read!(lhs) == *s_read!(rhs));

                                if let Value::Boolean(result) = value {
                                    // if value.into() {
                                    if result {
                                        let ty = Ty::new_boolean();
                                        let ty =
                                            s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();

                                        Ok((new_ref!(Value, value), ty))
                                    } else {
                                        let source = s_read!(lu_dog)
                                            .iter_dwarf_source_file()
                                            .next()
                                            .unwrap();
                                        let source = s_read!(source);
                                        let source = &source.source;

                                        let value =
                                            &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];

                                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                                        let read = s_read!(span);
                                        let span = read.start as usize..read.end as usize;

                                        Err(ChaChaError::Assertion {
                                            found: lhs,
                                            expected: rhs,
                                            code: source[span].to_owned(),
                                        })
                                    }
                                } else {
                                    unreachable!()
                                }
                            }
                            // "spawn" => {
                            //     // error!("spawn not implemented");

                            //     let (func, ty) = arg_values.pop_front().unwrap();
                            //     let func = s_read!(func);
                            //     ensure!(
                            //         if let Value::Function(_) = &*func {
                            //             true
                            //         } else {
                            //             false
                            //         },
                            //         {
                            //             // 🚧 I'm not really sure what to do about this here. It's
                            //             // all really a hack for now anyway.
                            //             let ty = PrintableValueType(&ty, &context);
                            //             TypeMismatchSnafu {
                            //                 expected: "<function>".to_string(),
                            //                 got: ty.to_string(),
                            //                 span: 0..0,
                            //             }
                            //         }
                            //     );
                            //     let func = if let Value::Function(f) = &*func {
                            //         f.clone()
                            //     } else {
                            //         unreachable!()
                            //     };

                            //     let (args, ty) = arg_values.pop_front().unwrap();
                            //     let args = s_read!(args);
                            //     ensure!(
                            //         if let Value::Vector(_) = &*args {
                            //             true
                            //         } else {
                            //             false
                            //         },
                            //         {
                            //             // 🚧 I'm not really sure what to do about this here. It's
                            //             // all really a hack for now anyway.
                            //             let ty = PrintableValueType(&ty, &context);
                            //             TypeMismatchSnafu {
                            //                 expected: "Vector".to_string(),
                            //                 got: ty.to_string(),
                            //                 span: 0..0,
                            //             }
                            //         }
                            //     );

                            //     let args = match &*args {
                            //         Value::Vector(v) => {
                            //             let mut stack = Vec::new();
                            //             for arg in v.iter() {
                            //                 stack.push(arg.clone());
                            //             }
                            //             stack
                            //         }
                            //         _ => unreachable!(),
                            //     };

                            //     debug!("spawn", func);
                            //     let mut c_clone = context.clone();
                            //     let handle = thread::spawn(move || {
                            //         eval_function_call2(func, &args, &mut c_clone)
                            //     });
                            //     let handle = ThreadHandle::new(
                            //         std::sync::Arc::new(handle),
                            //         &mut s_write!(lu_dog),
                            //     );
                            //     // let handle = ThreadHandle::new(
                            //     // new_rc!(ThreadHandleType, handle),
                            //     // &mut s_write!(lu_dog),
                            //     // );
                            //     // let handle = ThreadHandle::new(
                            //     //     std::sync::Arc::new(handle),
                            //     //     &mut s_write!(lu_dog),
                            //     // );
                            //     let handle = new_ref!(Value, Value::Thread(handle));

                            //     // debug!("StaticMethodCall meta value", value);
                            //     // debug!("StaticMethodCall meta ty", ty);

                            //     Ok((
                            //         new_ref!(Value, Value::Empty),
                            //         // handle,
                            //         ValueType::new_empty(&s_read!(lu_dog)),
                            //     ))
                            // }
                            method => Err(ChaChaError::NoSuchStaticMethod {
                                ty: ty.to_owned(),
                                method: method.to_owned(),
                            }),
                        }
                    } else if let Some(value) = context.memory.get_meta(ty, func) {
                        debug!("StaticMethodCall meta value", value);
                        match &*s_read!(value) {
                            Value::Function(ref func) => {
                                let func =
                                    s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                                debug!("StaticMethodCall meta func", func);
                                let (value, ty) = eval_function_call(
                                    func,
                                    &args,
                                    s_read!(call).arg_check,
                                    context,
                                    vm,
                                )?;
                                debug!("StaticMethodCall meta value", value);
                                debug!("StaticMethodCall meta ty", ty);
                                Ok((value, ty))
                            }
                            value => {
                                error!("deal with call expression", value);
                                Ok((
                                    new_ref!(Value, Value::Empty),
                                    ValueType::new_empty(&s_read!(lu_dog)),
                                ))
                            }
                        }
                    } else if let Some(value) = context.memory.get(ty) {
                        debug!("StaticMethodCall frame value", value);
                        match &mut *s_write!(value) {
                            Value::Function(ref func) => {
                                let func =
                                    s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                                debug!("StaticMethodCall frame func", func);
                                let (value, ty) = eval_function_call(
                                    func,
                                    &args,
                                    s_read!(call).arg_check,
                                    context,
                                    vm,
                                )?;
                                debug!("StaticMethodCall frame value", value);
                                debug!("StaticMethodCall frame ty", ty);
                                Ok((value, ty))
                            }
                            Value::ProxyType(ut) => {
                                debug!("StaticMethodCall proxy", ut);
                                s_write!(ut).call(
                                    func,
                                    &mut arg_values.iter().map(|v| v.0.clone()).collect(),
                                )
                            }
                            // Value::StoreType(ref mut store_type) => {
                            //     // We should actually know what's behind the curtain, since
                            //     // we requested it with `stack.get(ty)`, above.
                            //     match store_type {
                            //         StoreType::Inflection(ref mut inf) => {
                            //             let args: Vec<Value> = Vec::new();
                            //             inf.call(func, &args)
                            //         }
                            //         _ => Ok((
                            //             Value::Error("make point work".to_owned()),
                            //             ValueType::new_empty(),
                            //         )),
                            //     }
                            // }
                            value => {
                                error!("deal with call expression", value);
                                Ok((
                                    new_ref!(Value, Value::Empty),
                                    ValueType::new_empty(&s_read!(lu_dog)),
                                ))
                            }
                        }
                    } else {
                        ensure!(
                            false,
                            NoSuchStaticMethodSnafu {
                                ty: ty.to_owned(),
                                method: func.to_owned(),
                            }
                        );
                        unimplemented!();
                        // We never will get here.
                        // Ok((
                        //     NewRefType::new_ref_type(Value::Empty)),
                        //     ValueType::new_empty(&s_read!(lu_dog)),
                        // ))
                    }
                }
            };
            if s_read!(call).arg_check {
                s_write!(call).arg_check = false;
            }

            call_result
        }
        Expression::Debugger(_) => {
            debug!("StatementEnum::Debugger");
            let mut running = RUNNING.lock();
            *running = false;
            *STEPPING.lock() = true;
            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        //
        // Error Expression
        //
        // 🚧 This should be looked at as part of  The Great Error Overhaul
        //
        Expression::ErrorExpression(ref error) => {
            let error = s_read!(lu_dog).exhume_error_expression(error).unwrap();

            // 🚧 This isn't going to cut it.
            print!("\t{}", s_read!(error).span);

            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        //
        // FieldAccess
        //
        Expression::FieldAccess(ref field) => {
            let field = s_read!(lu_dog).exhume_field_access(field).unwrap();
            let fat = &s_read!(field).r65_field_access_target(&s_read!(lu_dog))[0];
            let field_name = match *s_read!(fat) {
                FieldAccessTarget::Field(ref field) => {
                    let field = s_read!(lu_dog).exhume_field(field).unwrap();
                    let field = s_read!(field);
                    field.name.to_owned()
                }
                FieldAccessTarget::Function(ref func) => {
                    let func = s_read!(lu_dog).exhume_function(func).unwrap();
                    let func = s_read!(func);
                    func.name.to_owned()
                }
            };

            // debug!("FieldAccess field", field);

            // let field_name = &s_read!(field).name;

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
                    let value = s_read!(value);
                    let value = value.get_attr_value(&field_name)?;
                    let ty = s_read!(value).get_type(&s_read!(lu_dog));

                    Ok((value, ty))
                }
                Value::UserType(value) => {
                    let value = s_read!(value);
                    let value = value.get_attr_value(field_name).unwrap();
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
        //
        // For Loop
        //
        Expression::ForLoop(ref for_loop) => {
            debug!("ForLoop", for_loop);

            let for_loop = s_read!(lu_dog).exhume_for_loop(for_loop).unwrap();
            let for_loop = s_read!(for_loop);
            let ident = for_loop.ident.to_owned();
            let block = s_read!(lu_dog).exhume_block(&for_loop.block).unwrap();
            let list = s_read!(lu_dog)
                .exhume_expression(&for_loop.expression)
                .unwrap();

            let (list, _ty) = eval_expression(list, context, vm)?;
            let list = s_read!(list);
            let list = if let Value::Vector(vec) = list.clone() {
                vec
            } else if let Value::String(str) = &*list {
                str.chars()
                    .map(|c| new_ref!(Value, Value::Char(c)))
                    .collect()
            } else if let Value::Range(range) = &*list {
                let mut vec = Vec::new();
                for i in (&*s_read!(range.start)).try_into()?..(&*s_read!(range.end)).try_into()? {
                    vec.push(new_ref!(Value, Value::Integer(i)));
                }
                vec
            } else {
                return Err(ChaChaError::BadJuJu {
                    message: "For loop expression is not a list".to_owned(),
                    location: location!(),
                });
            };

            let block = Expression::new_block(&block, &mut *s_write!(lu_dog));
            context.memory.push_frame();
            // list.par_iter().for_each(|item| {
            //     // This gives each thread it's own stack frame, and read only
            //     // access to the parent stack frame. I don't know that I love
            //     // this solution. But it's a qucik hack to threading.
            //     let mut stack = context.stack.clone();
            //     stack.insert(ident.clone(), item.clone());
            //     eval_expression(block.clone(), &mut context.clone()).unwrap();
            // });
            for item in list {
                context.memory.insert(ident.clone(), item);
                let expr_ty = eval_expression(block.clone(), context, vm);
                match expr_ty {
                    Ok(_) => {}
                    Err(e) => {
                        context.memory.pop_frame();
                        return Err(e);
                    }
                }
            }
            context.memory.pop_frame();

            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        //
        // Index
        //
        Expression::Index(ref index) => {
            let index = s_read!(lu_dog).exhume_index(index).unwrap();
            let index = s_read!(index);
            let target = s_read!(lu_dog).exhume_expression(&index.target).unwrap();
            let index = s_read!(lu_dog).exhume_expression(&index.index).unwrap();

            let (index, _ty) = eval_expression(index, context, vm)?;
            let index = if let Value::Integer(index) = *s_read!(index) {
                index as usize
            } else {
                return Err(ChaChaError::BadJuJu {
                    message: "Index is not an integer".to_owned(),
                    location: location!(),
                });
            };

            let (list, _ty) = eval_expression(target, context, vm)?;
            let list = s_read!(list);
            if let Value::Vector(vec) = list.clone() {
                if index < vec.len() {
                    Ok((
                        vec[index].to_owned(),
                        ValueType::new_empty(&s_read!(lu_dog)),
                    ))
                } else {
                    Err(ChaChaError::BadJuJu {
                        message: "Index out of bounds".to_owned(),
                        location: location!(),
                    })
                }
            } else if let Value::String(str) = &*list {
                let str = unicode_segmentation::UnicodeSegmentation::graphemes(str.as_str(), true)
                    .collect::<Vec<&str>>();

                if index < str.len() {
                    Ok((
                        new_ref!(Value, Value::String(str[index..index + 1].join(""),)),
                        ValueType::new_empty(&s_read!(lu_dog)),
                    ))
                } else {
                    Err(ChaChaError::BadJuJu {
                        message: "Index out of bounds".to_owned(),
                        location: location!(),
                    })
                }
            } else {
                Err(ChaChaError::BadJuJu {
                    message: "Target is not a list".to_owned(),
                    location: location!(),
                })
            }
        }
        //
        // ListElement
        //
        Expression::ListElement(ref element) => {
            let element = s_read!(lu_dog).exhume_list_element(element).unwrap();
            let element = s_read!(element);
            let expr = element.r55_expression(&s_read!(lu_dog))[0].clone();
            eval_expression(expr, context, vm)
        }
        //
        // ListExpression
        //
        Expression::ListExpression(ref list) => {
            let list = s_read!(lu_dog).exhume_list_expression(list).unwrap();
            let list = s_read!(list);
            if let Some(ref element) = list.elements {
                // This is the first element in the list. We need to give this list
                // a type, and I'm going to do the easy thing here and take the type
                // to be whatever the first element evaluates to be. We'll then check
                // each subsequent element to see if it can be cast into the type
                // of the first element.
                //
                // 🚧 Actually do the type checking mentioned above.
                //
                // I'm now not so sure that I need to do all this run-time type checking.
                // I mean, I'm doing it in the compiler, right? This would be a systemic
                // change I think. But I still need the type when I return from here.
                // So maybe it's just a loosening of the rules -- rules that I'm probably
                // not implementing now anyway.
                let element = s_read!(lu_dog).exhume_list_element(element).unwrap();
                let element = s_read!(element);
                let expr = element.r15_expression(&s_read!(lu_dog))[0].clone();
                let (value, ty) = eval_expression(expr, context, vm)?;
                let mut values = vec![value];

                let mut next = element.next;
                while let Some(ref id) = next {
                    let element = s_read!(lu_dog).exhume_list_element(id).unwrap();
                    let element = s_read!(element);
                    let expr = element.r15_expression(&s_read!(lu_dog))[0].clone();
                    let (value, _ty) = eval_expression(expr, context, vm)?;
                    values.push(value);
                    next = element.next;
                }

                let list = List::new(&ty, &mut s_write!(lu_dog));

                Ok((
                    new_ref!(Value, Value::Vector(values)),
                    ValueType::new_list(&list, &mut s_write!(lu_dog)),
                ))
            } else {
                let list = List::new(
                    &ValueType::new_empty(&mut s_write!(lu_dog)),
                    &mut s_write!(lu_dog),
                );

                Ok((
                    new_ref!(Value, Value::Vector(vec![new_ref!(Value, Value::Empty),])),
                    ValueType::new_list(&list, &mut s_write!(lu_dog)),
                ))
            }
        }
        //
        // Literal
        //
        Expression::Literal(ref literal) => {
            let literal = s_read!(lu_dog).exhume_literal(literal).unwrap();
            let z = match &*s_read!(literal) {
                //
                // BooleanLiteral
                //
                Literal::BooleanLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_boolean_literal(literal).unwrap();
                    let literal = s_read!(literal);
                    let ty = Ty::new_boolean();
                    let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();

                    match *literal {
                        BooleanLiteral::FalseLiteral(_) => {
                            Ok((new_ref!(Value, Value::Boolean(false,)), ty))
                        }
                        BooleanLiteral::TrueLiteral(_) => {
                            Ok((new_ref!(Value, Value::Boolean(true,)), ty))
                        }
                    }
                }
                //
                // FloatLiteral
                //
                Literal::FloatLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_float_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Float(value);
                    let ty = Ty::new_float();
                    debug!("ty: {:?}", ty);
                    let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();
                    debug!("ty: {:?}", ty);

                    Ok((new_ref!(Value, value), ty))
                }
                //
                // IntegerLiteral
                //
                Literal::IntegerLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_integer_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Integer(value);
                    let ty = Ty::new_integer();
                    debug!("ty: {:?}", ty);
                    let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();
                    debug!("ty: {:?}", ty);

                    Ok((new_ref!(Value, value), ty))
                }
                //
                // StringLiteral
                //
                Literal::StringLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_string_literal(literal).unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value = Value::String(s_read!(literal).x_value.clone());
                    let ty = Ty::new_s_string();
                    let ty = s_read!(lu_dog).exhume_value_type(&ty.id()).unwrap();
                    Ok((new_ref!(Value, value), ty))
                }
            };
            z
        }
        //
        // Operator
        //
        Expression::Operator(ref operator) => {
            let operator = s_read!(lu_dog).exhume_operator(operator).unwrap();
            let operator = s_read!(operator);
            let lhs_expr = s_read!(lu_dog).exhume_expression(&operator.lhs).unwrap();

            // let (lhs, lhs_ty) = eval_expression(lhs_expr.clone(), context, vm)?;
            // let rhs = if let Some(ref rhs) = operator.rhs {
            //     let rhs = s_read!(lu_dog).exhume_expression(rhs).unwrap();
            //     Some(eval_expression(rhs, context, vm)?)
            // } else {
            //     None
            // };

            debug!("operator", operator);

            match &operator.subtype {
                OperatorEnum::Binary(ref binary) => {
                    let binary = s_read!(lu_dog).exhume_binary(binary).unwrap();
                    let binary = s_read!(binary);

                    match &*binary {
                        Binary::Addition(_) => {
                            let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                            let rhs = {
                                let rhs = operator.rhs.unwrap();
                                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                eval_expression(rhs, context, vm)?
                            };
                            let value = s_read!(lhs).clone() + s_read!(rhs.0).clone();
                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                        Binary::Assignment(_) => {
                            // Type checking has already been handled by the compiler.
                            match &*s_read!(lhs_expr) {
                                // 🚧 I'm sort of duplicating work here. It's not exactly the same
                                // as the general expression handling code, but I think it's close
                                // enough that I could make it work if I wanted to. And so I should.
                                //
                                // Hm. I've already processed the lhs above, and I'm basically doing
                                // it again here.
                                Expression::VariableExpression(expr) => {
                                    let rhs = {
                                        let rhs = operator.rhs.unwrap();
                                        let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                        eval_expression(rhs, context, vm)?
                                    };
                                    let expr =
                                        s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
                                    let expr = s_read!(expr);
                                    let name = expr.name.clone();
                                    let value = context.memory.get(&name).unwrap();
                                    let mut value = s_write!(value);
                                    *value = s_read!(rhs.0).clone();
                                    Ok(rhs)
                                }
                                Expression::FieldAccess(field) => {
                                    let rhs = {
                                        let rhs = operator.rhs.unwrap();
                                        let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                        eval_expression(rhs, context, vm)?
                                    };
                                    let field = s_read!(lu_dog).exhume_field_access(field).unwrap();
                                    let fat = &s_read!(field)
                                        .r65_field_access_target(&s_read!(lu_dog))[0];
                                    let field_name = match *s_read!(fat) {
                                        FieldAccessTarget::Field(ref field) => {
                                            let field =
                                                s_read!(lu_dog).exhume_field(field).unwrap();
                                            let field = s_read!(field);
                                            field.name.to_owned()
                                        }
                                        FieldAccessTarget::Function(_) => {
                                            return Err(ChaChaError::BadJuJu {
                                                message: "Attempt to assign to function".to_owned(),
                                                location: location!(),
                                            })
                                        }
                                    };

                                    let expr = &s_read!(field).expression;
                                    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();

                                    let Expression::VariableExpression(expr) = &*s_read!(expr)
                                    else { unreachable!() };
                                    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                                    let expr = s_read!(lu_dog)
                                        .exhume_variable_expression(&(&*s_read!(expr)).id())
                                        .unwrap();

                                    let value = context.memory.get(&s_read!(expr).name);
                                    ensure!(value.is_some(), {
                                        let var = s_read!(expr).name.clone();
                                        VariableNotFoundSnafu { var }
                                    });

                                    let value = value.unwrap();

                                    match &*s_read!(value) {
                                        Value::ProxyType(_value) => {
                                            // dbg!(s_read!(value));
                                            // s_write!(value).set_attr_value(&field_name, rhs.0)?;
                                            // Ok(rhs)
                                        }
                                        Value::UserType(value) => {
                                            // dbg!(s_read!(value));
                                            s_write!(value).set_attr_value(&field_name, rhs.0);
                                            // Ok(rhs)
                                        }
                                        // 🚧 This needs it's own error. Lazy me.
                                        _value => {
                                            return Err(ChaChaError::BadJuJu {
                                                message: "Attempt to assign to non-struct"
                                                    .to_owned(),
                                                location: location!(),
                                            })
                                        }
                                    }

                                    // This is the LHS.
                                    // The "other" LHS? It's
                                    // let (value, ty) = eval_expression(expr, context, vm)?;
                                    // debug!("field access fubar", value);
                                    // let value_read = s_read!(value).clone();
                                    // match &value_read {
                                    //     // match &*s_read!(lhs) {
                                    //     Value::ProxyType(lhs) => {
                                    //         s_write!(lhs).set_attr_value(&field_name, rhs.0)?;
                                    //         Ok((value, ty))
                                    //     }
                                    //     Value::UserType(lhs) => {
                                    //         s_write!(lhs).set_attr_value(&field_name, rhs.0);
                                    //         Ok((value, ty))
                                    //     }
                                    //     // 🚧 This needs it's own error. Lazy me.
                                    //     value => {
                                    //         return Err(ChaChaError::BadJuJu {
                                    //             message: format!(
                                    //                 "Bad value ({:?}) in field access",
                                    //                 value
                                    //             ),

                                    //             location: location!(),
                                    //         })
                                    //     }
                                    // }
                                    Ok((
                                        new_ref!(Value, Value::Empty),
                                        ValueType::new_empty(&s_read!(lu_dog)),
                                    ))
                                }
                                _ => {
                                    return Err(ChaChaError::BadJuJu {
                                        message: "Bad LHS in assignment".to_owned(),
                                        location: location!(),
                                    })
                                }
                            }

                            // Ok((lhs, lhs_ty))
                        }
                        Binary::BooleanOperator(ref id) => {
                            let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                            let rhs = {
                                let rhs = operator.rhs.unwrap();
                                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                eval_expression(rhs, context, vm)?
                            };
                            let boolean_operator =
                                s_read!(lu_dog).exhume_boolean_operator(id).unwrap();
                            let boolean_operator = s_read!(boolean_operator);
                            match &*boolean_operator {
                                BooleanOperator::And(_) => {
                                    let value = Value::Boolean(
                                        (&*s_read!(lhs)).try_into().unwrap()
                                            && (&*s_read!(rhs.0)).try_into().unwrap(),
                                    );
                                    Ok((new_ref!(Value, value), lhs_ty))
                                } // BooleanOperator::Or(_) => {
                                  //     let value =
                                  //         s_read!(lhs).clone() || s_read!(rhs.unwrap()).clone();
                                  //     Ok((new_ref!(Value, value), lhs_ty))
                                  // }
                            }
                        }
                        Binary::Division(_) => {
                            let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                            let rhs = {
                                let rhs = operator.rhs.unwrap();
                                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                eval_expression(rhs, context, vm)?
                            };
                            let value = s_read!(lhs).clone() / s_read!(rhs.0).clone();
                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                        Binary::Subtraction(_) => {
                            let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                            let rhs = {
                                let rhs = operator.rhs.unwrap();
                                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                eval_expression(rhs, context, vm)?
                            };
                            let value = s_read!(lhs).clone() - s_read!(rhs.0).clone();
                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                        Binary::Multiplication(_) => {
                            let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                            let rhs = {
                                let rhs = operator.rhs.unwrap();
                                let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                                eval_expression(rhs, context, vm)?
                            };
                            let value = s_read!(lhs).clone() * s_read!(rhs.0).clone();
                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                    }
                }
                OperatorEnum::Comparison(ref comp) => {
                    let (lhs, _lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                    let rhs = {
                        let rhs = operator.rhs.unwrap();
                        let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();
                        eval_expression(rhs, context, vm)?
                    };
                    let comp = s_read!(lu_dog).exhume_comparison(comp).unwrap();
                    let comp = s_read!(comp);
                    match &*comp {
                        Comparison::Equal(_) => {
                            let value = *s_read!(lhs) == *s_read!(rhs.0);
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));

                            Ok((new_ref!(Value, value), ty))
                        }
                        Comparison::GreaterThan(_) => {
                            let value = s_read!(lhs).gt(&s_read!(rhs.0));
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));

                            Ok((new_ref!(Value, value), ty))
                        }
                        Comparison::GreaterThanOrEqual(_) => {
                            let value = s_read!(lhs).gte(&s_read!(rhs.0));
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));

                            Ok((new_ref!(Value, value), ty))
                        }
                        Comparison::LessThanOrEqual(_) => {
                            let value = s_read!(lhs).lte(&s_read!(rhs.0));
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(&new_ref!(Ty, ty), &mut s_write!(lu_dog));

                            Ok((new_ref!(Value, value), ty))
                        }
                        _ => unimplemented!(),
                    }
                }
                OperatorEnum::Unary(ref unary) => {
                    let (lhs, lhs_ty) = eval_expression(lhs_expr, context, vm)?;
                    let unary = s_read!(lu_dog).exhume_unary(unary).unwrap();
                    let unary = s_read!(unary);
                    match &*unary {
                        //
                        // Negation
                        //
                        Unary::Negation(_) => {
                            let value = -s_read!(lhs).clone();

                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                        Unary::Not(_) => {
                            let value = !s_read!(lhs).clone();

                            Ok((new_ref!(Value, value), lhs_ty))
                        }
                    }
                }
            }
        }
        //
        // Print
        //
        Expression::Print(ref print) => {
            let print = s_read!(lu_dog).exhume_print(print).unwrap();
            debug!("Expression::Print print", print);
            let expr = s_read!(print).r32_expression(&s_read!(lu_dog))[0].clone();
            let (value, _) = eval_expression(expr, context, vm)?;
            let result = format!("{}", s_read!(value));
            let result = result.replace("\\n", "\n");

            chacha_print(result, context)?;

            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        //
        // Range
        //
        Expression::RangeExpression(ref range) => {
            let range = s_read!(lu_dog).exhume_range_expression(range).unwrap();
            let lhs = s_read!(range).lhs.unwrap();
            let lhs = s_read!(lu_dog).exhume_expression(&lhs).unwrap();
            let rhs = s_read!(range).rhs.unwrap();
            let rhs = s_read!(lu_dog).exhume_expression(&rhs).unwrap();

            let (lhs, _) = eval_expression(lhs, context, vm)?;
            let (rhs, _) = eval_expression(rhs, context, vm)?;

            let range = Range {
                start: Box::new(lhs),
                end: Box::new(rhs),
            };

            Ok((
                new_ref!(Value, Value::Range(range)),
                ValueType::new_range(&s_read!(lu_dog)),
            ))
        }
        //
        // StructExpression
        //
        // 🚧  This creates `UserType`s, but what about `ProxyType`s? I sort of
        // think that the latter is only for imported, but I'm not sure.
        //
        Expression::StructExpression(ref expr) => {
            let expr = s_read!(lu_dog).exhume_struct_expression(expr).unwrap();
            let field_exprs = s_read!(expr).r26_field_expression(&s_read!(lu_dog));

            // Get name, value and type for each field expression.
            let field_exprs = field_exprs
                .iter()
                .map(|f| {
                    let expr = s_read!(lu_dog)
                        .exhume_expression(&s_read!(f).expression)
                        .unwrap();
                    let (value, ty) = eval_expression(expr.clone(), context, vm)?;
                    debug!("StructExpression field value", value);
                    debug!("StructExpression field ty", ty);
                    Ok((s_read!(f).name.clone(), ty, value, expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let woog_struct = s_read!(expr).r39_woog_struct(&s_read!(lu_dog))[0].clone();
            let ty = s_read!(lu_dog)
                .exhume_value_type(&s_read!(woog_struct).id)
                .unwrap();
            let fields = s_read!(woog_struct).r7_field(&s_read!(lu_dog));

            // Type checking fields here
            let ty_name = PrintableValueType(&ty, &context);
            let mut user_type = UserType::new(ty_name.to_string(), &ty);
            let lu_dog = s_read!(lu_dog);
            for (name, ty, value, expr) in field_exprs {
                if let Some(field) = fields.iter().find(|f| s_read!(f).name == name) {
                    let struct_ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();

                    let x_value = &s_read!(expr).r11_x_value(&lu_dog)[0];
                    let span = &s_read!(x_value).r63_span(&lu_dog)[0];

                    typecheck(&struct_ty, &ty, span, context)?;

                    // This is where we add the attribute value to the user type.
                    user_type.add_attr(&name, value);
                } else {
                    ensure!(
                        false,
                        NoSuchFieldSnafu {
                            field: name.to_owned(),
                        }
                    );
                }
            }

            Ok((
                new_ref!(Value, Value::UserType(new_ref!(UserType, user_type))),
                ty,
            ))
        }
        //
        // TypeCast
        //
        Expression::TypeCast(ref expr) => {
            let sarzak = context.sarzak.clone();

            let expr = s_read!(lu_dog).exhume_type_cast(expr).unwrap();
            debug!("Expression::TypeCast", expr);

            let lhs = s_read!(expr).r68_expression(&s_read!(lu_dog))[0].clone();
            let as_ty = s_read!(expr).r69_value_type(&s_read!(lu_dog))[0].clone();

            let (lhs, _lhs_ty) = eval_expression(lhs, context, vm)?;

            let value = match &*s_read!(as_ty) {
                ValueType::Ty(ref ty) => {
                    let ty = s_read!(sarzak).exhume_ty(ty).unwrap().clone();
                    match ty {
                        Ty::Float(_) => {
                            let value: f64 = (&*s_read!(lhs)).try_into()?;
                            new_ref!(Value, value.into())
                        }
                        Ty::Integer(_) => {
                            let value: i64 = (&*s_read!(lhs)).try_into()?;
                            new_ref!(Value, value.into())
                        }
                        Ty::SString(_) => {
                            let value: String = (&*s_read!(lhs)).try_into()?;
                            new_ref!(Value, value.into())
                        }
                        ref alpha => {
                            ensure!(
                                false,
                                UnimplementedSnafu {
                                    message: format!("deal with type cast as: {:?}", alpha),
                                }
                            );
                            unreachable!();
                        }
                    }
                }
                ref alpha => {
                    ensure!(
                        false,
                        UnimplementedSnafu {
                            message: format!("deal with type cast as: {:?}", alpha),
                        }
                    );
                    unreachable!();
                }
            };

            Ok((value, as_ty))
        }
        //
        // VariableExpression
        //
        Expression::VariableExpression(ref expr) => {
            let expr = s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
            debug!("Expression::VariableExpression", expr);
            let value = context.memory.get(&s_read!(expr).name);

            ensure!(value.is_some(), {
                let var = s_read!(expr).name.clone();
                VariableNotFoundSnafu { var }
            });

            let value = value.unwrap();

            debug!("Expression::VariableExpression value", s_read!(value));

            // 🚧 These statements were swapped, comment-status-wise. I thought
            // it was because of a deadlock, or trying to mutably borrow whilst
            // already borrowed error. Anyway, I needed the type, and swapped them
            // to how they are now and it seems to be working. I'm just leaving
            // this for my future self, just in case.
            let ty = s_read!(value).get_type(&s_read!(lu_dog));
            // let ty = ValueType::new_empty(&s_read!(lu_dog));

            Ok((value.clone(), ty))
        }
        //
        // XIf
        //
        Expression::XIf(ref expr) => {
            let expr = s_read!(lu_dog).exhume_x_if(expr).unwrap();
            let expr = s_read!(expr);
            debug!("Expression::XIf", expr);

            let cond_expr = s_read!(lu_dog).exhume_expression(&expr.test).unwrap();

            let (cond, _ty) = eval_expression(cond_expr, context, vm)?;
            debug!("Expression::XIf conditional", cond);

            let cond = s_read!(cond);
            Ok(if (&*cond).try_into()? {
                // Evaluate the true block
                let block = s_read!(lu_dog).exhume_expression(&expr.true_block).unwrap();
                eval_expression(block, context, vm)?
            } else {
                debug!("Expression::XIf else");
                if let Some(expr) = &expr.false_block {
                    debug!("Expression::XIf false block");
                    // Evaluate the false block
                    let block = s_read!(lu_dog).exhume_expression(expr).unwrap();
                    eval_expression(block, context, vm)?
                } else {
                    (
                        new_ref!(Value, Value::Empty),
                        ValueType::new_empty(&s_read!(lu_dog)),
                    )
                }
            })
        }
        //
        // XReturn
        //
        Expression::XReturn(ref expr) => {
            let expr = s_read!(lu_dog).exhume_x_return(expr).unwrap();
            debug!("Expression::XReturn", expr);

            let expr = &s_read!(expr).expression;
            let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();

            let (value, ty) = eval_expression(expr, context, vm)?;
            Err(ChaChaError::Return {
                value: value,
                ty: ty,
            })
        }
        //
        // ZNone
        //
        Expression::ZNone(_) => Ok((
            new_ref!(Value, Value::Empty),
            ValueType::new_empty(&s_read!(lu_dog)),
        )),
        //
        // ZSome
        //
        Expression::ZSome(ref some) => {
            let some = s_read!(lu_dog).exhume_z_some(some).unwrap();
            debug!("Expression::ZSome", some);

            let value = &s_read!(some).r23_x_value(&s_read!(lu_dog))[0];
            let option = &s_read!(some).r3_woog_option(&s_read!(lu_dog))[0];
            let ty = &s_read!(option).r2_value_type(&s_read!(lu_dog))[0];

            let value = match s_read!(value).subtype {
                XValueEnum::Expression(ref expr) => {
                    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                    let (value, _ty) = eval_expression(expr, context, vm)?;
                    value
                }
                XValueEnum::Variable(ref var) => {
                    let _var = s_read!(lu_dog).exhume_variable(var).unwrap();
                    new_ref!(Value, Value::Empty)
                }
            };

            Ok((value, ty.clone()))
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("deal with expression: {:?}", alpha),
                }
            );

            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
    }
}

pub fn eval_statement(
    statement: RefType<Statement>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog.clone();

    debug!("eval_statement statement", statement);
    trace!("eval_statement stack", context.memory);

    span!("eval_statement");

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&s_read!(lu_dog))[0].clone();
            let (value, ty) = eval_expression(expr, context, vm)?;
            no_debug!("StatementEnum::ExpressionStatement: value", s_read!(value));
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&s_read!(lu_dog))[0].clone();
            debug!("expr", expr);

            let (value, ty) = eval_expression(expr, context, vm)?;
            debug!("value", value);
            debug!("ty", ty);

            let var = s_read!(stmt.r21_local_variable(&s_read!(lu_dog))[0]).clone();
            let var = s_read!(var.r12_variable(&s_read!(lu_dog))[0]).clone();
            debug!("var", var);

            log::debug!("inserting {} = {}", var.name, s_read!(value));
            context.memory.insert(var.name, value);

            // 🚧 I'm changing this from returning ty. If something get's wonky,
            // maybe start looking here. But TBH, why would we return the type of
            // the storage?
            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::ResultStatement: stmt", stmt);

            let expr = stmt.r41_expression(&s_read!(lu_dog))[0].clone();
            debug!("StatementEnum::ResultStatement expr", expr);

            let (value, ty) = eval_expression(expr, context, vm)?;
            debug!("StatementEnum::ResultStatement value", value);
            debug!("StatementEnum::ResultStatement ty", ty);

            Ok((value, ty))
        }
        ref beta => {
            error!("deal with statement", beta);
            Ok((
                new_ref!(Value, Value::Empty),
                ValueType::new_empty(&s_read!(lu_dog)),
            ))
        }
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    /// The prompt to display in the REPL
    prompt: String,
    /// The root block, used by the REPL
    block: RefType<Block>,
    memory: Memory,
    lu_dog: RefType<LuDogStore>,
    sarzak: RefType<SarzakStore>,
    models: RefType<Vec<SarzakStore>>,
    mem_update_recv: Receiver<MemoryUpdateMessage>,
    #[allow(dead_code)]
    std_out_send: Sender<String>,
    std_out_recv: Receiver<String>,
    debug_status_writer: Option<Sender<DebuggerStatus>>,
    // obj_file_path: Option<PathBuf>,
    timings: CircularQueue<f64>,
    expr_count: usize,
    func_calls: usize,
    args: Option<RefType<Value>>,
}

/// Save the lu_dog model when the context is dropped
///
/// NB: This doesn't work. The thread that started us apparently goes away
/// before we get a chance to run this to completion. That's my current
/// working hypothesis.
///
/// Shouldn't this work if we are joining the threads? Maybe I wasn't doing that?
/// Do I still need this?
/// I do if we want to save the model on exit.
impl Drop for Context {
    fn drop(&mut self) {
        // s_read!(self.lu_dog)
        //     .unwrap()
        //     .persist_bincode(&self.obj_file_path)
        //     .unwrap();
    }
}

impl Context {
    pub fn add_args(&mut self, args: Vec<String>) {
        self.args = Some(new_ref!(Value, args.into()));
    }

    pub fn register_model<P: AsRef<Path>>(&self, model_path: P) -> Result<()> {
        let model =
            SarzakStore::load(model_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

        s_write!(self.models).push(model);

        Ok(())
    }

    pub fn register_memory_updates(&self) -> Receiver<MemoryUpdateMessage> {
        self.mem_update_recv.clone()
    }

    pub fn drain_std_out(&self) -> Vec<String> {
        let mut out = Vec::new();
        while let Ok(line) = self.std_out_recv.try_recv() {
            out.push(line);
        }
        out
    }

    pub fn get_stack(&self) -> &Memory {
        &self.memory
    }

    pub fn prompt(&self) -> &str {
        &self.prompt
    }

    pub fn source(&self) -> String {
        let source = s_read!(self.lu_dog)
            .iter_dwarf_source_file()
            .next()
            .unwrap();
        let source = s_read!(source);
        source.source.clone()
    }

    pub fn lu_dog_heel(&self) -> RefType<LuDogStore> {
        self.lu_dog.clone()
    }

    pub fn block(&self) -> RefType<Block> {
        self.block.clone()
    }

    pub fn sarzak_heel(&self) -> RefType<SarzakStore> {
        self.sarzak.clone()
    }

    pub fn models(&self) -> RefType<Vec<SarzakStore>> {
        self.models.clone()
    }

    pub fn register_store_proxy(&mut self, name: String, proxy: impl StoreProxy + 'static) {
        self.memory.insert_global(
            name.clone(),
            new_ref!(
                Value,
                Value::ProxyType(new_ref!(Box<dyn StoreProxy>, Box::new(proxy)))
            ),
        );

        let mut lu_dog = s_write!(self.lu_dog);
        let local = LocalVariable::new(Uuid::new_v4(), &mut *lu_dog);
        let var = Variable::new_local_variable(name.clone(), &local, &mut *lu_dog);
        let import = Import::new(
            "So ugly".to_owned(),
            false,
            name,
            "path".to_owned(),
            None,
            &mut *lu_dog,
        );

        let _value = XValue::new_variable(
            &self.block,
            &ValueType::new_import(&import, &mut *lu_dog),
            &var,
            &mut *lu_dog,
        );
        // {
        //     // Build the ASTs
        //     let local = LocalVariable::new(Uuid::new_v4(), &mut *s_write!(lu_dog));
        //     let var = Variable::new_local_variable(
        //         "MERLIN_STORE".to_owned(),
        //         local,
        //         &mut *s_write!(lu_dog),
        //     );

        //     let store = ZObjectStore::new("merlin".to_owned(), &mut *s_write!(lu_dog));
        //     let mut write = s_write!(lu_dog);
        //     let _value = LuDogValue::new_variable(
        //         block.clone(),
        //         ValueType::new_z_object_store(store, &mut write),
        //         var,
        //         &mut write,
        //     );
        // }
    }
}

#[derive(Debug)]
pub enum DebuggerStatus {
    Error(String),
    Paused(Range<usize>),
    Running,
    StdOut(String),
    Stopped(RefType<Value>, RefType<ValueType>),
}

pub enum DebuggerControl {
    ExecuteInput(String),
    Run,
    SetBreakpoint(usize),
    StepInto,
    StepOver,
    Stop,
}

#[cfg(not(feature = "single"))]
pub fn start_tui_repl(mut context: Context) -> (Sender<DebuggerControl>, Receiver<DebuggerStatus>) {
    use std::time::Duration;

    use crossbeam::channel::RecvTimeoutError;

    let (to_ui_write, to_ui_read) = unbounded();
    let (from_ui_write, from_ui_read) = unbounded();
    // let (from_worker_write, from_worker_read) = unbounded();
    let (to_worker_write, to_worker_read) = unbounded();

    context.debug_status_writer = Some(to_ui_write.clone());
    let std_out = context.std_out_recv.clone();

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
                    debug!("Setting breakpoint at character {}", character);
                }
                Ok(DebuggerControl::ExecuteInput(input)) => {
                    debug!("Executing input: {}", input);
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
    // This guy listens for statemntes and executes them. It relies on the state
    // of the condition variable and mutexes to know how to behave.
    thread::Builder::new()
        .name("worker".into())
        // .stack_size(128 * 1024)
        .spawn(move || {
            let stack = &mut context.memory;
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
                                    &DwarfSourceFile::new(input, &mut lu_dog),
                                    &block,
                                    &mut lu_dog,
                                    &s_read!(models),
                                    &s_read!(sarzak),
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

                            match eval_statement(stmt, &mut context, &mut vm) {
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
                            to_ui_write
                                .send(DebuggerStatus::Error(format!("{}", e)))
                                .unwrap();
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

/// This runs the main function, assuming it exists.
pub fn start_main(stopped: bool, mut context: Context) -> Result<(Value, Context), Error> {
    {
        let mut running = RUNNING.lock();
        *running = !stopped;
    }

    let stack = &mut context.memory;
    let vm_stack = stack.clone();
    let mut vm = VM::new(&vm_stack);

    let main = stack.get("main").expect("Missing main function.");

    // This should fail if it's not a function. Actually, I think that it _has_
    // to be a function. Unless there's another named item that I'm not thinking
    // of. I mean, maybe `use main;`  would trigger this to return OK(()), and
    // not do anything?
    let result = if let Value::Function(ref main) = *s_read!(main) {
        let main = s_read!(context.lu_dog)
            .exhume_function(&s_read!(main).id)
            .unwrap();

        let result = eval_function_call(main, &[], true, &mut context, &mut vm)?;

        Ok((s_read!(result.0.clone()).clone(), context))
    } else {
        Err(Error(ChaChaError::MainIsNotAFunction))
    };

    result
}

pub fn start_vm(n: DwarfInteger) -> Result<DwarfInteger, Error> {
    let (mut memory, _) = Memory::new();
    let mut thonk = Thonk::new("fib".to_string());

    // Get the parameter off the stack
    // push {fp + 0}
    thonk.add_instruction(Instruction::PushLocal(0));
    // push 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
    // Chcek if it's <= 1
    // lte
    thonk.add_instruction(Instruction::LessThanOrEqual);
    // jne
    thonk.add_instruction(Instruction::JumpIfFalse(2));
    // If false return 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
    thonk.add_instruction(Instruction::Return);
    // return fidbn-1) + fib(n-2)
    // Load fib
    thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Thonk("fib", 0))));
    // load n
    thonk.add_instruction(Instruction::PushLocal(0));
    // load 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
    // subtract
    thonk.add_instruction(Instruction::Subtract);
    // Call fib(n-1)
    thonk.add_instruction(Instruction::Call(1));
    // load fib
    thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Thonk("fib", 0))));
    // load n
    thonk.add_instruction(Instruction::PushLocal(0));
    // load 2
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 2.into())));
    // subtract
    thonk.add_instruction(Instruction::Subtract);
    // Call fib(n-1)
    thonk.add_instruction(Instruction::Call(1));
    // add
    thonk.add_instruction(Instruction::Add);
    thonk.add_instruction(Instruction::Return);

    // put fib in memory
    let slot = memory.reserve_thonk_slot();
    memory.insert_thonk(thonk.clone(), slot);

    let mut frame = CallFrame::new(0, 0, &thonk);

    let mut vm = VM::new(&memory);

    // Push the func
    vm.push_stack(new_ref!(Value, "fib".into()));
    // Push the argument
    vm.push_stack(new_ref!(Value, Value::Integer(n)));

    // vm.push_frame(frame);

    let result = vm.run(&mut frame, false);

    vm.pop_stack();
    vm.pop_stack();

    let result: DwarfInteger = (&*s_read!(result.unwrap())).try_into().unwrap();

    Ok(result)
}

#[cfg(feature = "repl")]
pub fn start_repl(mut context: Context) -> Result<(), Error> {
    use std::io;

    use rustyline::error::ReadlineError;
    use rustyline::validate::{ValidationContext, ValidationResult, Validator};
    use rustyline::{Completer, Helper, Highlighter, Hinter};
    use rustyline::{Editor, Result};

    let models = context.models.clone();
    let lu_dog = context.lu_dog.clone();
    let sarzak = context.sarzak.clone();

    let block = context.block.clone();
    // let stack = &mut context.stack;

    let vm_stack = context.memory.clone();
    let mut vm = VM::new(&vm_stack);

    let error_style = Colour::Red;
    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let type_style = Colour::Blue.italic().dimmed();

    #[derive(Completer, Helper, Highlighter, Hinter)]
    struct DwarfValidator {}

    impl DwarfValidator {
        fn validate(&self, _input: &str) -> ValidationResult {
            ValidationResult::Valid(None)
            // if let Some((stmt, _span)) = parse_line(input) {
            // ValidationResult::Valid(None)
            // } else {
            // ValidationResult::Incomplete
            // }
        }
    }

    impl Validator for DwarfValidator {
        fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult> {
            Ok(self.validate(ctx.input()))
        }
    }

    //
    // let some_value = format!("{:b}", 42);
    // let strings: &[ANSIString<'static>] = &[
    //     Red.paint("["),
    //     Red.bold().paint(some_value),
    //     Red.paint("]"),
    // ];
    //
    // println!("Value: {}", ANSIStrings(strings));

    banner();

    // `()` can be used when no completer is required
    let mut rl = Editor::new().map_err(|e| ChaChaError::RustyLine { source: e })?;
    let v = DwarfValidator {};
    rl.set_helper(Some(v));

    // #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    let reader = context.std_out_recv.clone();
    let handle = thread::spawn(move || loop {
        match reader.recv() {
            Ok(line) => {
                print!("{}", line);
                io::stdout().flush().unwrap();
            }
            Err(_) => {
                debug!("Debugger control thread exiting");
                break;
            }
        };
    });

    loop {
        let readline = rl.readline(&format!("{} ", prompt_style.paint("道:>")));
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .map_err(|e| ChaChaError::RustyLine { source: e })?;

                // Should do a regex here, or something.
                if line == "@logo" {
                    banner();
                } else if let Ok(Some((stmt, _span))) = parse_line(&line) {
                    debug!("stmt from readline", stmt);

                    let stmt = {
                        let mut lu_dog = s_write!(lu_dog);
                        match inter_statement(
                            &new_ref!(crate::dwarf::Statement, stmt),
                            &DwarfSourceFile::new(line.clone(), &mut lu_dog),
                            &block,
                            &mut lu_dog,
                            &s_read!(models),
                            &s_read!(sarzak),
                        ) {
                            Ok(stmt) => stmt.0,
                            Err(e) => {
                                println!("{}", e);
                                continue;
                            }
                        }
                    };

                    // 🚧 This needs fixing too.
                    let eval = eval_statement(stmt, &mut context, &mut vm);
                    // for i in context.drain_std_out() {
                    //     println!("{}", i);
                    // }
                    match eval {
                        Ok((value, ty)) => {
                            let value = format!("{}", s_read!(value));
                            print!("\n'{}'", result_style.paint(value));

                            let ty = PrintableValueType(&ty, &context);
                            let ty = format!("{}", ty);
                            println!("\t  ──➤  {}", type_style.paint(ty));
                        }
                        Err(e) => {
                            println!("{}", e);
                            if let ChaChaError::Return { value: _, ty: _ } = e {
                                println!("👋 Bye bye!");
                                break;
                            }
                        }
                    }
                } else {
                    println!("{}", error_style.paint("WTF?"));
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("👋 Bye bye!");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    // #[cfg(feature = "with-file-history")]
    rl.save_history("history.txt")
        .map_err(|e| ChaChaError::RustyLine { source: e })?;

    handle.join().unwrap();

    Ok(())
}

/// Display the banner
pub fn banner() -> String {
    let strings: &[ANSIString<'static>] = &[
        RGB(255, 184, 0).paint(
            r"
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣄⠀⢠⣾⣆⠀⣠⣶⡀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⣼⣿⣦⣼⣿⣿⣷⣿⣿⣿⣶⣿⣿⣧⣤⣾⣿⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣷⣶⣿⣿⣿⣿⣿⣿⣿⡏⠀⢘⣿⣿⣿⣿⣿⣿⣿⣶⣾⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣶⣶⣶⣿⣿⣿⣿⡿⠟⠋⠉⠁⢹⣿⣶⣾⣿⣥⣭⣽⠛⠿⣿⣿⣿⣿⣧⣶⣶⡆⠀⠀⠀⠀⠀⠀
⠀⠀⠀⢀⣀⣀⣿⣿⣿⣿⡿⠟⠁⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⣿⠀⠀⠈⠙⢿⣿⣿⣿⣿⣀⣀⣀⠀⠀⠀⠀
",
        ),
        RGB(255, 156, 0).paint(
            r"⠀⠀⠀⠘⣿⣿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠸⠿⠿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⡏⠀⠀⠀⠀
⠀⢠⣤⣤⣿⣿⣿⡿⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⢻⣿⣿⣿⣦⣤⣄⠀⠀
⠀⠈⢻⣿⣿⠿⢿⣧⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⡀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⣰⣿⠿⢿⣿⡿⠁⠀⠀
⢠⣴⣾⣿⣇⠀⣨⣿⡇⠀⠀⠀⣠⣾⣿⣿⣿⣿⣿⣷⣄⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢰⣿⣇⠀⣨⣿⣷⣦⣤⠀
",
        ),
        RGB(255, 128, 0).paint(
            r"⠈⠻⣿⣿⣿⡿⠟⠋⠁⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠉⠛⢿⣿⣿⣿⡟⠁⠀
⣤⣶⣿⣿⣿⡇⠀⠀⠀⠀⢰⣿⣿⣿⣿⣿⠟⠛⠛⠻⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣷⣦⡀
",
        ),
        // Colour::Yellow.italic().paint("ChaCha:\n"),
        RGB(255, 128, 0).paint(
            r"⠉⠻⣿⣿⣿⡇⠀⠀⠀⠀⣿⣿⣿⣿⣿⡏⠀⠀⠀⠀⠘⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⡟⠋⠀
",
        ),
        // Colour::Green.italic().paint("a dwarf language REPL.\n"),
        RGB(255, 128, 0).paint(
            r"⢠⣾⣿⣿⣿⣧⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣷⣄⠀
",
        ),
        RGB(255, 102, 0).paint(
            r"⠈⠙⢻⣿⣿⣿⡄⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⡟⠛⠉⠀
⠀⢠⣾⣿⣿⣿⣷⡀⠀⠀⣿⣿⣿⣿⣿⣇⠀⠀⠀⠀⢠⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣄⠀⠀
⠀⠈⠉⠉⣿⣿⣿⣿⣄⠀⠸⣿⣿⣿⣿⣿⣦⣀⣀⣴⣿⣿⣿⣿⣿⣿⣀⣀⡀⠀⠀⠀⢀⣾⣿⣿⣿⡋⠉⠁⠀⠀
⠀⠀⠀⢰⣿⣿⣿⣿⣿⣶⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣶⣶⣿⣿⣿⣿⣿⣧⠀⠀⠀⠀
",
        ),
        RGB(255, 67, 0).paint(
            r"⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⡁⠀⢹⣿⣿⣿⣿⣿⣿⡿⠋⣿⣿⣿⣿⣿⣿⣿⣟⠀⠈⣿⣿⣿⣿⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠟⠛⠛⣿⣿⣶⣿⣿⣿⣿⣿⣉⣉⣀⣀⣉⣉⣉⣭⣽⣿⣿⣿⣷⣾⣿⡟⠛⠻⠃⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠿⠛⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠛⠻⢿⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⠿⠋⠹⣿⣿⠿⢿⣿⣿⠿⣿⣿⡟⠙⠻⡿⠀⠀⠀      ",
        ),
        Colour::Yellow.italic().paint("ChaCha:\n"),
        RGB(255, 67, 0).paint(r"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠁⠀⠈⠿⠃⠀⠈⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"),
        Colour::Green.paint("a dwarf language interpreter\n\n"),
    ];

    format!("{}", ANSIStrings(strings))
}

pub fn banner2() -> String {
    let strings: &[ANSIString<'static>] = &[
        RGB(255, 184, 0).paint(
            r"
⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣀⠀⣿⣦⣠⣿⣦⣠⣿⡀⢀⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⣿⣶⣿⣿⣿⣿⣿⣿⠀⢹⣿⣿⣿⣿⣿⣶⣾⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠸⣿⣿⣿⣿⠟⠉⠀⠀⠀⣿⣿⣿⣿⣿⣿⠉⠛⣿⣿⣿⣿⡿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 156, 0).paint(
            r"⠀⠀⠈⣿⣿⣿⣿⠉⠀⠀⠀⠀⠀⠀⣿⠿⣿⣿⣿⣿⠀⠀⠀⠈⢿⣿⣿⣿⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣶⠛⢿⣿⣦⠀⠀⠀
⠀⢶⣿⣿⣿⣟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⠀⠀⠀⠀⠀⢙⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⡟⠀⠀⠉⠉⠀⠀⠀
⣠⣶⣿⣅⠀⣿⡆⠀⠀⢠⣾⣿⣿⣿⣿⣄⣿⣿⣿⣿⠀⠀⠀⠀⠀⣿⡀⢀⣿⣶⣤⠰⣶⣶⣶⣶⠶⠀⠰⣶⣶⣶⡶⠀⠀⠶⣶⣶⡶⠀⣠⣶⠖⠲⣶⣶⡀⠀⠀⢠⣤⣶⣶⣶⢀⣶⣿⣦⣶⣶⣿⣿⣿⣶⣶⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 128, 0).paint(
            r"⠀⣽⣿⣿⠉⠀⠀⠀⣼⣿⣿⣿⣿⡿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠈⣿⣿⣿⡀⠀⠀⢻⣿⣿⡄⠀⠀⠀⣿⣿⣷⠀⠀⠀⣿⠋⠀⠘⣿⡿⠀⠀⢹⣿⣿⠀⠀⠀⠀⣿⣿⣿⠉⠙⠿⠋⠀⠀⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀
⠉⢻⣿⣿⠀⠀⠀⢰⣿⣿⣿⡟⠀⠀⠀⠘⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⠋⠀⠀⠀⢿⣿⣿⡀⠀⣿⠉⣿⣿⣧⠀⣼⠋⠀⠀⠀⣠⣶⡾⠋⢹⣿⣿⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀
⠺⣿⣿⣿⣆⠀⠀⢸⣿⣿⣿⡇⠀⠀⠀⠀⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢀⣿⣿⣿⠿⠀⠀⠀⠀⣿⣿⣿⣾⠁⠀⠘⣿⣿⣶⠏⠀⠀⠀⣿⣿⣿⠀⠀⢸⣿⣿⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 102, 0).paint(
            r"⠀⣴⣿⣿⣿⣄⠀⠘⣿⣿⣿⣷⠀⠀⠀⢠⣿⣿⣿⣿⠀⠀⠀⠀⠀⢀⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠿⠿⠁⠀⠀⠀⠻⠿⠟⠀⠀⠀⠀⠈⠻⠿⠿⠋⠈⠿⠿⠟⠉⠐⠚⠛⠛⠛⠓⠒⠀⠀⠒⠛⠛⠛⠛⠓⠒⠀⠀⠀⠀⠀
⠀⠀⢀⣿⣿⣿⣷⣀⣿⣿⣿⣿⣿⣶⣾⣿⣿⣿⣿⣿⣶⣶⣄⣀⣴⣿⣿⣿⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⢠⣿⣿⣿⠀⣹⣿⣿⣿⣿⡿⠋⣿⣿⣿⣿⣿⣿⠀⢹⣿⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        RGB(255, 67, 0).paint(
            r"⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⣶⣶⣶⣶⣶⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠻⠛⠉⣿⡿⠻⣿⡿⠻⣿⠋⠙⠿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
",
        ),
        // Colour::Yellow.italic().paint("ChaCha:\n"),
        // RGB(255, 67, 0).paint(r"⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠛⠁⠀⠈⠿⠃⠀⠈⠛⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"),
        // Colour::Green.paint("a dwarf language interpreter\n\n"),
    ];

    format!("{}", ANSIStrings(strings))
}

pub(crate) struct PrintableValueType<'a>(pub &'a RefType<ValueType>, pub &'a Context);

impl<'a, 'b> fmt::Display for PrintableValueType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = s_read!(self.0);
        let context = self.1;
        let lu_dog = &context.lu_dog;
        let sarzak = &context.sarzak;
        let model = &context.models;

        match &*value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = s_read!(lu_dog).exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = s_read!(lu_dog).exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(&s_read!(lu_dog))[0].clone();
                write!(f, "[{}]", PrintableValueType(&ty, context))
            }
            ValueType::Range(_) => write!(f, "<range>"),
            ValueType::Reference(ref reference) => {
                let reference = s_read!(lu_dog).exhume_reference(reference).unwrap();
                let reference = s_read!(reference);
                let ty = reference.r35_value_type(&s_read!(lu_dog))[0].clone();
                write!(f, "&{}", PrintableValueType(&ty, context))
            }
            ValueType::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                let sarzak = s_read!(sarzak);
                if let Some(ty) = sarzak.exhume_ty(ty) {
                    match ty {
                        Ty::Boolean(_) => write!(f, "bool"),
                        Ty::Float(_) => write!(f, "float"),
                        Ty::Integer(_) => write!(f, "int"),
                        Ty::Object(ref object) => {
                            // This should probably just be an unwrap().
                            if let Some(object) = sarzak.exhume_object(object) {
                                write!(f, "{}", object.name)
                            } else {
                                write!(f, "<unknown object>")
                            }
                        }
                        Ty::SString(_) => write!(f, "String"),
                        Ty::SUuid(_) => write!(f, "Uuid"),
                        gamma => {
                            error!("deal with sarzak type", gamma);
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    let models = s_read!(model);
                    for model in &*models {
                        if let Some(Ty::Object(ref object)) = model.exhume_ty(ty) {
                            if let Some(object) = model.exhume_object(object) {
                                return write!(f, "{}Proxy", object.name);
                            }
                        }
                    }
                    write!(f, "<unknown object>")
                }
            }
            ValueType::Unknown(_) => write!(f, "<unknown>"),
            ValueType::WoogOption(ref option) => {
                let option = s_read!(lu_dog).exhume_woog_option(option).unwrap();
                let option = s_read!(option);
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = s_read!(lu_dog).exhume_z_some(some).unwrap();
                        let some = s_read!(some);
                        let value = s_read!(some.r23_x_value(&s_read!(lu_dog))[0]).clone();
                        let ty = value.r24_value_type(&s_read!(lu_dog))[0].clone();
                        write!(f, "Some({})", PrintableValueType(&ty, context))
                    }
                }
            }
            ValueType::WoogStruct(ref woog_struct) => {
                let woog_struct = s_read!(lu_dog).exhume_woog_struct(woog_struct).unwrap();
                debug!("woog_struct", woog_struct);
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = s_read!(lu_dog).exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}

fn typecheck(
    lhs: &RefType<ValueType>,
    rhs: &RefType<ValueType>,
    span: &RefType<Span>,
    context: &Context,
) -> Result<()> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "single")] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    match (&*s_read!(lhs), &*s_read!(rhs)) {
        (_, ValueType::Empty(_)) => Ok(()),
        (ValueType::Empty(_), _) => Ok(()),
        (_, ValueType::Unknown(_)) => Ok(()),
        (ValueType::Unknown(_), _) => Ok(()),
        (lhs_t, rhs_t) => {
            if lhs_t == rhs_t {
                Ok(())
            } else {
                let lhs = PrintableValueType(lhs, context);
                let rhs = PrintableValueType(rhs, context);

                Err(ChaChaError::TypeMismatch {
                    expected: lhs.to_string(),
                    got: rhs.to_string(),
                    span: s_read!(span).start as usize..s_read!(span).end as usize,
                })
            }
        }
    }
}
