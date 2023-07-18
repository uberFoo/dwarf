use std::{ops::Range, path::Path};

use ansi_term::Colour;
use circular_queue::CircularQueue;
use crossbeam::channel::unbounded;
use lazy_static::lazy_static;
use log::{self, log_enabled, Level::Debug};
use parking_lot::{Condvar, Mutex};
use rustc_hash::FxHashMap as HashMap;
use snafu::{prelude::*, Location};
use tracy_client::{span, Client};
use uuid::Uuid;

use crate::{
    chacha::{
        error::{Error, Result, UnimplementedSnafu, VariableNotFoundSnafu},
        memory::{Memory, MemoryUpdateMessage},
        value::UserType,
        vm::{CallFrame, Instruction, Thonk, VM},
    },
    lu_dog::ExpressionEnum,
    lu_dog::{
        Block, Expression, LocalVariable, ObjectStore as LuDogStore, Span, Statement,
        StatementEnum, ValueType, Variable, XValue,
    },
    new_ref, s_read,
    sarzak::store::ObjectStore as SarzakStore,
    sarzak::MODEL as SARZAK_MODEL,
    ChaChaError, DwarfInteger, NewRef, RefType, Value,
};

mod banner;
mod context;
mod expression;
mod func_call;
mod lambda;
mod pvt;
mod repl;
mod statement;
mod tui;

pub use banner::banner2;
pub(crate) use pvt::PrintableValueType;

#[cfg(feature = "repl")]
pub use repl::start_repl;

#[cfg(not(any(feature = "single", feature = "single-vec")))]
pub use tui::start_tui_repl;

use context::Context;
use expression::{
    block, call, debugger, field, for_loop, if_expr, index, list, literal, operator, print, range,
    ret, struct_expr, typecast, variable,
};
use func_call::eval_function_call;
use lambda::eval_lambda_expression;

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
pub(crate) use function;

macro_rules! trace {
    ($($arg:tt)*) => {
        log::trace!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Green.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use trace;

macro_rules! debug {
    ($($arg:tt)*) => {
        log::debug!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use debug;

macro_rules! error {
    ($($arg:tt)*) => {
        log::error!(
            target: "chacha",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Red.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        );
    };
}
pub(crate) use error;

// what is this even for?
macro_rules! no_debug {
    ($arg:expr) => {
        log::debug!("{}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
            target: "chacha",
            "{} --> {}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

const TIMING_COUNT: usize = 1_000;

lazy_static! {
    pub(super) static ref RUNNING: Mutex<bool> = Mutex::new(true);
    pub(super) static ref CVAR: Condvar = Condvar::new();
    pub(crate) static ref STEPPING: Mutex<bool> = Mutex::new(false);
}

pub fn initialize_interpreter_paths<P: AsRef<Path>>(lu_dog_path: P) -> Result<Context, Error> {
    // unimplemented!();
    let sarzak =
        SarzakStore::from_bincode(SARZAK_MODEL).map_err(|e| ChaChaError::Store { source: e })?;

    // This will always be a lu-dog -- it's basically compiled dwarf source.
    let lu_dog = LuDogStore::load_bincode(lu_dog_path.as_ref())
        .map_err(|e| ChaChaError::Store { source: e })?;

    initialize_interpreter(sarzak, lu_dog, Some(lu_dog_path.as_ref()))
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
        let imp = s_read!(func).r9_implementation_block(&lu_dog);
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

            trace!("inserting local function {}", name);
            stack.insert(name, new_ref!(Value, value));
        }
    }

    // Insert static methods for each struct. They go into the meta table.
    for user_type in lu_dog.iter_woog_struct() {
        let user_type = s_read!(user_type);
        // Create a meta table for each struct.
        debug!("inserting meta table {}", user_type.name);
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation_block(&lu_dog);
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

    Ok(Context::new(
        format!("{} ", Colour::Blue.normal().paint("道:>")),
        block,
        stack,
        new_ref!(LuDogStore, lu_dog),
        new_ref!(SarzakStore, sarzak),
        new_ref!(HashMap<String, SarzakStore>, HashMap::default()),
        receiver,
        std_out_send,
        std_out_recv,
        None,
        CircularQueue::with_capacity(TIMING_COUNT),
        0,
        0,
        None,
    ))
}

#[allow(unused_variables)]
fn chacha_print<S: AsRef<str>>(result: S, context: &mut Context) -> Result<()> {
    let result_style = Colour::Green.bold();
    cfg_if::cfg_if! {
        if #[cfg(feature = "print-std-out")] {
            print!("{}", result_style.paint(result.as_ref()));
            std::io::Write::flush(&mut std::io::stdout()).unwrap();
        } else {
            context
                .std_out_send()
                .send(format!("{}", result_style.paint(result.as_ref())))
                .context(crate::chacha::error::InternalCompilerChannelSnafu {
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
    let lu_dog = context.lu_dog_heel().clone();

    // Timing goodness
    context.increment_expression_count(1);

    // context.tracy.span(span_location!("eval_expression"), 0);
    // context
    //     .tracy
    //     .non_continuous_frame(frame_name!("eval_expression"));
    span!("eval_expression");

    {
        let mut running = RUNNING.lock();
        if !*running {
            if let Some(sender) = &context.debug_status_writer() {
                let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                debug!("value {value:#?}");

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

        debug!("running: {expression:#?}");
        trace!("stack: {:#?}", context.memory());
    }

    if log_enabled!(Debug) {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let read = s_read!(span);
        let span = read.start as usize..read.end as usize;
        let source = context.source();
        debug!("executing {}", source[span].to_owned());
    }

    match s_read!(expression).subtype {
        ExpressionEnum::Block(ref block) => block::eval_block(block, context, vm),
        ExpressionEnum::Call(ref call) => call::eval_call(call, &expression, context, vm),
        ExpressionEnum::Debugger(_) => debugger::eval_debugger(context),
        ExpressionEnum::ErrorExpression(ref error) => expression::error::eval_error(error, context),
        ExpressionEnum::FieldAccess(ref field) => field::eval_field_access(field, context, vm),
        ExpressionEnum::FieldExpression(ref field_expr) => {
            field::eval_field_expression(field_expr, context, vm)
        }
        ExpressionEnum::ForLoop(ref for_loop) => for_loop::eval_for_loop(for_loop, context, vm),
        ExpressionEnum::Index(ref index) => index::eval_index(index, context, vm),
        //
        // Lambda
        //
        // I know it's called eval_expression, but we don't actually want to
        // evaluate the lambda, we just want to return it.
        //
        ExpressionEnum::Lambda(ref lambda) => {
            let lambda = s_read!(lu_dog).exhume_lambda(lambda).unwrap();
            let ty = s_read!(lambda).r1_value_type(&s_read!(lu_dog))[0].clone();
            Ok((new_ref!(Value, Value::Lambda(lambda)), ty))
        }
        ExpressionEnum::ListElement(ref element) => list::eval_list_element(element, context, vm),
        ExpressionEnum::ListExpression(ref list) => list::eval_list_expression(list, context, vm),
        ExpressionEnum::Literal(ref literal) => literal::eval_literal(literal, context),
        ExpressionEnum::Operator(ref operator) => {
            operator::eval_operator(operator, &expression, context, vm)
        }
        ExpressionEnum::Print(ref print) => print::eval_print(print, context, vm),
        ExpressionEnum::RangeExpression(ref range) => range::eval_range(range, context, vm),
        ExpressionEnum::StructExpression(ref expr) => {
            struct_expr::eval_struct_expression(expr, context, vm)
        }
        ExpressionEnum::TypeCast(ref expr) => typecast::eval_as_expression(expr, context, vm),
        ExpressionEnum::VariableExpression(ref expr) => {
            variable::eval_variable_expression(expr, &expression, context)
        }
        ExpressionEnum::XIf(ref expr) => if_expr::eval_if_expression(expr, context, vm),
        ExpressionEnum::XReturn(ref expr) => ret::eval_return_expression(expr, context, vm),
        //
        // ZNone
        //
        ExpressionEnum::ZNone(_) => Ok((
            new_ref!(Value, Value::Empty),
            Value::Empty.get_type(&s_read!(lu_dog)),
        )),
        //
        // ZSome
        //
        ExpressionEnum::ZSome(ref some) => {
            let some = s_read!(lu_dog).exhume_z_some(some).unwrap();
            debug!("ExpressionEnum::ZSome {some:?}");

            let value = &s_read!(some).r23_x_value(&s_read!(lu_dog))[0];
            let option = &s_read!(some).r3_woog_option(&s_read!(lu_dog))[0];
            let ty = &s_read!(option).r2_value_type(&s_read!(lu_dog))[0];

            use crate::lu_dog::XValueEnum;
            let value = match s_read!(value).subtype {
                XValueEnum::Expression(ref expr) => {
                    let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                    let (value, _ty) = eval_expression(expr, context, vm)?;
                    value
                }
                XValueEnum::Variable(ref var) => {
                    let var = s_read!(lu_dog).exhume_variable(var).unwrap();
                    let value = context.memory().get(&s_read!(var).name);

                    ensure!(value.is_some(), {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;
                        let var = s_read!(var).name.clone();
                        VariableNotFoundSnafu { var, span }
                    });

                    value.unwrap()
                }
            };

            Ok((value, ty.clone()))
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("Hey! Implement expression: {:?}!", alpha),
                }
            );

            Ok((
                new_ref!(Value, Value::Empty),
                Value::Empty.get_type(&s_read!(lu_dog)),
            ))
        }
    }
}

pub fn eval_statement(
    statement: RefType<Statement>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    debug!("eval_statement statement {statement:?}");
    trace!("eval_statement stack {:?}", context.memory());

    span!("eval_statement");

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&s_read!(lu_dog))[0].clone();
            let (value, ty) = eval_expression(expr, context, vm)?;
            no_debug!("StatementEnum::ExpressionStatement: value", s_read!(value));
            debug!("StatementEnum::ExpressionStatement: ty {ty:?}");

            Ok((
                new_ref!(Value, Value::Empty),
                Value::Empty.get_type(&s_read!(lu_dog)),
            ))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::LetStatement: stmt {stmt:?}");

            let expr = stmt.r20_expression(&s_read!(lu_dog))[0].clone();
            debug!("expr {expr:?}");

            let (value, ty) = eval_expression(expr, context, vm)?;
            debug!("value {value:?}");
            debug!("ty {ty:?}");

            let var = s_read!(stmt.r21_local_variable(&s_read!(lu_dog))[0]).clone();
            let var = s_read!(var.r12_variable(&s_read!(lu_dog))[0]).clone();
            debug!("var {var:?}");

            debug!("inserting {} = {}", var.name, s_read!(value));
            context.memory().insert(var.name, value);

            // 🚧 I'm changing this from returning ty. If something get's wonky,
            // maybe start looking here. But TBH, why would we return the type of
            // the storage?
            Ok((
                new_ref!(Value, Value::Empty),
                Value::Empty.get_type(&s_read!(lu_dog)),
            ))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            debug!("StatementEnum::ResultStatement: stmt {stmt:?}");

            let expr = stmt.r41_expression(&s_read!(lu_dog))[0].clone();
            debug!("StatementEnum::ResultStatement expr {expr:?}");

            let (value, ty) = eval_expression(expr, context, vm)?;
            debug!("StatementEnum::ResultStatement value {value:?}");
            debug!("StatementEnum::ResultStatement ty {ty:?}");

            Ok((value, ty))
        }
        StatementEnum::ItemStatement(_) => Ok((
            new_ref!(Value, Value::Empty),
            Value::Empty.get_type(&s_read!(lu_dog)),
        )),
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

/// This runs the main function, assuming it exists.
pub fn start_main(stopped: bool, mut context: Context) -> Result<(Value, Context), Error> {
    {
        let mut running = RUNNING.lock();
        *running = !stopped;
    }

    let stack = &mut context.memory();
    let vm_stack = stack.clone();
    let mut vm = VM::new(&vm_stack);

    if let Some(main) = stack.get("main") {
        // This should fail if it's not a function. Actually, I think that it _has_
        // to be a function. Unless there's another named item that I'm not thinking
        // of. I mean, maybe `use main;`  would trigger this to return OK(()), and
        // not do anything?
        if let Value::Function(ref main) = *s_read!(main) {
            let main = s_read!(context.lu_dog_heel())
                .exhume_function(&s_read!(main).id)
                .unwrap();

            let value_ty = &s_read!(main).r1_value_type(&s_read!(context.lu_dog_heel()))[0];
            let span = &s_read!(value_ty).r62_span(&s_read!(context.lu_dog_heel()))[0];

            let result = eval_function_call(main, &[], true, span, &mut context, &mut vm)?;

            #[allow(clippy::redundant_clone)]
            //              ^^^^^^^^^^^^^^^ : It's not -- the macro is just hiding the fact that it isn't.
            Ok((s_read!(result.0.clone()).clone(), context))
        } else {
            Err(Error(ChaChaError::MainIsNotAFunction))
        }
    } else {
        Err(Error(ChaChaError::NoMainFunction))
    }
}

pub fn start_vm(n: DwarfInteger) -> Result<DwarfInteger, Error> {
    let (mut memory, _) = Memory::new();
    let mut thonk = Thonk::new("fib".to_string());

    // Get the parameter off the stack
    // push {fp + 0}
    thonk.add_instruction(Instruction::PushLocal(0));
    // push 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
    // Check if it's <= 1
    // lte
    thonk.add_instruction(Instruction::LessThanOrEqual);
    // jne
    thonk.add_instruction(Instruction::JumpIfFalse(2));
    // If false return 1
    thonk.add_instruction(Instruction::Push(new_ref!(Value, 1.into())));
    thonk.add_instruction(Instruction::Return);
    // return fib(n-1) + fib(n-2)
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

fn typecheck(
    lhs: &RefType<ValueType>,
    rhs: &RefType<ValueType>,
    span: &RefType<Span>,
    location: Location,
    context: &Context,
) -> Result<()> {
    cfg_if::cfg_if! {
        if #[cfg(any(feature = "single", feature = "single-vec"))] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    let (lhs_t, rhs_t) = (&s_read!(lhs).subtype, &s_read!(rhs).subtype);
    if lhs_t == rhs_t {
        Ok(())
    } else {
        let lhs = PrintableValueType(lhs, context);
        let rhs = PrintableValueType(rhs, context);
        Err(ChaChaError::TypeMismatch {
            expected: lhs.to_string(),
            found: rhs.to_string(),
            span: s_read!(span).start as usize..s_read!(span).end as usize,
            location,
        })
    }
}
