use std::{
    collections::VecDeque,
    fmt,
    ops::Range,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
    thread,
    time::Duration,
};

use ansi_term::Colour::{self, RGB};
use ansi_term::{ANSIString, ANSIStrings};
use crossbeam::channel::{select, unbounded, Receiver, RecvTimeoutError, Sender};
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use lazy_static::lazy_static;
use log;
use parking_lot::{Condvar, Mutex, RwLock as PRwLock};
use rayon::prelude::*;
use sarzak::sarzak::{store::ObjectStore as SarzakStore, types::Ty, MODEL as SARZAK_MODEL};
use snafu::{location, prelude::*, Location};
use uuid::Uuid;

use crate::{
    dwarf::{inter_statement, parse_line},
    lu_dog::{
        Argument, Binary, Block, BooleanLiteral, CallEnum, Comparison, DwarfSourceFile, Expression,
        FieldAccessTarget, Function, Import, List, Literal, LocalVariable,
        ObjectStore as LuDogStore, OperatorEnum, Span, Statement, StatementEnum, ValueType,
        Variable, WoogOptionEnum, XValue, XValueEnum,
    },
    svm::{CallFrame, Chunk, Instruction, VM},
    value::{StoreProxy, UserType},
    ChaChaError, DwarfInteger, Error, InternalCompilerChannelSnafu, NoSuchFieldSnafu,
    NoSuchStaticMethodSnafu, Result, TypeMismatchSnafu, UnimplementedSnafu, Value,
    VariableNotFoundSnafu, WrongNumberOfArgumentsSnafu,
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

// static ref STEPPING:

lazy_static! {
    static ref RUNNING: Mutex<bool> = Mutex::new(true);
    static ref CVAR: Condvar = Condvar::new();
    static ref STEPPING: Mutex<bool> = Mutex::new(false);
//     pub(crate) static ref MODELS: Arc<RwLock<Vec<SarzakStore>>> = Arc::new(RwLock::new(Vec::new()));
//     pub(crate) static ref LU_DOG: Arc<RwLock<LuDogStore>> =
//         Arc::new(RwLock::new(LuDogStore::new()));
//     pub(crate) static ref SARZAK: Arc<RwLock<SarzakStore>> =
//         Arc::new(RwLock::new(SarzakStore::new()));
}

pub fn initialize_interpreter_paths<P: AsRef<Path>>(lu_dog_path: P) -> Result<Context, Error> {
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
    lu_dog_path: Option<P>,
) -> Result<Context, Error> {
    // Initialize the stack with stuff from the compiled source.
    let block = Block::new(Uuid::new_v4(), &mut lu_dog);
    let (mut stack, receiver) = Memory::new();

    // Insert the functions in the root frame.
    let funcs = lu_dog.iter_function().collect::<Vec<_>>();

    for func in funcs {
        let func_read = func.read().unwrap();
        let imp = func_read.r9_implementation(&lu_dog);
        if imp.is_empty() {
            let name = func_read.name.clone();
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
            stack.insert(name, Arc::new(RwLock::new(value)));
        }
    }

    // Insert static methods for each struct. They go into the meta table.
    for user_type in lu_dog.iter_woog_struct() {
        let user_type = user_type.read().unwrap();
        // Create a meta table for each struct.
        debug!("inserting meta table {}", user_type.name);
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation(&lu_dog);
        if !impl_.is_empty() {
            // For each function in the impl, insert the function. I should probably
            // check and only insert the static functions.
            // 🚧 Only insert the static functions
            for func in impl_[0].read().unwrap().r9_function(&lu_dog) {
                debug!("inserting static function {}", func.read().unwrap().name);
                stack.insert_meta(
                    &user_type.name,
                    func.read().unwrap().name.to_owned(),
                    // It's here that I'd really like to be able to do a cheap
                    // clone of an Rc.
                    Arc::new(RwLock::new(Value::Function(func.clone()))),
                )
            }
        }
    }

    // T::initialize(&mut stack, &mut *lu_dog.write().unwrap());

    // 🚧 this needs to go into some sort of init function that get's passed
    // the stack. Or alternatively, there's a function that returns key value
    // pairs to be inserted into the stack. One way or the other...
    // stack.insert_global(
    // "MERLIN_STORE".to_owned(),
    // Value::UserType(MerlinType::MerlinStore((*model).clone())),
    // );

    // {
    //     // Build the ASTs
    //     let local = LocalVariable::new(Uuid::new_v4(), &mut *lu_dog.write().unwrap());
    //     let var = Variable::new_local_variable(
    //         "MERLIN_STORE".to_owned(),
    //         local,
    //         &mut *lu_dog.write().unwrap(),
    //     );

    //     let store = ZObjectStore::new("merlin".to_owned(), &mut *lu_dog.write().unwrap());
    //     let mut write = lu_dog.write().unwrap();
    //     let _value = LuDogValue::new_variable(
    //         block.clone(),
    //         ValueType::new_z_object_store(store, &mut write),
    //         var,
    //         &mut write,
    //     );
    // }

    // Hide everything behind a the globals.
    // *SARZAK.write().unwrap() = sarzak;
    // *LU_DOG.write().unwrap() = lu_dog;
    // *MODEL.write().unwrap() = model;

    let (std_out_send, std_out_recv) = unbounded();

    Ok(Context {
        prompt: format!("{} ", Colour::Blue.normal().paint("道:>")),
        stack,
        block,
        lu_dog: Arc::new(RwLock::new(lu_dog)),
        sarzak: Arc::new(RwLock::new(sarzak)),
        models: Arc::new(RwLock::new(Vec::new())),
        mem_update_recv: receiver,
        std_out_send,
        std_out_recv,
        debug_status_writer: None,
        obj_file_path: lu_dog_path.map(|p| p.as_ref().to_owned()),
    })
}

fn eval_function_call(
    func: Arc<RwLock<Function>>,
    args: &[Arc<RwLock<Argument>>],
    context: &mut Context,
) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
    let lu_dog = context.lu_dog.clone();

    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", context.stack);

    let func = func.read().unwrap();
    let block = lu_dog.read().unwrap().exhume_block(&func.block).unwrap();
    let block = block.read().unwrap();
    let stmts = block.r18_statement(&lu_dog.read().unwrap());

    if !stmts.is_empty() {
        context.stack.push();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&lu_dog.read().unwrap());

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
                .r13_parameter(&lu_dog.read().unwrap())
                .iter()
                .find(|p| {
                    p.read()
                        .unwrap()
                        .r14c_parameter(&lu_dog.read().unwrap())
                        .is_empty()
                })
                .unwrap()
                .clone();

            loop {
                let var = next.read().unwrap().r12_variable(&lu_dog.read().unwrap())[0]
                    .read()
                    .unwrap()
                    .clone();
                let value = var.r11_x_value(&lu_dog.read().unwrap())[0]
                    .read()
                    .unwrap()
                    .clone();
                let ty = value.r24_value_type(&lu_dog.read().unwrap())[0].clone();
                params.push((var.name.clone(), ty.clone()));

                let next_id = { next.read().unwrap().next };
                if let Some(ref id) = next_id {
                    next = lu_dog.read().unwrap().exhume_parameter(id).unwrap();
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
                .find(|a| {
                    a.read()
                        .unwrap()
                        .r27c_argument(&lu_dog.read().unwrap())
                        .is_empty()
                })
                .unwrap()
                .clone();

            loop {
                let expr = lu_dog
                    .read()
                    .unwrap()
                    .exhume_expression(&next.read().unwrap().expression)
                    .unwrap();
                let (value, ty) = eval_expression(expr.clone(), context)?;
                arg_values.push((expr, value, ty));

                let next_id = { next.read().unwrap().next };
                if let Some(ref id) = next_id {
                    next = lu_dog.read().unwrap().exhume_argument(id).unwrap();
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

            let x_value = &expr.read().unwrap().r11_x_value(&lu_dog.read().unwrap())[0];
            let span = &x_value.read().unwrap().r63_span(&lu_dog.read().unwrap())[0];

            typecheck(&param_ty, &arg_ty, span, context)?;

            context.stack.insert(name.clone(), value);
        }

        let mut value;
        let mut ty;
        // This is a pain.
        // Find the first statement, by looking for the one with no previous statement.
        let mut next = stmts
            .iter()
            .find(|s| {
                s.read()
                    .unwrap()
                    .r17c_statement(&lu_dog.read().unwrap())
                    .is_empty()
            })
            .unwrap()
            .clone();

        loop {
            let result = eval_statement(next.clone(), context).map_err(|e| {
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
                context.stack.pop();

                // if let ChaChaError::Return { value } = &e {
                //     let ty = value.get_type(&lu_dog.read().unwrap());
                //     return Ok((value, ty));
                // }

                // Err(e)
                e
            });

            if let Err(ChaChaError::Return { value, ty }) = &result {
                return Ok((value.clone(), ty.clone()));
            }

            (value, ty) = result?;

            if let Some(ref id) = next.clone().read().unwrap().next {
                next = lu_dog.read().unwrap().exhume_statement(id).unwrap();
            } else {
                break;
            }
        }

        // Clean up
        context.stack.pop();

        Ok((value, ty))
    } else {
        Ok((
            Arc::new(RwLock::new(Value::Empty)),
            ValueType::new_empty(&lu_dog.read().unwrap()),
        ))
    }
}

fn eval_expression(
    expression: Arc<RwLock<Expression>>,
    mut context: &mut Context,
) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
    let lu_dog = context.lu_dog.clone();

    debug!("expression", expression);
    trace!("stack", context.stack);

    {
        let mut running = RUNNING.lock();
        if !*running {
            if let Some(sender) = &context.debug_status_writer {
                let value = &expression
                    .read()
                    .unwrap()
                    .r11_x_value(&lu_dog.read().unwrap())[0];
                debug!("value", value);

                let span = &value.read().unwrap().r63_span(&lu_dog.read().unwrap())[0];

                let read = span.read().unwrap();
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

    let result_style = Colour::Green.bold();

    match *expression.read().unwrap() {
        //
        // Block
        //
        Expression::Block(ref block) => {
            let block = lu_dog.read().unwrap().exhume_block(block).unwrap();
            let block = block.read().unwrap();
            let stmts = block.r18_statement(&lu_dog.read().unwrap());

            if !stmts.is_empty() {
                context.stack.push();
                let mut value;
                let mut ty;
                // This is a pain.
                // Find the first statement, by looking for the one with no previous statement.
                let mut next = stmts
                    .iter()
                    .find(|s| {
                        s.read()
                            .unwrap()
                            .r17c_statement(&lu_dog.read().unwrap())
                            .is_empty()
                    })
                    .unwrap()
                    .clone();

                loop {
                    let result = eval_statement(next.clone(), context).map_err(|e| {
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
                        context.stack.pop();

                        // if let ChaChaError::Return { value } = &e {
                        //     let ty = value.get_type(&lu_dog.read().unwrap());
                        //     return Ok((value, ty));
                        // }

                        // Err(e)
                        e
                    });

                    // if let Err(ChaChaError::Return { value, ty }) = &result {
                    //     return Ok((value.clone(), ty.clone()));
                    // }

                    (value, ty) = result?;

                    if let Some(ref id) = next.clone().read().unwrap().next {
                        next = lu_dog.read().unwrap().exhume_statement(id).unwrap();
                    } else {
                        break;
                    }
                }

                // Clean up
                context.stack.pop();

                Ok((value, ty))
            } else {
                Ok((
                    Arc::new(RwLock::new(Value::Empty)),
                    ValueType::new_empty(&lu_dog.read().unwrap()),
                ))
            }
        }
        //
        // Call
        //
        Expression::Call(ref call) => {
            let call = lu_dog.read().unwrap().exhume_call(call).unwrap();
            let call = call.read().unwrap();
            debug!("call", call);
            let args = call.r28_argument(&lu_dog.read().unwrap());
            debug!("args", args);

            // This optional expression is the LHS of the call.
            let (value, ty) = if let Some(ref expr) = call.expression {
                let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let (value, ty) = eval_expression(expr, context)?;
                no_debug!("Expression::Call LHS value", value.read().unwrap());
                debug!("Expression::Call LHS ty", ty);
                // So now value is pointing a a legit Function. We need to jump
                // through all sorts of hoops now. We need to setup a new stack
                // frame, and push the old one on to a stack that doesn't exist
                // yet. Then we need to eval all the arguments and put them in
                // the frame. And then we need to eval the statements in the
                // function body.

                // Or we can just call the function we already wrote!
                let read_value = value.read().unwrap();
                match &*read_value {
                    Value::Function(ref func) => {
                        let func = lu_dog
                            .read()
                            .unwrap()
                            .exhume_function(&func.read().unwrap().id)
                            .unwrap();
                        debug!("Expression::Call func", func);

                        let (value, ty) = eval_function_call(func, &args, context)?;
                        debug!("value", value);
                        debug!("ty", ty);
                        (value, ty)
                    }
                    Value::ProxyType(pt) => {
                        let ty = lu_dog
                            .read()
                            .unwrap()
                            .exhume_value_type(&pt.read().unwrap().struct_uuid())
                            .unwrap();
                        (value.clone(), ty)
                    }
                    Value::UserType(ut) => (value.clone(), ut.read().unwrap().get_type().clone()),
                    value_ => {
                        let value = &expression
                            .read()
                            .unwrap()
                            .r11_x_value(&lu_dog.read().unwrap())[0];
                        debug!("value", value);

                        let span = &value.read().unwrap().r63_span(&lu_dog.read().unwrap())[0];

                        let read = span.read().unwrap();
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NotAFunction {
                            value: value_.to_owned(),
                            span,
                        });
                    }
                }
            } else {
                (
                    Arc::new(RwLock::new(Value::Empty)),
                    ValueType::new_empty(&lu_dog.read().unwrap()),
                )
            };

            // So we need to figure out the type this is being called upon.
            match (&call.subtype, value, ty) {
                //
                // FunctionCall
                //
                // We already handled this above.
                (CallEnum::FunctionCall(_), value, ty) => Ok((value, ty)),
                //
                // MethodCall
                //
                (CallEnum::MethodCall(meth), value, ty) => {
                    let meth = lu_dog.read().unwrap().exhume_method_call(meth).unwrap();
                    let meth = &meth.read().unwrap().name;
                    debug!("MethodCall method", meth);
                    debug!("MethodCall value", value);
                    debug!("MethodCall type", ty);

                    match &*value.read().unwrap() {
                        Value::ProxyType(pt) => {
                            let mut arg_values = if !args.is_empty() {
                                let mut arg_values = VecDeque::with_capacity(args.len());
                                let mut next = args
                                    .iter()
                                    .find(|a| {
                                        a.read()
                                            .unwrap()
                                            .r27c_argument(&lu_dog.read().unwrap())
                                            .is_empty()
                                    })
                                    .unwrap()
                                    .clone();

                                loop {
                                    let expr = lu_dog
                                        .read()
                                        .unwrap()
                                        .exhume_expression(&next.read().unwrap().expression)
                                        .unwrap();
                                    let (value, _ty) = eval_expression(expr, context)?;
                                    arg_values.push_back(value);

                                    let next_id = { next.read().unwrap().next };
                                    if let Some(ref id) = next_id {
                                        next = lu_dog.read().unwrap().exhume_argument(id).unwrap();
                                    } else {
                                        break;
                                    }
                                }

                                arg_values
                            } else {
                                VecDeque::new()
                            };

                            pt.write().unwrap().call(meth, &mut arg_values)
                        }
                        Value::UserType(ut) => {
                            // Well, we need to get the function and the arguments
                            // and invoke eval_function_call.
                            let ut_read = ut.read().unwrap();
                            let ty = ut_read.get_type();
                            let ty = ty.read().unwrap();
                            let woog_struct = if let ValueType::WoogStruct(woog_struct) = &*ty {
                                woog_struct
                            } else {
                                panic!("I'm trying to invoke a function on a UserType, and it's not a Struct!");
                            };
                            let woog_struct = lu_dog
                                .read()
                                .unwrap()
                                .exhume_woog_struct(&woog_struct)
                                .unwrap();
                            let woog_struct = woog_struct.read().unwrap();
                            let impl_ = &woog_struct.r8c_implementation(&lu_dog.read().unwrap())[0];
                            let x = if let Some(func) = &impl_
                                .read()
                                .unwrap()
                                .r9_function(&lu_dog.read().unwrap())
                                .iter()
                                .find(|f| f.read().unwrap().name == *meth)
                            {
                                eval_function_call((*func).clone(), &args, context)
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
                    let meth = lu_dog
                        .read()
                        .unwrap()
                        .exhume_static_method_call(meth)
                        .unwrap();
                    let call = meth.read().unwrap().r30_call(&lu_dog.read().unwrap())[0].clone();
                    let args = call.read().unwrap().r28_argument(&lu_dog.read().unwrap());

                    // This is for method call on a store type, and we do it out here so that we don't have
                    // to borrow stack mutably more than once.
                    let mut arg_values = if !args.is_empty() {
                        let mut arg_values = VecDeque::with_capacity(args.len());
                        let mut next = args
                            .iter()
                            .find(|a| {
                                a.read()
                                    .unwrap()
                                    .r27c_argument(&lu_dog.read().unwrap())
                                    .is_empty()
                            })
                            .unwrap()
                            .clone();

                        loop {
                            let expr = lu_dog
                                .read()
                                .unwrap()
                                .exhume_expression(&next.read().unwrap().expression)
                                .unwrap();
                            let (value, _ty) = eval_expression(expr, context)?;
                            arg_values.push_back(value);

                            let next_id = { next.read().unwrap().next };
                            if let Some(ref id) = next_id {
                                next = lu_dog.read().unwrap().exhume_argument(id).unwrap();
                            } else {
                                break;
                            }
                        }

                        arg_values
                    } else {
                        VecDeque::new()
                    };

                    let ty = &meth.read().unwrap().ty;
                    let func = &meth.read().unwrap().func;
                    debug!("StaticMethodCall ty", ty);
                    debug!("StaticMethodCall func", func);

                    // This is dirty. Down and dirty...
                    if ty == "Uuid" && func == "new" {
                        let value = Value::Uuid(Uuid::new_v4());
                        let ty = Ty::new_s_uuid();
                        let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                        Ok((Arc::new(RwLock::new(value)), ty))
                    } else if let Some(value) = context.stack.get_meta(ty, func) {
                        debug!("StaticMethodCall meta value", value);
                        match &*value.read().unwrap() {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall meta func", func);
                                let (value, ty) = eval_function_call(func, &args, context)?;
                                debug!("StaticMethodCall meta value", value);
                                debug!("StaticMethodCall meta ty", ty);
                                Ok((value, ty))
                            }
                            value => {
                                error!("deal with call expression", value);
                                Ok((
                                    Arc::new(RwLock::new(Value::Empty)),
                                    ValueType::new_empty(&lu_dog.read().unwrap()),
                                ))
                            }
                        }
                    } else if let Some(value) = context.stack.get(ty) {
                        debug!("StaticMethodCall frame value", value);
                        match &mut *value.write().unwrap() {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall frame func", func);
                                let (value, ty) = eval_function_call(func, &args, context)?;
                                debug!("StaticMethodCall frame value", value);
                                debug!("StaticMethodCall frame ty", ty);
                                Ok((value, ty))
                            }
                            Value::ProxyType(ut) => {
                                debug!("StaticMethodCall proxy", ut);
                                ut.write().unwrap().call(func, &mut arg_values)
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
                                    Arc::new(RwLock::new(Value::Empty)),
                                    ValueType::new_empty(&lu_dog.read().unwrap()),
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
                        //     Arc::new(RwLock::new(Value::Empty)),
                        //     ValueType::new_empty(&lu_dog.read().unwrap()),
                        // ))
                    }
                }
            }
        }
        Expression::Debugger(_) => {
            debug!("StatementEnum::Debugger");
            let mut running = RUNNING.lock();
            *running = false;
            *STEPPING.lock() = true;
            Ok((
                Arc::new(RwLock::new(Value::Empty)),
                ValueType::new_empty(&lu_dog.read().unwrap()),
            ))
        }
        //
        // Error Expression
        //
        // 🚧 This should be looked at as part of  The Great Error Overhaul
        //
        Expression::ErrorExpression(ref error) => {
            let error = lu_dog
                .read()
                .unwrap()
                .exhume_error_expression(error)
                .unwrap();

            // 🚧 This isn't going to cut it.
            print!("\t{}", error.read().unwrap().span);

            Ok((
                Arc::new(RwLock::new(Value::Empty)),
                ValueType::new_empty(&lu_dog.read().unwrap()),
            ))
        }
        //
        // FieldAccess
        //
        Expression::FieldAccess(ref field) => {
            let field = lu_dog.read().unwrap().exhume_field_access(field).unwrap();
            let fat = &field
                .read()
                .unwrap()
                .r65_field_access_target(&lu_dog.read().unwrap())[0];
            let field_name = match *fat.read().unwrap() {
                FieldAccessTarget::Field(ref field) => {
                    let field = lu_dog.read().unwrap().exhume_field(field).unwrap();
                    let field = field.read().unwrap();
                    field.name.to_owned()
                }
                FieldAccessTarget::Function(ref func) => {
                    let func = lu_dog.read().unwrap().exhume_function(func).unwrap();
                    let func = func.read().unwrap();
                    func.name.to_owned()
                }
            };

            // debug!("FieldAccess field", field);

            // let field_name = &field.read().unwrap().name;

            // What we're doing below is actualy dereferencing a pointer. I wonder
            // if there is a way to make this less confusing and error prone? A
            // macro wouldn't work because the pointer is stored under vairous
            // names. So it would be a function on the referrer. Like realationship
            // navigation, actually.
            //      `let expr = field.expression(lu_dog).unwrap()`
            // Something like that.
            // A macro could maybe do it, if we pass the name of the field storing
            // the pointer, actually.
            //
            let expr = &field.read().unwrap().expression;
            let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
            // dereference!(field, expression, lu_dog);

            let (value, _ty) = eval_expression(expr, context)?;
            let value = value.read().unwrap();
            match &*value {
                Value::ProxyType(value) => {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(&field_name)?;
                    let ty = value.read().unwrap().get_type(&lu_dog.read().unwrap());

                    Ok((value, ty))
                }
                Value::UserType(value) => {
                    // This could be a field that we look up, and it may be a
                    // function call. We need to check for a function call first,
                    // and then a field.
                    // let woog_struct = field.r7_woog_struct(&field.id).unwrap();
                    // let woog_struct = woog_struct.read().unwrap();
                    // if let Some(impl_) = woog_struct.r8c_implementation(lu_dog).pop() {
                    //     let impl_ = impl_.read().unwrap();

                    //     let funcs = impl_.r9_function(lu_dog);
                    //     let func = funcs
                    //         .iter()
                    //         .find(|f| f.read().unwrap().name == field_name)
                    //         .and_then(|f| Some(f.clone()));
                    //     let ty = func.read().unwrap().return_type;
                    // } else {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(field_name).unwrap();
                    let ty = value.read().unwrap().get_type(&lu_dog.read().unwrap());

                    Ok((value.clone(), ty))
                    // }
                }
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

            let for_loop = lu_dog.read().unwrap().exhume_for_loop(for_loop).unwrap();
            let for_loop = for_loop.read().unwrap();
            let ident = for_loop.ident.to_owned();
            let block = lu_dog
                .read()
                .unwrap()
                .exhume_block(&for_loop.block)
                .unwrap();
            let list = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&for_loop.expression)
                .unwrap();

            let (list, _ty) = eval_expression(list, context)?;
            let list = list.read().unwrap();
            let list = if let Value::Vector(vec) = list.clone() {
                vec
            } else if let Value::String(str) = &*list {
                str.chars()
                    .map(|c| Arc::new(RwLock::new(Value::Char(c))))
                    .collect()
            } else if let Value::Range(range) = &*list {
                let mut vec = Vec::new();
                for i in (&*range.start.read().unwrap()).try_into()?
                    ..(&*range.end.read().unwrap()).try_into()?
                {
                    vec.push(Arc::new(RwLock::new(Value::Integer(i))));
                }
                vec
            } else {
                return Err(ChaChaError::BadJuJu {
                    message: "For loop expression is not a list".to_owned(),
                    location: location!(),
                });
            };

            let block = Expression::new_block(&block, &mut *lu_dog.write().unwrap());
            context.stack.push();
            // list.par_iter().for_each(|item| {
            //     // This gives each thread it's own stack frame, and read only
            //     // access to the parent stack frame. I don't know that I love
            //     // this solution. But it's a qucik hack to threading.
            //     let mut stack = context.stack.clone();
            //     stack.insert(ident.clone(), item.clone());
            //     eval_expression(block.clone(), &mut context.clone()).unwrap();
            // });
            for item in list {
                context.stack.insert(ident.clone(), item);
                let expr_ty = eval_expression(block.clone(), context);
                match expr_ty {
                    Ok(_) => {}
                    Err(e) => {
                        context.stack.pop();
                        return Err(e);
                    }
                }
            }
            context.stack.pop();

            Ok((
                Arc::new(RwLock::new(Value::Empty)),
                ValueType::new_empty(&lu_dog.read().unwrap()),
            ))
        }
        //
        // Index
        //
        Expression::Index(ref index) => {
            let index = lu_dog.read().unwrap().exhume_index(index).unwrap();
            let index = index.read().unwrap();
            let target = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&index.target)
                .unwrap();
            let index = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&index.index)
                .unwrap();

            let (index, _ty) = eval_expression(index, context)?;
            let index = if let Value::Integer(index) = *index.read().unwrap() {
                index as usize
            } else {
                return Err(ChaChaError::BadJuJu {
                    message: "Index is not an integer".to_owned(),
                    location: location!(),
                });
            };

            let (list, _ty) = eval_expression(target, context)?;
            let list = list.read().unwrap();
            if let Value::Vector(vec) = list.clone() {
                if index < vec.len() {
                    Ok((
                        vec[index].to_owned(),
                        ValueType::new_empty(&lu_dog.read().unwrap()),
                    ))
                } else {
                    Err(ChaChaError::BadJuJu {
                        message: "Index out of bounds".to_owned(),
                        location: location!(),
                    })
                }
            } else if let Value::String(str) = &*list {
                if index < str.len() {
                    Ok((
                        Arc::new(RwLock::new(Value::String(str[index..index + 1].to_owned()))),
                        ValueType::new_empty(&lu_dog.read().unwrap()),
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
            let element = lu_dog.read().unwrap().exhume_list_element(element).unwrap();
            let element = element.read().unwrap();
            let expr = element.r55_expression(&lu_dog.read().unwrap())[0].clone();
            eval_expression(expr, context)
        }
        //
        // ListExpression
        //
        Expression::ListExpression(ref list) => {
            let list = lu_dog.read().unwrap().exhume_list_expression(list).unwrap();
            let list = list.read().unwrap();
            if let Some(ref element) = list.elements {
                // This is the first element in the list. We need to give this list
                // a type, and I'm going to do the esay thing here and take the type
                // to be whatever the first element evaluetes to. We'll then check
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
                let element = lu_dog.read().unwrap().exhume_list_element(element).unwrap();
                let element = element.read().unwrap();
                let expr = element.r15_expression(&lu_dog.read().unwrap())[0].clone();
                let (value, ty) = eval_expression(expr, context)?;
                let mut values = vec![value];

                let mut next = element.next;
                while let Some(ref id) = next {
                    let element = lu_dog.read().unwrap().exhume_list_element(id).unwrap();
                    let element = element.read().unwrap();
                    let expr = element.r15_expression(&lu_dog.read().unwrap())[0].clone();
                    let (value, _ty) = eval_expression(expr, context)?;
                    values.push(value);
                    next = element.next;
                }

                let mut lu_dog = lu_dog.write().unwrap();
                let list = List::new(&ty, &mut lu_dog);

                Ok((
                    Arc::new(RwLock::new(Value::Vector(values))),
                    ValueType::new_list(&list, &mut lu_dog),
                ))
            } else {
                let mut lu_dog = lu_dog.write().unwrap();
                let list = List::new(&ValueType::new_empty(&lu_dog), &mut lu_dog);

                Ok((
                    Arc::new(RwLock::new(Value::Vector(vec![Arc::new(RwLock::new(
                        Value::Empty,
                    ))]))),
                    ValueType::new_list(&list, &mut lu_dog),
                ))
            }
        }
        //
        // Literal
        //
        Expression::Literal(ref literal) => {
            let literal = lu_dog.read().unwrap().exhume_literal(literal).unwrap();
            let z = match &*literal.read().unwrap() {
                //
                // BooleanLiteral
                //
                Literal::BooleanLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_boolean_literal(literal)
                        .unwrap();
                    let literal = literal.read().unwrap();
                    let ty = Ty::new_boolean();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    match *literal {
                        BooleanLiteral::FalseLiteral(_) => {
                            Ok((Arc::new(RwLock::new(Value::Boolean(false))), ty))
                        }
                        BooleanLiteral::TrueLiteral(_) => {
                            Ok((Arc::new(RwLock::new(Value::Boolean(true))), ty))
                        }
                    }
                }
                //
                // FloatLiteral
                //
                Literal::FloatLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_float_literal(literal)
                        .unwrap();
                    let value = literal.read().unwrap().x_value;
                    let value = Value::Float(value);
                    let ty = Ty::new_float();
                    debug!("ty: {:?}", ty);
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();
                    debug!("ty: {:?}", ty);

                    Ok((Arc::new(RwLock::new(value)), ty))
                }
                //
                // IntegerLiteral
                //
                Literal::IntegerLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_integer_literal(literal)
                        .unwrap();
                    let value = literal.read().unwrap().x_value;
                    let value = Value::Integer(value);
                    let ty = Ty::new_integer();
                    debug!("ty: {:?}", ty);
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();
                    debug!("ty: {:?}", ty);

                    Ok((Arc::new(RwLock::new(value)), ty))
                }
                //
                // StringLiteral
                //
                Literal::StringLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_string_literal(literal)
                        .unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value = Value::String(literal.read().unwrap().x_value.clone());
                    let ty = Ty::new_s_string();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();
                    Ok((Arc::new(RwLock::new(value)), ty))
                }
            };
            z
        }
        //
        // Negation
        //
        Expression::Negation(ref id) => {
            let negation = lu_dog.read().unwrap().exhume_negation(id).unwrap();
            let operand = negation
                .read()
                .unwrap()
                .r70_expression(&lu_dog.read().unwrap())[0]
                .clone();
            let (value, ty) = eval_expression(operand, context)?;
            let value = -value.read().unwrap().clone();

            Ok((Arc::new(RwLock::new(value)), ty))
        }
        //
        // Operator
        //
        Expression::Operator(ref operator) => {
            let operator = lu_dog.read().unwrap().exhume_operator(operator).unwrap();
            let operator = operator.read().unwrap();
            let lhs_expr = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&operator.lhs)
                .unwrap();

            let (lhs, lhs_ty) = eval_expression(lhs_expr.clone(), context)?;
            let rhs = if let Some(ref rhs) = operator.rhs {
                let rhs = lu_dog.read().unwrap().exhume_expression(rhs).unwrap();
                let (rhs, _rhs_ty) = eval_expression(rhs, context)?;
                Some(rhs)
            } else {
                None
            };

            match &operator.subtype {
                OperatorEnum::Binary(ref binary) => {
                    let binary = lu_dog.read().unwrap().exhume_binary(binary).unwrap();
                    let binary = binary.read().unwrap();
                    match &*binary {
                        Binary::Addition(_) => {
                            let value =
                                lhs.read().unwrap().clone() + rhs.unwrap().read().unwrap().clone();
                            Ok((Arc::new(RwLock::new(value)), lhs_ty))
                        }
                        Binary::Assignment(_) => {
                            if let Expression::VariableExpression(expr) = &*lhs_expr.read().unwrap()
                            {
                                let expr = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_variable_expression(expr)
                                    .unwrap();
                                let expr = expr.read().unwrap();
                                let name = expr.name.clone();
                                let value = context.stack.get(&name).unwrap();
                                let mut value = value.write().unwrap();
                                *value = rhs.unwrap().read().unwrap().clone();
                                // stack.insert(name, rhs.unwrap().clone());
                            }

                            Ok((lhs, lhs_ty))
                        }
                        Binary::Division(_) => {
                            let value =
                                lhs.read().unwrap().clone() / rhs.unwrap().read().unwrap().clone();
                            Ok((Arc::new(RwLock::new(value)), lhs_ty))
                        }
                        Binary::Subtraction(_) => {
                            let value =
                                lhs.read().unwrap().clone() - rhs.unwrap().read().unwrap().clone();
                            Ok((Arc::new(RwLock::new(value)), lhs_ty))
                        }
                        Binary::Multiplication(_) => {
                            let value =
                                lhs.read().unwrap().clone() * rhs.unwrap().read().unwrap().clone();
                            Ok((Arc::new(RwLock::new(value)), lhs_ty))
                        }
                        ref alpha => {
                            ensure!(
                                false,
                                UnimplementedSnafu {
                                    message: format!("deal with expression: {:?}", alpha),
                                }
                            );
                            Ok((Arc::new(RwLock::new(Value::Empty)), lhs_ty))
                        }
                    }
                }
                OperatorEnum::Comparison(ref comp) => {
                    let comp = lu_dog.read().unwrap().exhume_comparison(comp).unwrap();
                    let comp = comp.read().unwrap();
                    match &*comp {
                        Comparison::GreaterThan(_) => {
                            let value = lhs.read().unwrap().gt(&rhs.unwrap().read().unwrap());
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(
                                &Arc::new(RwLock::new(ty)),
                                &mut lu_dog.write().unwrap(),
                            );

                            Ok((Arc::new(RwLock::new(value)), ty))
                        }
                        Comparison::LessThanOrEqual(_) => {
                            let value = lhs.read().unwrap().lte(&rhs.unwrap().read().unwrap());
                            let value = Value::Boolean(value);
                            let ty = Ty::new_boolean();
                            let ty = ValueType::new_ty(
                                &Arc::new(RwLock::new(ty)),
                                &mut lu_dog.write().unwrap(),
                            );

                            Ok((Arc::new(RwLock::new(value)), ty))
                        }
                        _ => unimplemented!(),
                    }
                }
            }
        }
        //
        // Print
        //
        Expression::Print(ref print) => {
            let print = lu_dog.read().unwrap().exhume_print(print).unwrap();
            debug!("Expression::Print print", print);
            let expr = print
                .read()
                .unwrap()
                .r32_expression(&lu_dog.read().unwrap())[0]
                .clone();
            let (value, _) = eval_expression(expr, context)?;
            let result = format!("{}", value.read().unwrap());
            let result = result.replace("\\n", "\n");

            context
                .std_out_send
                .send(format!("{}", result_style.paint(result)))
                .context(InternalCompilerChannelSnafu {
                    message: "error writing to std out queue".to_owned(),
                })?;

            Ok((value, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
        //
        // Range
        //
        Expression::RangeExpression(ref range) => {
            let range = lu_dog
                .read()
                .unwrap()
                .exhume_range_expression(range)
                .unwrap();
            let lhs = range.read().unwrap().lhs.unwrap();
            let lhs = lu_dog.read().unwrap().exhume_expression(&lhs).unwrap();
            let rhs = range.read().unwrap().rhs.unwrap();
            let rhs = lu_dog.read().unwrap().exhume_expression(&rhs).unwrap();

            let (lhs, _) = eval_expression(lhs, context)?;
            let (rhs, _) = eval_expression(rhs, context)?;

            let range = Range {
                start: Box::new(lhs),
                end: Box::new(rhs),
            };

            Ok((
                Arc::new(RwLock::new(Value::Range(range))),
                ValueType::new_range(&lu_dog.read().unwrap()),
            ))
        }
        //
        // StructExpression
        //
        Expression::StructExpression(ref expr) => {
            let expr = lu_dog
                .read()
                .unwrap()
                .exhume_struct_expression(expr)
                .unwrap();
            let field_exprs = expr
                .read()
                .unwrap()
                .r26_field_expression(&lu_dog.read().unwrap());

            // Get name, value and type for each field expression.
            let field_exprs = field_exprs
                .iter()
                .map(|f| {
                    let expr = lu_dog
                        .read()
                        .unwrap()
                        .exhume_expression(&f.read().unwrap().expression)
                        .unwrap();
                    let (value, ty) = eval_expression(expr.clone(), context)?;
                    debug!("StructExpression field value", value);
                    debug!("StructExpression field ty", ty);
                    Ok((f.read().unwrap().name.clone(), ty, value, expr))
                })
                .collect::<Result<Vec<_>>>()?;

            let woog_struct = expr
                .read()
                .unwrap()
                .r39_woog_struct(&lu_dog.read().unwrap())[0]
                .clone();
            let ty = lu_dog
                .read()
                .unwrap()
                .exhume_value_type(&woog_struct.read().unwrap().id)
                .unwrap();
            let fields = woog_struct
                .read()
                .unwrap()
                .r7_field(&lu_dog.read().unwrap());

            // Type checking fields here
            let mut user_type = UserType::new(&ty, context);
            for (name, ty, value, expr) in field_exprs {
                if let Some(field) = fields.iter().find(|f| f.read().unwrap().name == name) {
                    let struct_ty = lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&field.read().unwrap().ty)
                        .unwrap();

                    let x_value = &expr.read().unwrap().r11_x_value(&lu_dog.read().unwrap())[0];
                    let span = &x_value.read().unwrap().r63_span(&lu_dog.read().unwrap())[0];

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
                Arc::new(RwLock::new(Value::UserType(Arc::new(RwLock::new(
                    user_type,
                ))))),
                ty,
            ))
        }
        //
        // TypeCast
        //
        Expression::TypeCast(ref expr) => {
            let sarzak = context.sarzak.clone();

            let expr = lu_dog.read().unwrap().exhume_type_cast(expr).unwrap();
            debug!("Expression::TypeCast", expr);

            let lhs = expr.read().unwrap().r68_expression(&lu_dog.read().unwrap())[0].clone();
            let as_ty = expr.read().unwrap().r69_value_type(&lu_dog.read().unwrap())[0].clone();

            let (lhs, _lhs_ty) = eval_expression(lhs, context)?;

            let value = match &*as_ty.read().unwrap() {
                ValueType::Ty(ref ty) => {
                    // 🚧 WTF?
                    let ty = sarzak.read().unwrap().exhume_ty(ty).unwrap().clone();
                    match ty {
                        Ty::Float(_) => {
                            let value: f64 = (&*lhs.read().unwrap()).try_into()?;
                            Arc::new(RwLock::new(Value::Float(value)))
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
            let expr = lu_dog
                .read()
                .unwrap()
                .exhume_variable_expression(expr)
                .unwrap();
            debug!("Expression::VariableExpression", expr);
            let value = context.stack.get(&expr.read().unwrap().name);

            ensure!(value.is_some(), {
                let var = expr.read().unwrap().name.clone();
                VariableNotFoundSnafu { var }
            });

            let value = value.unwrap();

            no_debug!("Expression::VariableExpression", value.read().unwrap());

            let ty = value.read().unwrap().get_type(&lu_dog.read().unwrap());

            // 🚧
            // Cloning the value isn't going to cut it I don't think. There are
            // three cases to consider. One is when the value is used read-only.
            // Cloning is fine here. If the value is mutated however, we would
            // either need to return a reference, or write the value when it's
            // modified. The third thing is when the value is a reference. By
            // that I mean the type (above) is a reference. Not even sure what
            // to think about that atm.
            Ok((value.clone(), ty))
        }
        //
        // XIf
        //
        Expression::XIf(ref expr) => {
            let expr = lu_dog.read().unwrap().exhume_x_if(expr).unwrap();
            let expr = expr.read().unwrap();
            debug!("Expression::XIf", expr);

            let cond_expr = lu_dog
                .read()
                .unwrap()
                .exhume_expression(&expr.test)
                .unwrap();

            let (cond, _ty) = eval_expression(cond_expr, context)?;
            debug!("Expression::XIf conditional", cond);

            let cond = cond.read().unwrap();
            Ok(if (&*cond).try_into()? {
                // Evaluate the true block
                let block = lu_dog
                    .read()
                    .unwrap()
                    .exhume_expression(&expr.true_block)
                    .unwrap();
                eval_expression(block, context)?
            } else {
                debug!("Expression::XIf else");
                if let Some(expr) = &expr.false_block {
                    debug!("Expression::XIf false block");
                    // Evaluate the false block
                    let block = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                    eval_expression(block, context)?
                } else {
                    (
                        Arc::new(RwLock::new(Value::Empty)),
                        ValueType::new_empty(&lu_dog.read().unwrap()),
                    )
                }
            })
        }
        //
        // XReturn
        //
        Expression::XReturn(ref expr) => {
            let expr = lu_dog.read().unwrap().exhume_x_return(expr).unwrap();
            debug!("Expression::XReturn", expr);

            let expr = &expr.read().unwrap().expression;
            let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();

            let (value, ty) = eval_expression(expr, context)?;
            Err(ChaChaError::Return {
                value: value,
                ty: ty,
            })
        }
        //
        // ZNone
        //
        Expression::ZNone(_) => Ok((
            Arc::new(RwLock::new(Value::Empty)),
            ValueType::new_empty(&lu_dog.read().unwrap()),
        )),
        //
        // ZSome
        //
        Expression::ZSome(ref some) => {
            let some = lu_dog.read().unwrap().exhume_z_some(some).unwrap();
            debug!("Expression::ZSome", some);

            let value = &some.read().unwrap().r23_x_value(&lu_dog.read().unwrap())[0];
            let option = &some.read().unwrap().r3_woog_option(&lu_dog.read().unwrap())[0];
            let ty = &option
                .read()
                .unwrap()
                .r2_value_type(&lu_dog.read().unwrap())[0];

            let value = match value.read().unwrap().subtype {
                XValueEnum::Expression(ref expr) => {
                    let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                    let (value, _ty) = eval_expression(expr, context)?;
                    value
                }
                XValueEnum::Variable(ref var) => {
                    let var = lu_dog.read().unwrap().exhume_variable(var).unwrap();
                    Arc::new(RwLock::new(Value::Empty))
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
                Arc::new(RwLock::new(Value::Empty)),
                ValueType::new_empty(&lu_dog.read().unwrap()),
            ))
        }
    }
}

pub fn eval_statement(
    statement: Arc<RwLock<Statement>>,
    context: &mut Context,
) -> Result<(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>)> {
    let lu_dog = context.lu_dog.clone();

    debug!("eval_statement statement", statement);
    trace!("eval_statement stack", context.stack);

    match statement.read().unwrap().subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog
                .read()
                .unwrap()
                .exhume_expression_statement(stmt)
                .unwrap();
            let stmt = stmt.read().unwrap();
            let expr = stmt.r31_expression(&lu_dog.read().unwrap())[0].clone();
            let (value, ty) = eval_expression(expr, context)?;
            no_debug!(
                "StatementEnum::ExpressionStatement: value",
                value.read().unwrap()
            );
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            Ok((value, ty))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.read().unwrap().exhume_let_statement(stmt).unwrap();
            let stmt = stmt.read().unwrap();
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&lu_dog.read().unwrap())[0].clone();
            debug!("expr", expr);

            let (value, ty) = eval_expression(expr, context)?;
            debug!("value", value);
            debug!("ty", ty);

            let var = stmt.r21_local_variable(&lu_dog.read().unwrap())[0]
                .read()
                .unwrap()
                .clone();
            let var = var.r12_variable(&lu_dog.read().unwrap())[0]
                .read()
                .unwrap()
                .clone();
            debug!("var", var);

            log::debug!("inserting {} = {}", var.name, value.read().unwrap());
            context.stack.insert(var.name, value);

            Ok((Arc::new(RwLock::new(Value::Empty)), ty))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog
                .read()
                .unwrap()
                .exhume_result_statement(stmt)
                .unwrap();
            let stmt = stmt.read().unwrap();
            debug!("StatementEnum::ResultStatement: stmt", stmt);

            let expr = stmt.r41_expression(&lu_dog.read().unwrap())[0].clone();
            debug!("StatementEnum::ResultStatement expr", expr);

            let (value, ty) = eval_expression(expr, context)?; // {
                                                               //     Ok((value, ty)) => (value, ty),
                                                               //     Err(ChaChaError::Return { value, ty }) => (value, ty),
                                                               //     Err(e) => return Err(e),
                                                               // };

            debug!("StatementEnum::ResultStatement value", value);
            debug!("StatementEnum::ResultStatement ty", ty);

            Ok((value, ty))
        }
        ref beta => {
            error!("deal with statement", beta);
            Ok((
                Arc::new(RwLock::new(Value::Empty)),
                ValueType::new_empty(&lu_dog.read().unwrap()),
            ))
        }
    }
}

#[derive(Clone)]
pub struct Context {
    prompt: String,
    block: Arc<RwLock<Block>>,
    stack: Memory,
    lu_dog: Arc<RwLock<LuDogStore>>,
    sarzak: Arc<RwLock<SarzakStore>>,
    models: Arc<RwLock<Vec<SarzakStore>>>,
    mem_update_recv: Receiver<MemoryUpdateMessage>,
    std_out_send: Sender<String>,
    std_out_recv: Receiver<String>,
    debug_status_writer: Option<Sender<DebuggerStatus>>,
    obj_file_path: Option<PathBuf>,
}

/// Save the lu_dog model when the context is dropped
///
/// NB: This doesn't work. The thread that started us apparently goes away
/// before we get a chance to run this to completion. That's my current
/// working hypothesis.
impl Drop for Context {
    fn drop(&mut self) {
        // self.lu_dog
        //     .read()
        //     .unwrap()
        //     .persist_bincode(&self.obj_file_path)
        //     .unwrap();
    }
}

impl Context {
    pub fn register_model<P: AsRef<Path>>(&self, model_path: P) -> Result<()> {
        let model =
            SarzakStore::load(model_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

        self.models.write().unwrap().push(model);

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
        &self.stack
    }

    pub fn prompt(&self) -> &str {
        &self.prompt
    }

    pub fn source(&self) -> String {
        let source = self
            .lu_dog
            .read()
            .unwrap()
            .iter_dwarf_source_file()
            .next()
            .unwrap();
        let source = source.read().unwrap();
        source.source.clone()
    }

    pub fn lu_dog_heel(&self) -> Arc<RwLock<LuDogStore>> {
        self.lu_dog.clone()
    }

    pub fn block(&self) -> Arc<RwLock<Block>> {
        self.block.clone()
    }

    pub fn sarzak_heel(&self) -> Arc<RwLock<SarzakStore>> {
        self.sarzak.clone()
    }

    pub fn models(&self) -> Arc<RwLock<Vec<SarzakStore>>> {
        self.models.clone()
    }

    pub fn register_store_proxy(&mut self, name: String, proxy: impl StoreProxy + 'static) {
        self.stack.insert_global(
            name.clone(),
            Arc::new(RwLock::new(Value::ProxyType(Arc::new(RwLock::new(proxy))))),
        );

        let mut lu_dog = self.lu_dog.write().unwrap();
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
        //     let local = LocalVariable::new(Uuid::new_v4(), &mut *lu_dog.write().unwrap());
        //     let var = Variable::new_local_variable(
        //         "MERLIN_STORE".to_owned(),
        //         local,
        //         &mut *lu_dog.write().unwrap(),
        //     );

        //     let store = ZObjectStore::new("merlin".to_owned(), &mut *lu_dog.write().unwrap());
        //     let mut write = lu_dog.write().unwrap();
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
    Stopped(Arc<RwLock<Value>>, Arc<RwLock<ValueType>>),
}

pub enum DebuggerControl {
    ExecuteInput(String),
    Run,
    SetBreakpoint(usize),
    StepInto,
    StepOver,
    Stop,
}

pub fn start_repl2(mut context: Context) -> (Sender<DebuggerControl>, Receiver<DebuggerStatus>) {
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
        .spawn(move || loop {
            match to_worker_read.recv_timeout(Duration::from_millis(10)) {
                Ok(input) => {
                    if let Some((stmt, _span)) = parse_line(&input) {
                        let lu_dog = context.lu_dog_heel();
                        let block = context.block();
                        let sarzak = context.sarzak_heel();
                        let models = context.models();

                        let stmt = {
                            let mut writer = lu_dog.write().unwrap();
                            match inter_statement(
                                &Arc::new(RwLock::new(stmt)),
                                &DwarfSourceFile::new(input, &mut writer),
                                &block,
                                &mut writer,
                                &models.read().unwrap(),
                                &sarzak.read().unwrap(),
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

                        match eval_statement(stmt, &mut context) {
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
                }
                Err(RecvTimeoutError::Timeout) => {}
                Err(_) => {
                    debug!("Worker thread exiting");
                    break;
                }
            }
        })
        .unwrap();

    (from_ui_write, to_ui_read)
}

/// This runs the main function, assuming it exists. It should really return
/// Ok(Value) I think.
pub fn start_main(stopped: bool, mut context: Context) -> Result<Value, Error> {
    {
        let mut running = RUNNING.lock();
        *running = !stopped;
    }

    let stack = &mut context.stack;

    let main = stack.get("main").expect("Missing main function.");

    dbg!("foo");
    // This should fail if it's not a function. Actually, I think that it _has_
    // to be a function. Unless there's another named item that I'm not thinking
    // of. I mean, maybe `use main;`  would trigger this to return OK(()), and
    // not do anything?
    let result = if let Value::Function(ref main) = *main.read().unwrap() {
        let main = context
            .lu_dog
            .read()
            .unwrap()
            .exhume_function(&main.read().unwrap().id)
            .unwrap();

        dbg!("uber");
        let thread_join_handle = thread::spawn(move || {
            let result = eval_function_call(main, &[], &mut context);
            result
        });

        // this is lame
        thread::sleep(Duration::from_millis(1));

        dbg!("baz");
        CVAR.notify_all();

        dbg!("wtf");
        // some work here
        let result = thread_join_handle.join().unwrap().unwrap();

        dbg!("finally");
        // let result = thread::scope(|s| {
        //     s.spawn(move |_| {
        //         let result = eval_function_call(main, &[], &mut context);
        //         result
        //     })
        // })
        // .unwrap();
        // let result = result.join().unwrap()?;

        // let result = eval_function_call(main, &[], &mut context)?;

        Ok(result.0.clone().read().unwrap().clone())
    } else {
        Err(Error(ChaChaError::MainIsNotAFunction))
    };

    dbg!("bar");

    result
}

pub fn start_vm(n: DwarfInteger) -> Result<DwarfInteger, Error> {
    let (mut memory, _) = Memory::new();
    let mut chunk = Chunk::new("fib".to_string());

    chunk.add_variable("n".to_owned());

    // Get the parameter off the stack
    chunk.add_instruction(Instruction::FetchLocal(0));
    chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
    // Chcek if it's <= 1
    chunk.add_instruction(Instruction::LessThanOrEqual);
    chunk.add_instruction(Instruction::JumpIfFalse(2));
    // If false return 1
    chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
    chunk.add_instruction(Instruction::Return);
    // return fidbn-1) + fib(n-2)
    // Load fib
    chunk.add_instruction(Instruction::Constant(Value::Chunk("fib", 0)));
    // load n
    chunk.add_instruction(Instruction::FetchLocal(0));
    // load 1
    chunk.add_instruction(Instruction::Constant(Value::Integer(1)));
    // subtract
    chunk.add_instruction(Instruction::Subtract);
    // Call fib(n-1)
    chunk.add_instruction(Instruction::Call(1));
    // load fib
    chunk.add_instruction(Instruction::Constant(Value::Chunk("fib", 0)));
    // load n
    chunk.add_instruction(Instruction::FetchLocal(0));
    // load 2
    chunk.add_instruction(Instruction::Constant(Value::Integer(2)));
    // subtract
    chunk.add_instruction(Instruction::Subtract);
    // Call fib(n-1)
    chunk.add_instruction(Instruction::Call(1));
    // add
    chunk.add_instruction(Instruction::Add);
    chunk.add_instruction(Instruction::Return);

    // put fib in memory
    let slot = memory.reserve_chunk_slot();
    memory.insert_chunk(chunk.clone(), slot);

    let frame = CallFrame::new(0, 0, &chunk);

    let mut vm = VM::new(&memory);

    // Push the func
    vm.push_stack(Value::String("fib".to_string()));
    // Push the argument
    vm.push_stack(Value::Integer(n));

    vm.push_frame(frame);

    let result = vm.run(false);

    let result: DwarfInteger = result.unwrap().try_into().unwrap();

    Ok(result)
}

#[cfg(feature = "repl")]
pub fn start_repl(mut context: Context) -> Result<(), Error> {
    use std::io::{self, Write};

    use rustyline::error::ReadlineError;
    use rustyline::validate::{ValidationContext, ValidationResult, Validator};
    use rustyline::{Completer, Helper, Highlighter, Hinter};
    use rustyline::{Editor, Result};

    let models = context.models.clone();
    let lu_dog = context.lu_dog.clone();
    let sarzak = context.sarzak.clone();

    let block = context.block.clone();
    // let stack = &mut context.stack;

    let error_style = Colour::Red;
    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let type_style = Colour::Blue.italic().dimmed();

    #[derive(Completer, Helper, Highlighter, Hinter)]
    struct DwarfValidator {}

    impl DwarfValidator {
        fn validate(&self, input: &str) -> ValidationResult {
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
                io::stdout().flush();
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
                } else if let Some((stmt, _span)) = parse_line(&line) {
                    debug!("stmt from readline", stmt);

                    let stmt = {
                        let mut writer = lu_dog.write().unwrap();
                        match inter_statement(
                            &Arc::new(RwLock::new(stmt)),
                            &DwarfSourceFile::new(line.clone(), &mut writer),
                            &block,
                            &mut writer,
                            &models.read().unwrap(),
                            &sarzak.read().unwrap(),
                        ) {
                            Ok(stmt) => stmt.0,
                            Err(e) => {
                                println!("{}", e);
                                continue;
                            }
                        }
                    };

                    // 🚧 This needs fixing too.
                    let eval = eval_statement(stmt, &mut context);
                    // for i in context.drain_std_out() {
                    //     println!("{}", i);
                    // }
                    match eval {
                        Ok((value, ty)) => {
                            let value = format!("{}", value.read().unwrap());
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

pub(crate) struct PrintableValueType<'a>(pub &'a Arc<RwLock<ValueType>>, pub &'a Context);

impl<'a> fmt::Display for PrintableValueType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.0.read().unwrap();
        let context = self.1;
        let lu_dog = &context.lu_dog;
        let sarzak = &context.sarzak;
        let model = &context.models;

        match &*value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = lu_dog.read().unwrap().exhume_import(import).unwrap();
                let import = import.read().unwrap();
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = lu_dog.read().unwrap().exhume_list(list).unwrap();
                let list = list.read().unwrap();
                let ty = list.r36_value_type(&lu_dog.read().unwrap())[0].clone();
                write!(f, "[{}]", PrintableValueType(&ty, context))
            }
            ValueType::Range(_) => write!(f, "<range>"),
            ValueType::Reference(ref reference) => {
                let reference = lu_dog.read().unwrap().exhume_reference(reference).unwrap();
                let reference = reference.read().unwrap();
                let ty = reference.r35_value_type(&lu_dog.read().unwrap())[0].clone();
                write!(f, "&{}", PrintableValueType(&ty, context))
            }
            ValueType::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                let sarzak = sarzak.read().unwrap();
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
                    let models = model.read().unwrap();
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
                let option = lu_dog.read().unwrap().exhume_woog_option(option).unwrap();
                let option = option.read().unwrap();
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.read().unwrap().exhume_z_some(some).unwrap();
                        let some = some.read().unwrap();
                        let value = some.r23_x_value(&lu_dog.read().unwrap())[0]
                            .read()
                            .unwrap()
                            .clone();
                        let ty = value.r24_value_type(&lu_dog.read().unwrap())[0].clone();
                        write!(f, "Some({})", PrintableValueType(&ty, context))
                    }
                }
            }
            ValueType::WoogStruct(ref woog_struct) => {
                let woog_struct = lu_dog
                    .read()
                    .unwrap()
                    .exhume_woog_struct(woog_struct)
                    .unwrap();
                debug!("woog_struct", woog_struct);
                let woog_struct = woog_struct.read().unwrap();
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.read().unwrap().exhume_z_object_store(id).unwrap();
                let zobject_store = zobject_store.read().unwrap();
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}

pub(crate) struct ChunkReservation {
    slot: usize,
}

type MemoryCell = (String, Arc<RwLock<Value>>);

#[derive(Clone, Debug)]
pub enum MemoryUpdateMessage {
    AddGlobal(MemoryCell),
    AddMeta(MemoryCell),
    PushFrame,
    PopFrame,
    AddLocal(MemoryCell),
}

impl fmt::Display for MemoryUpdateMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemoryUpdateMessage::AddGlobal((name, value)) => {
                write!(f, "add global {}: {}", name, value.read().unwrap())
            }
            MemoryUpdateMessage::AddMeta((name, value)) => {
                write!(f, "add meta {}: {}", name, value.read().unwrap())
            }
            MemoryUpdateMessage::PushFrame => write!(f, "push frame"),
            MemoryUpdateMessage::PopFrame => write!(f, "pop frame"),
            MemoryUpdateMessage::AddLocal((name, value)) => {
                write!(f, "add local {}: {}", name, value.read().unwrap())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Memory {
    chunks: Vec<Chunk>,
    meta: HashMap<String, HashMap<String, Arc<RwLock<Value>>>>,
    global: HashMap<String, Arc<RwLock<Value>>>,
    frames: Vec<HashMap<String, Arc<RwLock<Value>>>>,
    sender: Sender<MemoryUpdateMessage>,
}

impl Memory {
    pub(crate) fn new() -> (Self, Receiver<MemoryUpdateMessage>) {
        let (sender, receiver) = unbounded();

        (
            Memory {
                chunks: Vec::new(),
                meta: HashMap::default(),
                global: HashMap::default(),
                frames: vec![HashMap::default()],
                sender,
            },
            receiver,
        )
    }

    pub fn get_globals(&self) -> Vec<(&str, &Arc<RwLock<Value>>)> {
        self.global
            .iter()
            .map(|(name, value)| (name.as_str(), value))
            .collect()
    }

    pub fn get_frames(&self) -> Vec<Vec<(&str, &Arc<RwLock<Value>>)>> {
        self.frames
            .iter()
            .map(|frame| {
                frame
                    .iter()
                    .map(|(name, value)| (name.as_str(), value))
                    .collect()
            })
            .collect()
    }

    pub(crate) fn chunk_index<S: AsRef<str>>(&self, name: S) -> Option<usize> {
        self.chunks
            .iter()
            .enumerate()
            .find(|(_, chunk)| chunk.name == name.as_ref())
            .map(|(index, _)| index)
    }

    pub(crate) fn reserve_chunk_slot(&mut self) -> ChunkReservation {
        let slot = self.chunks.len();
        self.chunks.push(Chunk::new("placeholder".to_string()));
        ChunkReservation { slot }
    }

    pub(crate) fn insert_chunk(&mut self, chunk: Chunk, reservation: ChunkReservation) {
        self.chunks[reservation.slot] = chunk;
    }

    pub(crate) fn get_chunk(&self, index: usize) -> Option<&Chunk> {
        self.chunks.get(index)
    }

    pub(crate) fn push(&mut self) {
        self.sender.send(MemoryUpdateMessage::PushFrame).unwrap();
        self.frames.push(HashMap::default());
    }

    pub(crate) fn pop(&mut self) {
        self.sender.send(MemoryUpdateMessage::PopFrame).unwrap();
        self.frames.pop();
    }

    pub(crate) fn insert_meta_table(&mut self, table: String) {
        self.meta.insert(table, HashMap::default());
    }

    pub(crate) fn insert_meta(&mut self, table: &str, name: String, value: Arc<RwLock<Value>>) {
        let table = self.meta.get_mut(table).unwrap();
        table.insert(name, value);
    }

    pub(crate) fn get_meta(&self, table: &str, name: &str) -> Option<Arc<RwLock<Value>>> {
        if let Some(table) = self.meta.get(table) {
            if let Some(val) = table.get(name) {
                Some(val.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    // 🚧 Document this -- I'm not sure what the :: business is about.
    pub(crate) fn insert_global(&mut self, name: String, value: Arc<RwLock<Value>>) {
        if name.contains("::") {
            let mut split = name.split("::");
            let table = split.next().unwrap();
            let name = split
                .next()
                .expect("name contained `::`, but no second element");

            self.sender
                .send(MemoryUpdateMessage::AddGlobal((
                    name.to_owned(),
                    value.clone(),
                )))
                .unwrap();

            if let Some(value) = self.global.get(table) {
                let mut write_value = value.write().unwrap();
                if let Value::Table(ref mut table) = *write_value {
                    table.insert(name.to_owned(), value.clone());
                } else {
                    unreachable!()
                }
            } else {
                let mut map = HashMap::default();
                map.insert(name.to_owned(), value.clone());
                self.global
                    .insert(table.to_owned(), Arc::new(RwLock::new(Value::Table(map))));
            }
        } else {
            self.global.insert(name, value);
        }
    }

    pub(crate) fn insert(&mut self, name: String, value: Arc<RwLock<Value>>) {
        self.sender
            .send(MemoryUpdateMessage::AddLocal((name.clone(), value.clone())))
            .unwrap();
        let frame = self.frames.last_mut().unwrap();
        frame.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<Arc<RwLock<Value>>> {
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(value) = self.get_simple(table) {
                let value = value.read().unwrap();
                if let Value::Table(ref table) = *value {
                    if let Some(val) = table.get(name) {
                        Some(val.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            if let Some(value) = self.get_simple(name) {
                Some(value.clone())
            } else {
                None
            }
        }
    }

    fn get_simple(&self, name: &str) -> Option<&Arc<RwLock<Value>>> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        self.global.get(name)
    }
}

fn typecheck(
    lhs: &Arc<RwLock<ValueType>>,
    rhs: &Arc<RwLock<ValueType>>,
    span: &Arc<RwLock<Span>>,
    context: &Context,
) -> Result<()> {
    match (&*lhs.read().unwrap(), &*rhs.read().unwrap()) {
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
                    span: span.read().unwrap().start as usize..span.read().unwrap().end as usize,
                })
            }
        }
    }
}
