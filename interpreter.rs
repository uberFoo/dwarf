use std::{
    collections::VecDeque,
    fmt,
    path::Path,
    sync::{Arc, RwLock},
};

use ansi_term::Colour;
use fxhash::FxHashMap as HashMap;
use heck::ToUpperCamelCase;
use lazy_static::lazy_static;
use log;
use sarzak::{
    dwarf::{inter_statement, parse_line},
    lu_dog::{
        Argument, Block, CallEnum, Expression, Function, Literal, LocalVariable,
        ObjectStore as LuDogStore, Statement, StatementEnum, Value as LuDogValue, ValueType,
        Variable, WoogOptionEnum,
    },
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
};
use snafu::{location, prelude::*, Location};
use uuid::Uuid;

#[cfg(feature = "repl")]
use rustyline::{error::ReadlineError, DefaultEditor};

use crate::{
    value::{StoreProxy, UserType},
    BadJuJuSnafu, ChaChaError, Error, NoSuchFieldSnafu, NoSuchStaticMethodSnafu, Result,
    TypeMismatchSnafu, UnimplementedSnafu, Value, WrongNumberOfArgumentsSnafu,
};

const BANNER: &str = r#"
   ________          ________             _       __                            __
  / ____/ /_  ____ _/ ____/ /_  ____ _   (_)___  / /____  _________  ________  / /____  _____
 / /   / __ \/ __ `/ /   / __ \/ __ `/  / / __ \/ __/ _ \/ ___/ __ \/ ___/ _ \/ __/ _ \/ ___/
/ /___/ / / / /_/ / /___/ / / / /_/ /  / / / / / /_/  __/ /  / /_/ / /  /  __/ /_/  __/ /
\____/_/ /_/\__,_/\____/_/ /_/\__,_/  /_/_/ /_/\__/\___/_/  / .___/_/   \___/\__/\___/_/
                        __              __                 /_/____
   ____ ___  ____  ____/ /__  _    ____/ /      ______ ______/ __/
  / __ `__ \/ __ \/ __  / _ \(_)  / __  / | /| / / __ `/ ___/ /_
 / / / / / / /_/ / /_/ /  __/    / /_/ /| |/ |/ / /_/ / /  / __/
/_/ /_/ /_/\____/\__,_/\___(_)   \__,_/ |__/|__/\__,_/_/  /_/
                                                                                             "#;

macro_rules! error {
    ($arg:expr) => {
        log::error!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::error!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Red.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
    };
}

macro_rules! debug {
    ($arg:expr) => {
        log::debug!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
    ($msg:literal, $arg:expr) => {
        log::debug!(
            "{} --> {:?}\n  --> {}:{}:{}",
            Colour::Yellow.paint($msg),
            $arg,
            file!(),
            line!(),
            column!()
        );
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

macro_rules! dereference {
    ($referrer:expr, $field:expr, $store:expr) => {
        let ptr = &$referrer.read().unwrap().$field;
        $store.exhume_$field(ptr).unwrap()
    };
}

lazy_static! {
    pub(crate) static ref MODELS: Arc<RwLock<Vec<SarzakStore>>> = Arc::new(RwLock::new(Vec::new()));
    pub(crate) static ref LU_DOG: Arc<RwLock<LuDogStore>> =
        Arc::new(RwLock::new(LuDogStore::new()));
    pub(crate) static ref SARZAK: Arc<RwLock<SarzakStore>> =
        Arc::new(RwLock::new(SarzakStore::new()));
}

pub fn initialize_interpreter<P: AsRef<Path>>(
    sarzak_path: P,
    lu_dog_path: P,
) -> Result<Context, Error> {
    let sarzak =
        SarzakStore::load(sarzak_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

    // This will always be a lu-dog, but it's basically a compiled dwarf file.
    // let mut lu_dog = LuDogStore::load("../sarzak/target/sarzak/lu_dog")
    let mut lu_dog =
        LuDogStore::load(lu_dog_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

    // This won't always be Lu-Dog, clearly. So we'll need to be sure to also
    // generate some code that imports the types from the model.
    // let model = SarzakStore::load("../sarzak/models/lu_dog.v2.json")
    //
    // This one snuck in. It should be registerd as part of the code generation.
    // let model = SarzakStore::load("../sarzak/models/merlin.v2.json")
    //     .map_err(|e| InnerError::Store { source: e })?;

    // Initialize the stack with stuff from the compiled source.
    let block = Block::new(Uuid::new_v4(), &mut lu_dog);
    let mut stack = Stack::new();

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
            // 🚧 What's up with the type below?
            let _value =
                LuDogValue::new_variable(&block, &ValueType::new_empty(&lu_dog), &var, &mut lu_dog);

            log::trace!("inserting local function {}", name);
            stack.insert(name, value);
        }
    }

    // Insert static methods for each struct. They go into the meta table.
    for user_type in lu_dog.iter_woog_struct() {
        let user_type = user_type.read().unwrap();
        // Create a meta table for each struct.
        stack.insert_meta_table(user_type.name.to_owned());
        let impl_ = user_type.r8c_implementation(&lu_dog);
        if !impl_.is_empty() {
            // For each function in the impl, insert the function. I should probably
            // check and only insert the static functions.
            // 🚧 Only insert the static functions
            for func in impl_[0].read().unwrap().r9_function(&lu_dog) {
                stack.insert_meta(
                    &user_type.name,
                    func.read().unwrap().name.to_owned(),
                    // It's here that I'd really like to be able to do a cheap
                    // clone of an Rc.
                    Value::Function(func.clone()),
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
    *SARZAK.write().unwrap() = sarzak;
    *LU_DOG.write().unwrap() = lu_dog;
    // *MODEL.write().unwrap() = model;

    Ok(Context {
        stack,
        block,
        lu_dog: LU_DOG.clone(),
    })
}

fn eval_function_call(
    func: Arc<RwLock<Function>>,
    args: &[Arc<RwLock<Argument>>],
    stack: &mut Stack,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    let lu_dog = &LU_DOG;
    let sarzak = &SARZAK;

    debug!("eval_function_call func ", func);
    trace!("eval_function_call stack", stack);

    let func = func.read().unwrap();
    let block = lu_dog.read().unwrap().exhume_block(&func.block).unwrap();
    let block = block.read().unwrap();
    let stmts = block.r18_statement(&lu_dog.read().unwrap());

    if !stmts.is_empty() {
        stack.push();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = func.r13_parameter(&lu_dog.read().unwrap());
        // Damn, this really needs to return a Result.
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
                let value = var.r11_value(&lu_dog.read().unwrap())[0]
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
                let (value, ty) = eval_expression(expr, stack)?;
                arg_values.push((value, ty));

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
        for ((name, param_ty), (value, arg_ty)) in zipped {
            debug!("eval_function_call type check name", name);
            debug!("eval_function_call type param_ty", param_ty);
            debug!("eval_function_call type check value", value);
            debug!("eval_function_call type check arg_ty", arg_ty);

            ensure!(*param_ty.read().unwrap() == *arg_ty.read().unwrap(), {
                let expected = PrintableValueType(param_ty).to_string();
                let got = PrintableValueType(arg_ty).to_string();
                TypeMismatchSnafu { expected, got }
            });

            // Insert the parameter into the frame.
            stack.insert(name, value);
        }

        let mut value;
        let mut ty;
        // This is a pain.
        // Find the first statement, by looking for the one with no previous statement.
        let mut next = stmts
            .iter()
            .find(|s| s.read().unwrap().r17c_statement(lu_dog).is_empty())
            .unwrap()
            .clone();

        loop {
            (value, ty) = eval_statement(next.clone(), stack, lu_dog, sarzak)?;
            if let Some(ref id) = next.clone().read().unwrap().next {
                next = lu_dog.exhume_statement(id).unwrap();
            } else {
                break;
            }
        }

        // Clean up
        stack.pop();

        Ok((value, ty))
    } else {
        Ok((Value::Empty, ValueType::new_empty(lu_dog)))
    }
}

fn eval_expression(
    expression: Arc<RwLock<Expression>>,
    stack: &mut Stack,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    let lu_dog = &LU_DOG;
    let sarzak = &SARZAK;

    debug!("eval_expression: expression", expression);
    trace!("eval_expression: stack", stack);

    let result_style = Colour::Green.bold();

    match *expression.read().unwrap() {
        //
        // Call
        //
        Expression::Call(ref call) => {
            let call = lu_dog.read().unwrap().exhume_call(call).unwrap();
            let call = call.read().unwrap();
            debug!("call", call);
            let args = call.r28_argument(lu_dog.read().unwrap());
            debug!("args", args);

            // This optional expression is the LHS of the call.
            let (value, ty) = if let Some(ref expr) = call.expression {
                let expr = lu_dog.read().unwrap().exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the function.
                let (value, ty) = eval_expression(expr, stack, lu_dog, sarzak)?;
                no_debug!("Expression::Call LHS value", value);
                debug!("Expression::Call LHS ty", ty);
                // So now value is pointing a a legit Function. We need to jump
                // through all sorts of hoops now. We need to setup a new stack
                // frame, and push the old one on to a stack that doesn't exist
                // yet. Then we need to eval all the arguments and put them in
                // the frame. And then we need to eval the statements in the
                // function body.

                // Or we can just call the function we already wrote!
                match &value {
                    Value::Function(ref func) => {
                        let func = lu_dog
                            .read()
                            .unwrap()
                            .exhume_function(&func.read().unwrap().id)
                            .unwrap();
                        debug!("Expression::Call func", func);
                        let (value, ty) = eval_function_call(func, &args, stack, lu_dog, sarzak)?;
                        debug!("value", value);
                        debug!("ty", ty);
                        (value, ty)
                    }
                    Value::ProxyType(pt) => {
                        let ty = lu_dog
                            .read()
                            .unwrap()
                            .exhume_value_type(&pt.read().unwrap().get_struct_uuid())
                            .unwrap();
                        (value.clone(), ty)
                    }
                    Value::UserType(ut) => (value.clone(), ut.read().unwrap().get_type().clone()),
                    value => {
                        panic!("dereferenced function expression and found this: {}", value);
                    }
                }
            } else {
                (Value::Empty, ValueType::new_empty(lu_dog.read().unwrap()))
            };

            // So we need to figure out the type this is being called on.
            match (&call.subtype, value, ty) {
                //
                // FunctionCall
                //
                (CallEnum::FunctionCall(_), value, ty) => Ok((value, ty)),
                //
                // MethodCall
                //
                (CallEnum::MethodCall(meth), mut value, ty) => {
                    let meth = lu_dog.read().unwrap().exhume_method_call(meth).unwrap();
                    let meth = &meth.read().unwrap().name;
                    debug!("MethodCall method", meth);
                    debug!("MethodCall value", value);
                    debug!("MethodCall type", ty);

                    match &mut value {
                        Value::ProxyType(pt) => {
                            let arg_values = if !args.is_empty() {
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
                                    let (value, _ty) = eval_expression(expr, stack)?;
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

                            pt.write().unwrap().call(meth, arg_values)
                        }
                        // Value::UserType(ut) => {}
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
                    let arg_values = if !args.is_empty() {
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
                            let (value, _ty) = eval_expression(expr, stack)?;
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

                        Ok((value, ty))
                    } else if let Some(value) = stack.get_meta(ty, func) {
                        debug!("StaticMethodCall meta value", value);
                        match &value {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall meta func", func);
                                let (value, ty) = eval_function_call(func, &args, stack)?;
                                debug!("StaticMethodCall meta value", value);
                                debug!("StaticMethodCall meta ty", ty);
                                Ok((value, ty))
                            }
                            value => {
                                error!("deal with call expression", value);
                                Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
                            }
                        }
                    } else if let Some(mut value) = stack.get_mut(ty) {
                        debug!("StaticMethodCall frame value", value);
                        match &mut value {
                            Value::Function(ref func) => {
                                let func = lu_dog
                                    .read()
                                    .unwrap()
                                    .exhume_function(&func.read().unwrap().id)
                                    .unwrap();
                                debug!("StaticMethodCall frame func", func);
                                let (value, ty) = eval_function_call(func, &args, stack)?;
                                debug!("StaticMethodCall frame value", value);
                                debug!("StaticMethodCall frame ty", ty);
                                Ok((value, ty))
                            }
                            Value::ProxyType(ut) => {
                                debug!("StaticMethodCall proxy", ut);
                                ut.write().unwrap().call(func, arg_values)
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
                                Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
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

                        // We never will get here.
                        Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
                    }
                }
            }
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

            print!("\t{}", error.read().unwrap().span);

            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
        //
        // FieldAccess
        //
        Expression::FieldAccess(ref field) => {
            let field = lu_dog.read().unwrap().exhume_field_access(field).unwrap();

            debug!("FieldAccess field", field);

            let field_name = &field.read().unwrap().name;

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

            let (value, _ty) = eval_expression(expr, stack)?;

            match value {
                Value::ProxyType(value) => {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(field_name);
                    match value {
                        Ok(value) => {
                            let ty = value.get_type(&lu_dog.read().unwrap());

                            Ok((value.to_owned(), ty))
                        }
                        Err(e) => Err(e),
                    }
                }
                Value::UserType(value) => {
                    let value = value.read().unwrap();
                    let value = value.get_attr_value(field_name).unwrap();
                    let ty = value.get_type(&lu_dog.read().unwrap());

                    Ok((value.to_owned(), ty))
                }
                _ => Err(ChaChaError::BadJuJu {
                    message: "Bad value in field access".to_owned(),
                    location: location!(),
                }),
            }
        }
        //
        // Literal
        //
        Expression::Literal(ref literal) => {
            let literal = lu_dog.read().unwrap().exhume_literal(literal).unwrap();
            let z = match &*literal.read().unwrap() {
                //
                // FloatLiteral
                //
                Literal::FloatLiteral(ref literal) => {
                    let literal = lu_dog
                        .read()
                        .unwrap()
                        .exhume_float_literal(literal)
                        .unwrap();
                    let value = literal.read().unwrap().value;
                    let value = Value::Float(value);
                    let ty = Ty::new_float();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
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
                    let value = literal.read().unwrap().value;
                    let value = Value::Integer(value);
                    let ty = Ty::new_integer();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
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
                    let value = Value::String(literal.read().unwrap().value.clone());
                    let ty = Ty::new_s_string();
                    let ty = lu_dog.read().unwrap().exhume_value_type(&ty.id()).unwrap();

                    Ok((value, ty))
                }
                z => {
                    ensure!(
                        false,
                        UnimplementedSnafu {
                            message: format!("deal with literal expression: {:?}", z),
                        }
                    );

                    Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
                }
            };
            z
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
            let (value, _) = eval_expression(expr, stack)?;
            let result = format!("{}", value);
            let result = result.replace("\\n", "\n");
            print!("{}", result_style.paint(result));

            Ok((value, ValueType::new_empty(&lu_dog.read().unwrap())))
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
                    let (value, ty) = eval_expression(expr, stack)?;
                    debug!("StructExpression field value", value);
                    debug!("StructExpression field ty", ty);
                    Ok((f.read().unwrap().name.clone(), ty, value))
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
            let mut user_type = UserType::new(ty.clone());
            for (name, ty, value) in field_exprs {
                if let Some(field) = fields.iter().find(|f| f.read().unwrap().name == name) {
                    let struct_ty = lu_dog
                        .read()
                        .unwrap()
                        .exhume_value_type(&field.read().unwrap().ty)
                        .unwrap();
                    ensure!(*struct_ty.read().unwrap() == *ty.read().unwrap(), {
                        let expected = PrintableValueType(struct_ty).to_string();
                        let got = PrintableValueType(ty).to_string();
                        TypeMismatchSnafu { expected, got }
                    });

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

            Ok((Value::UserType(Arc::new(RwLock::new(user_type))), ty))
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
            debug!("expr", expr);
            let value = stack.get(&expr.read().unwrap().name);
            if let Some(value) = value {
                no_debug!("value", value);

                // We can grab the type from the value
                // let ty = match &value {
                //     Value::Empty => ValueType::new_empty(),
                //     Value::Function(ref func) => {
                //         let func = lu_dog.exhume_function(&func.read().unwrap().id).unwrap();
                //         debug!("VariableExpression get type func", func);
                //         let z = func.read().unwrap().r1_value_type(lu_dog)[0].clone();
                //         z
                //     }
                //     Value::Integer(ref int) => {
                //         debug!("VariableExpression get type for int", int);
                //         let ty = Ty::new_integer();
                //         lu_dog.exhume_value_type(&ty.id()).unwrap()
                //     }
                //     // Value::StoreType(ref store) => {
                //     //     debug!("VariableExpression get type for store", store);
                //     //     store.get_type()
                //     // }
                //     Value::String(ref str) => {
                //         debug!("VariableExpression get type for string", str);
                //         let ty = Ty::new_s_string();
                //         lu_dog.exhume_value_type(&ty.id()).unwrap()
                //     }
                //     Value::ProxyType(ref pt) => {
                //         no_debug!(
                //             "VariableExpression get type for proxy type",
                //             pt.read().unwrap()
                //         );
                //         lu_dog
                //             .exhume_value_type(&pt.read().unwrap().get_struct_uuid())
                //             .unwrap()
                //     }
                //     Value::UserType(ref ut) => {
                //         no_debug!(
                //             "VariableExpression get type for user type",
                //             ut.read().unwrap()
                //         );
                //         ut.read().unwrap().get_type().clone()
                //     }
                //     Value::Uuid(ref uuid) => {
                //         debug!("VariableExpression get type for uuid", uuid);
                //         let ty = Ty::new_s_uuid();
                //         lu_dog.exhume_value_type(&ty.id()).unwrap()
                //     }
                //     value => {
                //         error!("deal with variable expression", value);
                //         ValueType::new_empty()
                //     }
                // };

                let ty = value.get_type(&lu_dog.read().unwrap());

                // Cloning the value isn't going to cut it I don't think. There are
                // three cases to consider. One is when the value is used read-only.
                // Cloning is find here. If the value is mutated however, we would
                // either need to return a reference, or write the value when it's
                // modified. The third thing is when the value is a reference. By
                // that I mean the type (above) is a reference. Not even sure what
                // to think about that atm.
                Ok((value.clone(), ty))
            } else {
                println!(
                    "\t{} not found.",
                    Colour::Red.paint(&expr.read().unwrap().name)
                );
                Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
            }
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("deal with expression: {:?}", alpha),
                }
            );

            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
    }
}

fn eval_statement(
    statement: Arc<RwLock<Statement>>,
    stack: &mut Stack,
) -> Result<(Value, Arc<RwLock<ValueType>>)> {
    let lu_dog = &LU_DOG;
    let sarzak = &SARZAK;

    debug!("eval_statement statement", statement);
    trace!("eval_statement stack", stack);

    match statement.read().unwrap().subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog
                .read()
                .unwrap()
                .exhume_expression_statement(stmt)
                .unwrap();
            let stmt = stmt.read().unwrap();
            let expr = stmt.r31_expression(&lu_dog.read().unwrap())[0].clone();
            let (value, ty) = eval_expression(expr, stack)?;
            no_debug!("StatementEnum::ExpressionStatement: value", value);
            debug!("StatementEnum::ExpressionStatement: ty", ty);

            Ok((Value::Empty, ty))
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.read().unwrap().exhume_let_statement(stmt).unwrap();
            let stmt = stmt.read().unwrap();
            debug!("StatementEnum::LetStatement: stmt", stmt);

            let expr = stmt.r20_expression(&lu_dog.read().unwrap())[0].clone();
            debug!("expr", expr);

            let (value, ty) = eval_expression(expr, stack)?;
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

            log::debug!("inserting {} = {}", var.name, value);
            stack.insert(var.name, value);

            Ok((Value::Empty, ty))
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

            let (value, ty) = eval_expression(expr, stack)?;
            debug!("StatementEnum::ResultStatement value", value);
            debug!("StatementEnum::ResultStatement ty", ty);

            Ok((value, ty))
        }
        ref beta => {
            error!("deal with statement", beta);
            Ok((Value::Empty, ValueType::new_empty(&lu_dog.read().unwrap())))
        }
    }
}

pub struct Context {
    block: Arc<RwLock<Block>>,
    stack: Stack,
    lu_dog: Arc<RwLock<LuDogStore>>,
}

impl Context {
    pub fn register_model<P: AsRef<Path>>(&self, model_path: P) -> Result<()> {
        let model =
            SarzakStore::load(model_path.as_ref()).map_err(|e| ChaChaError::Store { source: e })?;

        MODELS.write().unwrap().push(model);

        Ok(())
    }

    pub fn lu_dog_heel(&self) -> Arc<RwLock<LuDogStore>> {
        self.lu_dog.clone()
    }

    pub fn register_store_proxy(&mut self, name: String, proxy: impl StoreProxy + 'static) {
        self.stack
            .insert_global(name, Value::ProxyType(Arc::new(RwLock::new(proxy))));
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

#[cfg(feature = "repl")]
pub fn start_repl(context: Context) -> Result<(), Error> {
    let models = &MODELS;
    let lu_dog = &LU_DOG;
    let sarzak = &SARZAK;

    let block = context.block;
    let mut stack = context.stack;

    let prompt_style = Colour::Blue.normal();
    let result_style = Colour::Yellow.italic().dimmed();
    let type_style = Colour::Blue.italic().dimmed();
    let error_style = Colour::Red.bold();
    let banner_style = Colour::Purple;

    println!("{}", banner_style.paint(BANNER));

    // `()` can be used when no completer is required
    let mut rl = DefaultEditor::new().map_err(|e| ChaChaError::RustyLine { source: e })?;

    // #[cfg(feature = "with-file-history")]
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(&format!("{} ", prompt_style.paint("道:>")));
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())
                    .map_err(|e| ChaChaError::RustyLine { source: e })?;
                if let Some(stmt) = parse_line(&line) {
                    debug!("stmt from readline", stmt);

                    // 🚧 Need to send an Arc to the inter_statement function instead
                    // of locking the whole time.
                    let stmt = {
                        let (stmt, _ty) = inter_statement(
                            &Arc::new(RwLock::new(stmt)),
                            &block,
                            &mut lu_dog.write().unwrap(),
                            &models.read().unwrap(),
                            &sarzak.read().unwrap(),
                        );
                        stmt
                    };

                    // 🚧 This needs fixing too.
                    match eval_statement(stmt, &mut stack) {
                        Ok((value, ty)) => {
                            let value = format!("{}", value);
                            println!("{}", result_style.paint(value));

                            let ty = PrintableValueType(ty);
                            let ty = format!("{}", ty);
                            println!("\t  ──➤  {}", type_style.paint(ty));
                        }
                        Err(e) => {
                            println!("{}", e);
                        }
                    }
                } else {
                    println!("{}", error_style.paint("\tWTF?"));
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

    Ok(())
}

pub(crate) struct PrintableValueType(pub Arc<RwLock<ValueType>>);

impl fmt::Display for PrintableValueType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.0.read().unwrap();
        let lu_dog = &LU_DOG;
        let sarzak = &SARZAK;
        let model = &MODELS;

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
                write!(f, "[{}]", PrintableValueType(ty))
            }
            ValueType::Reference(ref reference) => {
                let reference = lu_dog.read().unwrap().exhume_reference(reference).unwrap();
                let reference = reference.read().unwrap();
                let ty = reference.r35_value_type(&lu_dog.read().unwrap())[0].clone();
                write!(f, "&{}", PrintableValueType(ty))
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
                        let value = some.r23_value(&lu_dog.read().unwrap())[0]
                            .read()
                            .unwrap()
                            .clone();
                        let ty = value.r24_value_type(&lu_dog.read().unwrap())[0].clone();
                        write!(f, "Some({})", PrintableValueType(ty))
                    }
                }
            }
            ValueType::WoogStruct(ref woog_struct) => {
                debug!("woog_struct", woog_struct);
                let woog_struct = lu_dog
                    .read()
                    .unwrap()
                    .exhume_woog_struct(woog_struct)
                    .unwrap();
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

#[derive(Debug)]
pub struct Stack {
    pub(crate) meta: HashMap<String, HashMap<String, Value>>,
    pub(crate) global: HashMap<String, Value>,
    pub(crate) frames: Vec<HashMap<String, Value>>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            meta: HashMap::default(),
            global: HashMap::default(),
            frames: vec![HashMap::default()],
        }
    }

    fn push(&mut self) {
        self.frames.push(HashMap::default());
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    pub(crate) fn insert_meta_table(&mut self, table: String) {
        self.meta.insert(table, HashMap::default());
    }

    pub(crate) fn insert_meta(&mut self, table: &str, name: String, value: Value) {
        let table = self.meta.get_mut(table).unwrap();
        table.insert(name, value);
    }

    fn get_meta(&self, table: &str, name: &str) -> Option<&Value> {
        if let Some(table) = self.meta.get(table) {
            table.get(name)
        } else {
            None
        }
    }

    pub(crate) fn insert_global(&mut self, name: String, value: Value) {
        if name.contains("::") {
            let mut split = name.split("::");
            let table = split.next().unwrap();
            let name = split
                .next()
                .expect("name contained `::`, but no second element");

            if let Some(Value::Table(ref mut table)) = self.global.get_mut(table) {
                table.insert(name.to_owned(), value);
            } else {
                self.global
                    .insert(table.to_owned(), Value::Table(HashMap::default()));
                if let Some(Value::Table(ref mut table)) = self.global.get_mut(table) {
                    table.insert(name.to_owned(), value);
                } else {
                    unreachable!()
                }
            }
        } else {
            self.global.insert(name, value);
        }
    }

    pub(crate) fn insert(&mut self, name: String, value: Value) {
        let frame = self.frames.last_mut().unwrap();
        frame.insert(name, value);
    }

    fn get(&self, name: &str) -> Option<&Value> {
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(Value::Table(ref table)) = self.get_simple(table) {
                table.get(name)
            } else {
                None
            }
        } else {
            self.get_simple(name)
        }
    }

    fn get_simple(&self, name: &str) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        self.global.get(name)
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut Value> {
        if name.contains("::") {
            let mut split = name.split("::");

            let name = split.next().unwrap();
            let table = split.next().unwrap();

            if let Some(Value::Table(ref mut table)) = self.get_simple_mut(table) {
                table.get_mut(name)
            } else {
                None
            }
        } else {
            self.get_simple_mut(name)
        }
    }

    fn get_simple_mut(&mut self, name: &str) -> Option<&mut Value> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(value) = frame.get_mut(name) {
                return Some(value);
            }
        }
        self.global.get_mut(name)
    }
}
