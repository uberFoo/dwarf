use std::{collections::VecDeque, path::Path, thread, time::Duration, time::Instant};

use abi_stable::{
    library::{lib_header_from_path, LibrarySuffix, RawLibrary},
    std_types::{RErr, ROk},
};
use ansi_term::Colour;

#[cfg(feature = "async")]
use async_io::Timer;
#[cfg(feature = "async")]
use smol::future;
#[cfg(feature = "async")]
use tracing::{debug_span, Instrument};

use snafu::{location, prelude::*, Location};
use uuid::Uuid;

#[cfg(feature = "async")]
use crate::chacha::asink::AsyncTask as ExecutorTask;

#[cfg(feature = "async")]
use super::Executor;

use crate::{
    chacha::{
        error::{NoSuchStaticMethodSnafu, Result, TypeMismatchSnafu},
        vm::{CallFrame, VM},
    },
    dwarf::extruder::update_span_value,
    interpreter::{
        debug, error, eval_expression, eval_function_call, eval_lambda_expression, function,
        ChaChaError, Context, PrintableValueType,
    },
    keywords::{
        ADD, ARGS, ASLEEP, ASSERT, ASSERT_EQ, CHACHA, COMPLEX_EX, EPS, EVAL, FN_NEW, FORMAT,
        HTTP_GET, INTERVAL, JOIN, LEN, MAP, NEW, NORM_SQUARED, ONE_SHOT, PARSE, PLUGIN, SLEEP,
        SPAWN, SPAWN_NAMED, SQUARE, SUM, TIME, TIMER, TYPEOF, UUID_TYPE,
    },
    lu_dog::{
        Argument, Block, Call, CallEnum, Expression, IntegerLiteral, Literal, MethodCall,
        ValueType, ValueTypeEnum, XValue,
    },
    new_ref,
    plug_in::PluginModRef,
    plug_in::PluginType,
    s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Value, ValueResult,
};

mod chacha;

pub fn eval(
    call_id: &SarzakStorePtr,
    expression: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> ValueResult {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let call = s_read!(lu_dog).exhume_call(call_id).unwrap();
    let first_arg = s_read!(call).argument;
    debug!("call {call:?}");
    let mut args = s_read!(call).r28_argument(&s_read!(lu_dog));
    debug!("args {args:?}");

    let arg_check = s_read!(call).arg_check;
    if arg_check {
        // 🚧 Shouldn't I be checking args here?
        s_write!(call).arg_check = false;
    }

    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
    let span = s_read!(value).r63_span(&s_read!(lu_dog))[0].clone();

    // This optional expression is the LHS of the call.
    let value = if let Some(ref expr) = s_read!(call).expression {
        let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
        // Evaluate the LHS to get at the underlying value/instance.
        let value = eval_expression(expr, context, vm)?;
        debug!("ExpressionEnum::Call LHS value {:?}", s_read!(value));

        // 🚧 I don't remember why this is a closure.
        let mut eval_lhs = || -> Result<RefType<Value>> {
            // Below we are reading the value of the LHS, and then using that
            // to determine what to do with the RHS.
            let read_value = s_read!(value);
            match &*read_value {
                Value::Function(ref func) => {
                    let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                    debug!("ExpressionEnum::Call func: {func:?}");
                    let value =
                        eval_function_call(func, &args, first_arg, arg_check, &span, context, vm)?;
                    debug!("value {value:?}");
                    Ok(value)
                }
                Value::Lambda(ref ƛ) => {
                    let ƛ = s_read!(lu_dog).exhume_lambda(&s_read!(ƛ).id).unwrap();
                    debug!("ExpressionEnum::Call ƛ: {ƛ:?}");
                    let value = eval_lambda_expression(ƛ, &args, arg_check, &span, context, vm)?;
                    debug!("value {value:?}");
                    Ok(value)
                }
                Value::ProxyType {
                    module: _,
                    obj_ty: _,
                    id: _,
                    plugin: _,
                } => Ok(value.clone()),
                Value::Range(_) => Ok(value.clone()),
                Value::Struct(_) => Ok(value.clone()),
                Value::Store(_store, _plugin) => Ok(value.clone()),
                Value::Task {
                    executor_id: _,
                    parent: _,
                } => Ok(value.clone()),
                Value::Vector(_) => Ok(value.clone()),
                misc_value => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    debug!("value {value:?}");

                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    dbg!(&misc_value);

                    Err(ChaChaError::NotAFunction {
                        value: misc_value.to_owned(),
                        span,
                        location: location!(),
                    })
                }
            }
        };

        let ty = s_read!(value).get_type(&s_read!(sarzak), &mut s_write!(lu_dog));

        // First we need to check the type of the LHS to see if there are
        // any instance methods on the type. This seems weird. I'm not sure
        // where to put it in my brain just yet.
        // But basically it comes down to allowing things like
        // `"dwarf".len()`
        // Or iterator things like
        // `[1, 2, 3].iter().map(|x| x + 1)`
        // `["hello", "I", "am", "dwarf!"].sort();
        let x = match &s_read!(ty).subtype {
            ValueTypeEnum::Ty(ref id) => {
                let _ty = s_read!(sarzak).exhume_ty(id).unwrap();
                // SString is here because we have methods on that type
                // 🚧 We need to add Vector or whatever as well.
                let x = match &*_ty.read().unwrap() {
                    Ty::SString(_) => value,
                    _ => eval_lhs()?,
                };
                x
            }
            _ => eval_lhs()?,
        };
        x
    } else {
        new_ref!(Value, Value::Empty)
    };

    // So we need to figure out the type that this is being called upon.
    let subtype = &s_read!(call).subtype;
    let call_result = match (subtype, value) {
        (CallEnum::MacroCall(_), _) => unimplemented!(),
        //
        // FunctionCall
        //
        // We already handled this above.
        (CallEnum::FunctionCall(_), value) => Ok(value),
        //
        // MethodCall
        //
        (CallEnum::MethodCall(ref meth), ref value) => {
            let meth = s_read!(lu_dog).exhume_method_call(meth).unwrap();
            let meth_name = &s_read!(meth).name;
            debug!("MethodCall method {meth:?}");
            debug!("MethodCall value {value:?}");
            // let mut value = s_write!(value);

            // let x = match &mut *value {
            let read_value = s_read!(value);
            match &*read_value {
                Value::ProxyType {
                    module: _,
                    obj_ty: ref id,
                    id: _,
                    plugin: _proxy,
                } => {
                    // Q: How do I invoke a function on an instance without
                    // actually grabbing the instance from memory?
                    // A: It's eval'd above, and in the `value` variable, which
                    // is deconstructed into this ProxyType. So that is bad.
                    // 🚧 We need to store a pointer to an in-memory value of
                    // this struct
                    let vt = s_read!(lu_dog);
                    let mut vt = vt.iter_value_type();
                    let woog_struct = loop {
                        if let Some(vt) = vt.next() {
                            if let ValueTypeEnum::WoogStruct(woog) = s_read!(vt).subtype {
                                let woog = s_read!(lu_dog).exhume_woog_struct(&woog).unwrap();
                                let object = s_read!(woog).object;
                                if let Some(ref obj_id) = object {
                                    if id == obj_id {
                                        break woog;
                                    }
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    };
                    let woog_struct = s_read!(woog_struct);
                    let impl_ = &woog_struct.r8c_implementation_block(&s_read!(lu_dog))[0];
                    let x = if let Some(func) = s_read!(impl_)
                        .r9_function(&s_read!(lu_dog))
                        .iter()
                        .find(|f| s_read!(f).name == *meth_name)
                    {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        eval_function_call(
                            (*func).clone(),
                            &args,
                            first_arg,
                            arg_check,
                            span,
                            context,
                            vm,
                        )
                    } else {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: meth_name.to_owned(),
                            span,
                            location: location!(),
                        });
                    };
                    x
                }
                Value::Range(range) => match meth_name.as_str() {
                    MAP => {
                        debug!("evaluating Range::map");
                        let func = args.pop().unwrap();
                        let func = s_read!(func).r37_expression(&s_read!(lu_dog))[0].clone();
                        let ƛ = eval_expression(func.clone(), context, vm).unwrap();
                        let ƛ = s_read!(ƛ);
                        let ƛ = if let Value::Lambda(ƛ) = &*ƛ {
                            ƛ
                        } else {
                            unreachable!()
                        };

                        let call = &MethodCall::new("map".to_owned(), &mut s_write!(lu_dog));
                        let call =
                            Call::new_method_call(false, None, None, &call, &mut s_write!(lu_dog));

                        let result = (range.start..range.end)
                            .map(|i| {
                                let block = Block::new(
                                    false,
                                    Uuid::new_v4(),
                                    None,
                                    None,
                                    &mut s_write!(lu_dog),
                                );
                                let literal = IntegerLiteral::new(i, &mut s_write!(lu_dog));
                                let literal =
                                    Literal::new_integer_literal(&literal, &mut s_write!(lu_dog));
                                let expression =
                                    Expression::new_literal(&literal, &mut s_write!(lu_dog));
                                let ty = ValueType::new_ty(
                                    &Ty::new_integer(&s_read!(sarzak)),
                                    &mut s_write!(lu_dog),
                                );
                                let value = XValue::new_expression(
                                    &block,
                                    &ty,
                                    &expression,
                                    &mut s_write!(lu_dog),
                                );
                                update_span_value(&span, &value, location!());
                                let argument = Argument::new(
                                    0,
                                    &expression,
                                    &call,
                                    None,
                                    &mut s_write!(lu_dog),
                                );
                                eval_lambda_expression(
                                    ƛ.clone(),
                                    &[argument],
                                    false,
                                    &span,
                                    context,
                                    vm,
                                )
                            })
                            .collect::<Result<Vec<RefType<Value>>>>()?;

                        Ok(new_ref!(Value, Value::Vector(result)))
                    }
                    _ => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: meth_name.to_owned(),
                            span,
                            location: location!(),
                        });
                    }
                },
                Value::String(string) => match meth_name.as_str() {
                    LEN => {
                        debug!("evaluating String::len");
                        let len = unicode_segmentation::UnicodeSegmentation::graphemes(
                            string.as_str(),
                            true,
                        )
                        .collect::<Vec<&str>>()
                        .len();
                        Ok(new_ref!(Value, Value::Integer(len as i64)))
                    }
                    FORMAT => {
                        debug!("evaluating String::format");
                        // let mut arg_map = HashMap::default();
                        let arg_values = if !args.is_empty() {
                            // The VecDeque is so that I can pop off the args, and then push them
                            // back onto a queue in the same order. What? That doesn't make sense.
                            // 🚧 I feel like I'm doing something stupid here -- take a look please!
                            let mut arg_values = VecDeque::with_capacity(args.len());

                            // Gotta do this goofy thing because we don't have a first pointer,
                            // and they aren't in order.
                            let mut next = args
                                .iter()
                                .inspect(|a| {
                                    debug!("arg: {a:?}");
                                })
                                .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                                .unwrap()
                                .clone();

                            // 🚧 ugly hack until I track down why the first argument is the string.
                            let next_id = s_read!(next).next.unwrap();
                            next = s_read!(lu_dog).exhume_argument(&next_id).unwrap();

                            // It's not clear what's happening below really. Here's the scoop:
                            // We iterate over the arguments to the `format` call. For each one
                            // we evaluate it and store it in a map. And also push it onto vac.
                            loop {
                                let expr = s_read!(lu_dog)
                                    .exhume_expression(&s_read!(next).expression)
                                    .unwrap();

                                // let source =
                                //     s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
                                // let source = s_read!(source);
                                // let source = &source.source;

                                // let value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];

                                // let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                                // let read = s_read!(span);
                                // let span = read.start as usize..read.end as usize;

                                // let key = source[span].to_owned();

                                let value = eval_expression(expr, context, vm)?;
                                debug!("value {value:?}");

                                // This is where the magic happens and we turn the value
                                // into a string.
                                arg_values.push_back(s_read!(value).to_string());

                                // debug!(
                                // "insert into arg_map `{}`: `{}`",
                                // key,
                                // s_read!(value).to_string()
                                // );
                                // arg_map.insert(key, s_read!(value).to_string());

                                let next_id = s_read!(next).next;
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
                                                location: location!(),
                                            });
                                        }
                                    } else {
                                        current.push(c);
                                    }
                                }
                            }
                        }

                        Ok(new_ref!(Value, Value::String(result)))
                    }
                    value_ => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        debug!("value {value:?}");

                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: value_.to_owned(),
                            span,
                            location: location!(),
                        });
                    }
                },
                Value::Struct(ut) => {
                    // Below is all wrapped up to avoid a double borrow.
                    let woog_struct = {
                        let ut_read = s_read!(ut);
                        let ty = ut_read.get_type();
                        let ty = s_read!(ty);
                        if let ValueTypeEnum::WoogStruct(woog_struct) = &ty.subtype {
                            *woog_struct
                        } else {
                            unreachable!();
                        }
                    };

                    let woog_struct = s_read!(lu_dog).exhume_woog_struct(&woog_struct).unwrap();
                    let woog_struct = s_read!(woog_struct);
                    let impl_ = &woog_struct.r8c_implementation_block(&s_read!(lu_dog))[0];
                    let x = if let Some(func) = s_read!(impl_)
                        .r9_function(&s_read!(lu_dog))
                        .iter()
                        .find(|f| s_read!(f).name == *meth_name)
                    {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        eval_function_call(
                            (*func).clone(),
                            &args,
                            first_arg,
                            arg_check,
                            span,
                            context,
                            vm,
                        )
                    } else {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: meth_name.to_owned(),
                            span,
                            location: location!(),
                        });
                    };
                    x
                }
                Value::Vector(v) => match meth_name.as_str() {
                    SUM => {
                        let mut sum = 0;
                        for value in v {
                            let mut value = s_write!(value);
                            match &mut *value {
                                Value::Integer(i) => sum += *i,
                                Value::Task {
                                    executor_id,
                                    parent,
                                } => {
                                    let e = executor_id.take().unwrap();
                                    let t = parent.take().unwrap();

                                    let value = future::block_on(t)?;

                                    let v = s_read!(value);
                                    match &*v {
                                        Value::Integer(i) => sum += i,
                                        v => {
                                            dbg!(v);
                                            unreachable!()
                                        }
                                    }
                                }
                                v => {
                                    dbg!(v);
                                    unreachable!()
                                }
                            }
                        }
                        Ok(new_ref!(Value, Value::Integer(sum)))
                    }
                    _ => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: meth_name.to_owned(),
                            span,
                            location: location!(),
                        });
                    }
                },
                bar => panic!("need to deal with Value {:?}", bar),
            }
        }
        //
        // StaticMethodCall
        //
        (CallEnum::StaticMethodCall(ref meth), _) => {
            let meth = s_read!(lu_dog).exhume_static_method_call(meth).unwrap();
            let call = s_read!(meth).r30_call(&s_read!(lu_dog))[0].clone();

            let arg_check = s_read!(call).arg_check;
            if arg_check {
                s_write!(call).arg_check = false;
            }

            let mut arg_values = {
                let args = s_read!(call).r28_argument(&s_read!(lu_dog));
                if !args.is_empty() {
                    let mut arg_values = VecDeque::with_capacity(args.len());
                    // Find the first one.
                    let mut next = args
                        .iter()
                        .find(|a| s_read!(a).r27c_argument(&s_read!(lu_dog)).is_empty())
                        .unwrap()
                        .clone();

                    loop {
                        let expr = s_read!(lu_dog)
                            .exhume_expression(&s_read!(next).expression)
                            .unwrap();
                        let x_value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(x_value).r63_span(&s_read!(lu_dog))[0];
                        let span = s_read!(span).start as usize..s_read!(span).end as usize;
                        let value = eval_expression(expr, context, vm)?;
                        arg_values.push_back((value, span));

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
                }
            };

            let ty = &s_read!(meth).ty;
            let func = &s_read!(meth).func;
            debug!("StaticMethodCall ty {ty:?}");
            debug!("StaticMethodCall func {func:?}");

            // This is dirty. Down and dirty...
            if ty == UUID_TYPE && func == FN_NEW {
                let value = Value::Uuid(Uuid::new_v4());

                Ok(new_ref!(Value, value))
            } else if ty == COMPLEX_EX {
                match func.as_str() {
                    NORM_SQUARED => {
                        let value = arg_values.pop_front().unwrap().0;
                        let thonk = context.memory().get_thonk(0).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "norm_squared".into()));
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(2);

                        Ok(result.unwrap())
                    }
                    SQUARE => {
                        let value = arg_values.pop_front().unwrap().0;
                        let thonk = context.memory().get_thonk(2).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "square".into()));
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(5);

                        Ok(result.unwrap())
                    }
                    ADD => {
                        let thonk = context.memory().get_thonk(1).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "add".into()));
                        let value = arg_values.pop_front().unwrap().0;
                        vm.push_stack(value);
                        let value = arg_values.pop_front().unwrap().0;
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(2);

                        Ok(result.unwrap())
                    }
                    method => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        Err(ChaChaError::NoSuchStaticMethod {
                            ty: ty.to_owned(),
                            method: method.to_owned(),
                            span,
                            location: location!(),
                        })
                    }
                }
            } else if ty == CHACHA {
                match func.as_str() {
                    ARGS => {
                        debug!("evaluating chacha::args");

                        if let Some(args) = &context.get_args() {
                            Ok(args.clone())
                        } else {
                            Ok(new_ref!(Value, Value::Vector(Vec::new())))
                        }
                    }
                    ASLEEP => {
                        let (duration, _) = arg_values.pop_front().unwrap();
                        let millis = &*s_read!(duration);
                        let millis: u64 = millis.try_into()?;
                        let duration = Duration::from_millis(millis);

                        let exec = context.executor_index();

                        let span = debug_span!("asleep", duration = ?duration, target = "async");
                        let future = async move {
                            tracing::debug!("sleeping for {duration:?}");
                            let _instant = Timer::after(duration).await;
                            tracing::debug!("done sleeping");
                            Ok(new_ref!(Value, Value::Empty))
                        }
                        .instrument(span);

                        let task = ExecutorTask::new(
                            format!("sleep {millis}ms"),
                            Executor::root_worker(),
                            future,
                        );
                        Executor::start_task(&task);

                        // let task =
                        // ExecutorTask::new(Executor::at_index(context.executor_index()), future);

                        Ok(new_ref!(
                            Value,
                            Value::Future("sleep".to_owned(), Some(task))
                        ))
                    }
                    ASSERT => chacha::assert(arg_values, expression, lu_dog),
                    ASSERT_EQ => chacha::assert_eq(arg_values, expression, lu_dog),
                    EPS => {
                        debug!("evaluating chacha::eps");
                        let mut timings = context.get_timings().to_vec();
                        timings.sort_by(|a, b| a.partial_cmp(b).unwrap());

                        let mean = timings.iter().sum::<f64>() / timings.len() as f64;
                        let std_dev = timings.iter().map(|x| (x - mean).powi(2)).sum::<f64>()
                            / timings.len() as f64;
                        let median = timings[timings.len() / 2];

                        let result = format!(
                                    "expressions (mean/std_dev/median) ((10k)/sec): {:.1} / {:.1} / {:.1}\n",
                                    mean,
                                    std_dev,
                                    median
                                );
                        // chacha_print(result, context)?;

                        Ok(new_ref!(Value, Value::String(result)))
                    }
                    EVAL => chacha::eval_dwarf(arg_values, expression, context),
                    HTTP_GET => chacha::http_get(arg_values, expression, context),
                    PARSE => chacha::parse_dwarf(arg_values, expression, context),
                    SLEEP => {
                        let (duration, _) = arg_values.pop_front().unwrap();
                        let millis = &*s_read!(duration);
                        let millis: u64 = millis.try_into()?;
                        let duration = Duration::from_millis(millis);

                        std::thread::sleep(duration);
                        Ok(new_ref!(Value, Value::Empty))
                    }
                    #[cfg(feature = "async")]
                    SPAWN => spawn("task".to_owned(), &mut arg_values, expression, context),
                    #[cfg(feature = "async")]
                    SPAWN_NAMED => {
                        let (name, _) = arg_values.pop_front().unwrap();
                        let name: String = (&*s_read!(name)).try_into()?;
                        spawn(name, &mut arg_values, expression, context)
                    }
                    TIME => {
                        debug!("evaluating chacha::time");
                        // 🚧 I should be checking that there is an argument before
                        // I go unwrapping it.
                        let (func, span) = arg_values.pop_front().unwrap();
                        let func = s_read!(func);
                        ensure!(
                            matches!(&*func, Value::Lambda(_))
                                || matches!(&*func, Value::Function(_)),
                            {
                                // 🚧 I'm not really sure what to do about this here. It's
                                // all really a hack for now anyway.
                                let ty = func.get_type(&s_read!(sarzak), &mut s_write!(lu_dog));
                                let ty = PrintableValueType(true, ty, context.models());
                                let ty = ty.to_string();
                                TypeMismatchSnafu {
                                    expected: "<function>".to_string(),
                                    found: ty,
                                    span,
                                }
                            }
                        );

                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        let now = Instant::now();
                        if let Value::Function(func) = &*func {
                            let _result = eval_function_call(
                                func.clone(),
                                &[],
                                None,
                                true,
                                span,
                                context,
                                vm,
                            )?;
                        } else if let Value::Lambda(ƛ) = &*func {
                            let _result =
                                eval_lambda_expression(ƛ.clone(), &[], true, span, context, vm)?;
                        } else {
                            panic!("missing implementation for timing this type: {func:?}");
                        };
                        let elapsed = now.elapsed();

                        Ok(new_ref!(Value, Value::Float(elapsed.as_secs_f64())))
                    }
                    // This returns a string because that's the easy button given what
                    // I have to work with. Once I get enums into the language, I'll
                    // be able to return a proper enum.
                    TYPEOF => {
                        debug!("evaluating chacha::typeof");
                        let arg = arg_values.pop_front().unwrap().0;
                        let ty = s_read!(arg).get_type(&s_read!(sarzak), &mut s_write!(lu_dog));
                        let pvt_ty = PrintableValueType(false, ty, context.models());

                        Ok(new_ref!(Value, pvt_ty.to_string().into()))
                    }
                    method => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        Err(ChaChaError::NoSuchStaticMethod {
                            ty: ty.to_owned(),
                            method: method.to_owned(),
                            span,
                            location: location!(),
                        })
                    }
                }
            // #[cfg(feature = "async")]
            } else if ty == TIMER {
                match func.as_str() {
                    ONE_SHOT => {
                        // dbg!("huh");
                        // 🚧 I should be checking that there is an argument before
                        // I go unwrapping it.
                        let (duration, _) = arg_values.pop_front().unwrap();
                        let millis = &*s_read!(duration);
                        let millis: u64 = millis.try_into()?;
                        let duration = Duration::from_millis(millis);

                        let (func, span) = arg_values.pop_front().unwrap();
                        // let read_func = s_read!(func);
                        ensure!(
                            matches!(&*s_read!(func), Value::Lambda(_))
                                || matches!(&*s_read!(func), Value::Function(_)),
                            {
                                // 🚧 I'm not really sure what to do about this here. It's
                                // all really a hack for now anyway.
                                let ty =
                                    s_read!(func).get_type(&s_read!(sarzak), &mut s_write!(lu_dog));
                                let ty = PrintableValueType(true, ty, context.models());
                                let ty = ty.to_string();
                                TypeMismatchSnafu {
                                    expected: "<function>".to_string(),
                                    found: ty,
                                    span,
                                }
                            }
                        );

                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = s_read!(value).r63_span(&s_read!(lu_dog))[0].clone();
                        let func = s_read!(func).clone();

                        let mut fubar = context.clone();
                        // let mut baz = fubar.executor().clone();
                        let future = async move {
                            let mem = fubar.memory().clone();
                            let mut vm = VM::new(&mem);

                            // let func = func.clone();

                            debug!("sleeping for {duration:?}");
                            // dbg!(&duration, &fubar.executor());
                            let _instant = Timer::after(duration).await;
                            // dbg!(&duration, _instant, &fubar.executor());
                            debug!("done sleeping");

                            if let Value::Function(func) = &func {
                                eval_function_call(
                                    func.clone(),
                                    &[],
                                    None,
                                    true,
                                    &span,
                                    &mut fubar,
                                    &mut vm,
                                )
                            } else if let Value::Lambda(ƛ) = &func {
                                eval_lambda_expression(
                                    ƛ.clone(),
                                    &[],
                                    true,
                                    &span,
                                    &mut fubar,
                                    &mut vm,
                                )
                            } else {
                                panic!("missing implementation for timing this type: {func:?}");
                            }
                        };

                        // let executor = context.executor();
                        let task = ExecutorTask::new(
                            "one_shot".to_owned(),
                            Executor::at_index(context.executor_index()),
                            future,
                        );
                        // let task = ExecutorTask::new(Executor::global(), future);

                        // let task = baz.spawn(future);

                        let value = new_ref!(Value, Value::Future("sleep".to_owned(), Some(task)));
                        // Stash the future away so that it doesn't get dropped when it's done running.
                        // context.executor().park_value(value.clone());

                        Ok(value)
                    }
                    missing_method => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        Err(ChaChaError::NoSuchStaticMethod {
                            ty: ty.to_owned(),
                            method: missing_method.to_owned(),
                            span,
                            location: location!(),
                        })
                    }
                }
            } else if Some(PLUGIN) == ty.split("::").next() {
                let plugin = ty.split("::").nth(1).unwrap();
                match func.as_str() {
                    NEW => {
                        let library_path = RawLibrary::path_in_directory(
                            Path::new(&format!(
                                "{}/extensions/{plugin}/lib",
                                context.get_home().display()
                            )),
                            plugin,
                            LibrarySuffix::NoSuffix,
                        );
                        let root_module = (|| {
                            let header = lib_header_from_path(&library_path)?;
                            header.init_root_module::<PluginModRef>()
                        })()
                        .map_err(|e| {
                            eprintln!("{e}");
                            ChaChaError::BadnessHappened {
                                message: "Plug-in error".to_owned(),
                                location: location!(),
                            }
                        })?;

                        let ctor = root_module.new();
                        // let (_, path) = arg_values.pop().unwrap();
                        // let path = s_read!(path).clone();
                        // let plugin = new_ref!(PluginType, ctor(vec![path.into()].into()).unwrap());
                        let plugin = new_ref!(PluginType, ctor(vec![].into()).unwrap());
                        // model.1.replace(plugin.clone());

                        // let value = new_ref!(Value, Value::Store(store, plugin));
                        let value = new_ref!(Value, Value::Plugin(plugin));

                        Ok(value)
                    }
                    missing_method => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        Err(ChaChaError::NoSuchStaticMethod {
                            ty: ty.to_owned(),
                            method: missing_method.to_owned(),
                            span,
                            location: location!(),
                        })
                    }
                }
            } else if let Some(value) = context.memory().get_meta(ty, func) {
                debug!("StaticMethodCall meta value {value:?}");
                match &*s_read!(value) {
                    Value::Function(func) => {
                        debug!("StaticMethodCall meta func {func:?}");
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        debug!("StaticMethodCall::Function {value:?}");
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let value = eval_function_call(
                            func.clone(),
                            &args,
                            first_arg,
                            arg_check,
                            span,
                            context,
                            vm,
                        )?;
                        debug!("StaticMethodCall meta value {value:?}");
                        Ok(value)
                    }
                    value => {
                        error!("deal with call expression {value:?}");
                        Ok(new_ref!(Value, Value::Empty))
                    }
                }
            } else if let Some(value) = context.memory().get(ty) {
                debug!("StaticMethodCall frame value {value:?}");
                match &mut *s_write!(value) {
                    Value::Function(ref func) => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                        debug!("StaticMethodCall frame func {func:?}");
                        let value = eval_function_call(
                            func, &args, first_arg, arg_check, span, context, vm,
                        )?;
                        debug!("StaticMethodCall frame value {value:?}");
                        Ok(value)
                    }
                    Value::ProxyType {
                        module: _,
                        obj_ty: _,
                        id: _,
                        plugin: _,
                    } => {
                        unimplemented!();
                        // debug!("StaticMethodCall proxy {ut:?}");
                        // s_write!(ut).call(
                        //     func,
                        //     &mut arg_values.iter().map(|v| v.0 .0.clone()).collect(),
                        // )
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
                        error!("deal with call expression {value}");
                        panic!("fix this");
                    }
                }
            } else {
                dbg!(&ty, &func);
                ensure!(false, {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    NoSuchStaticMethodSnafu {
                        ty: ty.to_owned(),
                        method: func.to_owned(),
                        span,
                    }
                });

                unreachable!();
            }
        }
    };

    call_result
}

fn spawn(
    name: String,
    arg_values: &mut VecDeque<(RefType<Value>, std::ops::Range<usize>)>,
    expression: &RefType<Expression>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    let sarzak = context.sarzak_heel().clone();
    let lu_dog = context.lu_dog_heel().clone();

    debug!("evaluating chacha::spawn");
    // 🚧 I should be checking that there is an argument before
    // I go unwrapping it.
    let (func, span) = arg_values.pop_front().unwrap();
    let func = s_read!(func);
    ensure!(
        matches!(&*func, Value::Lambda(_)) || matches!(&*func, Value::Function(_)),
        {
            // 🚧 I'm not really sure what to do about this here. It's
            // all really a hack for now anyway.
            let ty = func.get_type(&s_read!(sarzak), &mut s_write!(lu_dog));
            let ty = PrintableValueType(true, ty, context.models());
            let ty = ty.to_string();
            TypeMismatchSnafu {
                expected: "<function>".to_string(),
                found: ty,
                span,
            }
        }
    );

    let func = func.to_owned();
    let expression = expression.clone();
    let mut nested_context = context.clone();

    let executor_id = Executor::new_worker();
    nested_context.set_executor_index(executor_id);

    let t_span = debug_span!("spawn_span", target = "async", name = ?name);
    let future = async move {
        let mem = nested_context.memory().clone();
        let mut vm = VM::new(&mem);
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        if let Value::Function(func) = &func {
            eval_function_call(
                func.clone(),
                &[],
                None,
                true,
                span,
                &mut nested_context,
                &mut vm,
            )
        } else if let Value::Lambda(ƛ) = &func {
            eval_lambda_expression(ƛ.clone(), &[], true, span, &mut nested_context, &mut vm)
        } else {
            unreachable!()
        }
    }
    .instrument(t_span);

    // let task = fubar.executor().spawn(future);
    // context_copy.executor().park_value(new_ref!(Value, Value::Task(name, Some(task))));

    // let future =
    // async move { future::block_on(async { fubar.executor().resolve_task(task).await }) };
    // future::block_on(async { ctx.executor().run().await });

    // let task = nested_context_clone.executor().spawn(future);

    // dbg!(driver.executor_index());

    let child_task = ExecutorTask::new("spawn".to_owned(), Executor::at_index(executor_id), future);

    // This is *key*.
    // task.detach();
    // child_task.start();
    Executor::start_task(&child_task);

    // let child = new_ref!(Value, Value::Future(name.clone(), Some(task)));
    // let value = new_ref!(Value, Value::Task(ChaChaTask::new(name.clone(), task)));

    // Stash the future away so that it doesn't get dropped when it's done running.
    // nested_context_clone.executor().park_value(value.clone());

    let future = async move {
        let result = child_task.await;
        Executor::remove_worker(executor_id);
        result
    };

    // 🚧 This puts all spawned tasks on the main executor. I need to ponder whether
    // or not I want them to be nested.
    // let task = Executor::global().spawn(future);
    let task = ExecutorTask::new("spawn driver".to_owned(), Executor::root_worker(), future);
    // Executor::spawn(&task);

    let value = new_ref!(
        Value,
        Value::Task {
            executor_id: Some(executor_id),
            parent: Some(task),
            // child: None
        }
    );

    // context.executor().park_value(v.clone());

    Ok(value)
}
