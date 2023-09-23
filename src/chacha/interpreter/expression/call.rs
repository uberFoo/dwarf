use std::{collections::VecDeque, thread, time::Duration, time::Instant};

use ansi_term::Colour;
use snafu::{location, prelude::*, Location};
use uuid::Uuid;

use crate::{
    chacha::{
        error::{NoSuchStaticMethodSnafu, Result, TypeMismatchSnafu},
        vm::{CallFrame, VM},
    },
    interpreter::{
        debug, error, eval_expression, eval_function_call, eval_lambda_expression,
        func_call::eval_external_method, function, ChaChaError, Context, PrintableValueType,
    },
    lu_dog::{CallEnum, Expression, ValueTypeEnum},
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Value, ADD, ARGS, ASSERT, ASSERT_EQ, CHACHA, COMPLEX_EX, EPS,
    EVAL, FN_NEW, FORMAT, LEN, NORM_SQUARED, PARSE, SLEEP, SQUARE, TIME, TYPEOF, UUID_TYPE,
};

mod chacha;

pub fn eval(
    call_id: &SarzakStorePtr,
    expression: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let call = s_read!(lu_dog).exhume_call(call_id).unwrap();
    let first_arg = s_read!(call).argument;
    debug!("call {call:?}");
    let args = s_read!(call).r28_argument(&s_read!(lu_dog));
    debug!("args {args:?}");
    // fix_error!("arg_check", s_read!(call).arg_check);
    let arg_check = s_read!(call).arg_check;
    if arg_check {
        s_write!(call).arg_check = false;
    }

    // This optional expression is the LHS of the call.
    let value = if let Some(ref expr) = s_read!(call).expression {
        let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
        // Evaluate the LHS to get at the underlying value/instance.
        let value = eval_expression(expr, context, vm)?;
        debug!("ExpressionEnum::Call LHS value {:?}", s_read!(value));

        let mut eval_lhs = || -> Result<RefType<Value>> {
            // Below we are reading the value of the LHS, and then using that
            // to determine what to do with the RHS.
            let read_value = s_read!(value);
            match &*read_value {
                Value::Function(ref func) => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                    debug!("ExpressionEnum::Call func: {func:?}");
                    let value =
                        eval_function_call(func, &args, first_arg, arg_check, span, context, vm)?;
                    debug!("value {value:?}");
                    Ok(value)
                }
                Value::Future(_) => Ok(value.clone()),
                Value::Lambda(ref ƛ) => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let ƛ = s_read!(lu_dog).exhume_lambda(&s_read!(ƛ).id).unwrap();
                    debug!("ExpressionEnum::Call ƛ: {ƛ:?}");
                    let value = eval_lambda_expression(ƛ, &args, arg_check, span, context, vm)?;
                    debug!("value {value:?}");
                    Ok(value)
                }
                Value::ProxyType {
                    module: _,
                    obj_ty: _,
                    id: _,
                    plugin: _,
                } => Ok(value.clone()),
                Value::Struct(_) => Ok(value.clone()),
                Value::Store(_store, _plugin) => Ok(value.clone()),
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

        let ty = {
            let lu_dog = s_read!(lu_dog);
            s_read!(value).get_type(&s_read!(sarzak), &lu_dog)
        };

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
            match &*s_read!(value) {
                // Value::Future(ref mut join_handle) => {
                //     use futures::future::FutureExt;

                //     let lambda = args[0].clone();
                //     let lambda = s_read!(lambda).r37_expression(&s_read!(lu_dog))[0].clone();
                //     dbg!(&args);
                //     let mut context = context.to_owned();
                //     // let remote_handle = join_handle.remote_handle();

                //     let future = async move {
                //         let mem = context.memory().clone();
                //         let mut vm = VM::new(&mem);

                //         // remote_handle
                //         let value = eval_expression(lambda, &mut context, &mut vm).unwrap();
                //         dbg!(&value);
                //         // join_handle.await
                //         value
                //     };
                //     let value = async_std::task::spawn_local(future);
                //     // Ok(async_std::task::block_on(future))

                //     Ok(new_ref!(Value, Value::Future(value)))
                // }
                Value::Store(store, _plugin) => {
                    let impl_ = &s_read!(store).r83c_implementation_block(&s_read!(lu_dog))[0];
                    let x = if let Some(func) = s_read!(impl_)
                        .r9_function(&s_read!(lu_dog))
                        .iter()
                        .find(|f| s_read!(f).name == *meth_name)
                    {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        eval_external_method(
                            (*func).clone(),
                            &args,
                            s_read!(call).argument,
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
                    PARSE => chacha::parse_dwarf(arg_values, expression, context),
                    SLEEP => {
                        let (duration, _) = arg_values.pop_front().unwrap();
                        let millis = &*s_read!(duration);
                        let millis: u64 = millis.try_into()?;

                        thread::sleep(Duration::from_millis(millis));

                        Ok(new_ref!(Value, Value::Empty))
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
                                let ty = func.get_type(&s_read!(sarzak), &s_read!(lu_dog));
                                let ty = PrintableValueType(true, &ty, context);
                                TypeMismatchSnafu {
                                    expected: "<function>".to_string(),
                                    found: ty.to_string(),
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
                        let ty = s_read!(arg).get_type(&s_read!(sarzak), &s_read!(lu_dog));
                        let pvt_ty = PrintableValueType(false, &ty, context);

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
