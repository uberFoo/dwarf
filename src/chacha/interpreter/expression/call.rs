use std::{collections::VecDeque, time::Instant};

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::prelude::*;
use uuid::Uuid;

use crate::{
    chacha::{
        error::{NoSuchStaticMethodSnafu, Result, TypeMismatchSnafu},
        vm::{CallFrame, VM},
    },
    interpreter::{
        debug, error, eval_expression, eval_function_call, eval_lambda_expression, function,
        ChaChaError, Context, PrintableValueType,
    },
    lu_dog::{CallEnum, Expression, List, ValueType, ValueTypeEnum},
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_call(
    call_id: &SarzakStorePtr,
    expression: &RefType<Expression>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let call = s_read!(lu_dog).exhume_call(call_id).unwrap();
    debug!("call {call:?}");
    let args = s_read!(call).r28_argument(&s_read!(lu_dog));
    debug!("args {args:?}");
    // fix_error!("arg_check", s_read!(call).arg_check);

    let arg_check = s_read!(call).arg_check;
    if arg_check {
        s_write!(call).arg_check = false;
    }

    // This optional expression is the LHS of the call.
    let (value, ty) = if let Some(ref expr) = s_read!(call).expression {
        let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
        // Evaluate the LHS to get at the function.
        let (value, ty) = eval_expression(expr, context, vm)?;
        debug!("ExpressionEnum::Call LHS value {:?}", s_read!(value));
        debug!("ExpressionEnum::Call LHS ty {ty:?}");

        let mut tuple_from_value = || -> Result<(RefType<Value>, RefType<ValueType>)> {
            // Below we are reading the value of the LHS, and then using that
            // to determine what to do with the RHS.
            let read_value = s_read!(value);
            match &*read_value {
                Value::Function(ref func) => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                    debug!("ExpressionEnum::Call func: {func:?}");
                    let (value, ty) =
                        eval_function_call(func, &args, arg_check, span, context, vm)?;
                    debug!("value {value:?}");
                    debug!("ty {ty:?}");
                    Ok((value, ty))
                }
                Value::Lambda(ref ƛ) => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let ƛ = s_read!(lu_dog).exhume_lambda(&s_read!(ƛ).id).unwrap();
                    debug!("ExpressionEnum::Call ƛ: {ƛ:?}");
                    let (value, ty) =
                        eval_lambda_expression(ƛ, &args, arg_check, span, context, vm)?;
                    debug!("value {value:?}");
                    debug!("ty {ty:?}");
                    Ok((value, ty))
                }
                // 🚧 ProxyType
                // Value::ProxyType(pt) => {
                //     let ty = s_read!(lu_dog)
                //         .exhume_value_type(&s_read!(pt).struct_uuid())
                //         .unwrap();
                //     Ok((value.clone(), ty))
                // }
                Value::UserType(ut) => Ok((value.clone(), s_read!(ut).get_type().clone())),
                value_ => {
                    let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                    debug!("value {value:?}");

                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    Err(ChaChaError::NotAFunction {
                        value: value_.to_owned(),
                        span,
                    })
                }
            }
        };

        // First we need to check the type of the LHS to see if there are
        // any instance methods on the type. This seems weird. I'm not sure
        // where to put it in my brain just yet.
        // But basically it comes down to allowing things like
        // `"dwarf".len()`
        // Or iterators things like
        // `[1, 2, 3].iter().map(|x| x + 1)`
        // `["hello", "I", "am", "dwarf!"].sort();
        let x = match &s_read!(ty).subtype {
            ValueTypeEnum::Ty(ref id) => {
                let _ty = *s_read!(sarzak).exhume_ty(id).unwrap();
                match &*_ty.borrow() {
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
            Value::Empty.get_type(&s_read!(sarzak), &s_read!(lu_dog)),
        )
    };

    // So we need to figure out the type that this is being called upon.
    let subtype = &s_read!(call).subtype;
    let call_result = match (subtype, value, ty) {
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
            debug!("MethodCall method {meth:?}");
            debug!("MethodCall value {value:?}");
            debug!("MethodCall type {ty:?}");

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
                        debug!("evaluating String::len");
                        let len = unicode_segmentation::UnicodeSegmentation::graphemes(
                            string.as_str(),
                            true,
                        )
                        .collect::<Vec<&str>>()
                        .len();
                        let ty = Ty::new_integer(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));
                        Ok((new_ref!(Value, Value::Integer(len as i64)), ty))
                    }
                    "format" => {
                        debug!("evaluating String::format");
                        let mut arg_map = HashMap::default();
                        let mut arg_values = if !args.is_empty() {
                            // The VecDeque is so that I can pop off the args, and then push them
                            // back onto a queue in the same order.
                            // Gotta do this goofy thing because we don't have a first pointer,
                            // and they aren't in order.
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

                                let source =
                                    s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
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

                        // 🚧 Oddly, the lhs is the first argument. Not something that I want
                        // or even need to understand atm.
                        arg_values.pop_front();

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

                        let ty = Ty::new_s_string(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));
                        Ok((new_ref!(Value, Value::String(result)), ty))
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
                        });
                    }
                },
                Value::UserType(ut) => {
                    // Below is all wrapped up to avoid a double borrow.
                    let woog_struct = {
                        let ut_read = s_read!(ut);
                        let ty = ut_read.get_type();
                        let ty = s_read!(ty);
                        if let ValueTypeEnum::WoogStruct(woog_struct) = &ty.subtype {
                            *woog_struct
                        } else {
                            // 🚧 This should be an error.
                            panic!("I'm trying to invoke a function on a UserType, and it's not a Struct!");
                        }
                    };

                    let woog_struct = s_read!(lu_dog).exhume_woog_struct(&woog_struct).unwrap();
                    let woog_struct = s_read!(woog_struct);
                    let impl_ = &woog_struct.r8c_implementation_block(&s_read!(lu_dog))[0];
                    let x = if let Some(func) = s_read!(impl_)
                        .r9_function(&s_read!(lu_dog))
                        .iter()
                        .find(|f| s_read!(f).name == *meth)
                    {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        eval_function_call((*func).clone(), &args, arg_check, span, context, vm)
                    } else {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        return Err(ChaChaError::NoSuchMethod {
                            method: meth.to_owned(),
                            span,
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
        (CallEnum::StaticMethodCall(meth), _, _) => {
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
            if ty == "Uuid" && func == "new" {
                let value = Value::Uuid(Uuid::new_v4());
                let ty = Ty::new_s_uuid(&s_read!(sarzak));
                let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

                Ok((new_ref!(Value, value), ty))
            } else if ty == "ComplexEx" {
                match func.as_str() {
                    "norm_squared" => {
                        let (value, ty) = arg_values.pop_front().unwrap().0;
                        let thonk = context.memory().get_thonk(0).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "norm_squared".into()));
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(2);

                        Ok((result.unwrap(), ty))
                    }
                    "square" => {
                        let (value, ty) = arg_values.pop_front().unwrap().0;
                        let thonk = context.memory().get_thonk(2).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "square".into()));
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(5);

                        Ok((result.unwrap(), ty))
                    }
                    "add" => {
                        let thonk = context.memory().get_thonk(1).unwrap();
                        let mut frame = CallFrame::new(0, 0, thonk);
                        vm.push_stack(new_ref!(Value, "add".into()));
                        let (value, _ty) = arg_values.pop_front().unwrap().0;
                        vm.push_stack(value);
                        let (value, ty) = arg_values.pop_front().unwrap().0;
                        vm.push_stack(value);
                        let result = vm.run(&mut frame, false);
                        vm.pop_stack();
                        vm.pop_stack();
                        vm.pop_stack();
                        context.increment_expression_count(2);

                        Ok((result.unwrap(), ty))
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
                        })
                    }
                }
            } else if ty == "chacha" {
                match func.as_str() {
                    "args" => {
                        debug!("evaluating chacha::args");
                        let ty = Ty::new_s_string(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));
                        let ty = List::new(&ty, &mut s_write!(lu_dog));
                        let ty = ValueType::new_list(&ty, &mut s_write!(lu_dog));

                        if let Some(args) = &context.get_args() {
                            Ok((args.clone(), ty))
                        } else {
                            Ok((new_ref!(Value, Value::Vector(Vec::new())), ty))
                        }
                    }
                    // This returns a string because that's the easy button given what
                    // I have to work with. Once I get enums into the language, I'll
                    // be able to return a proper enum.
                    "typeof" => {
                        debug!("evaluating chacha::typeof");
                        let (_arg, ty) = arg_values.pop_front().unwrap().0;
                        let pvt_ty = PrintableValueType(&ty, context);
                        let ty = Ty::new_s_string(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

                        Ok((new_ref!(Value, pvt_ty.to_string().into()), ty))
                    }
                    "time" => {
                        debug!("evaluating chacha::time");
                        // 🚧 I should be checking that there is an argument before
                        // I go unwrapping it.
                        let ((func, ty), span) = arg_values.pop_front().unwrap();
                        let func = s_read!(func);
                        ensure!(
                            matches!(&*func, Value::Lambda(_))
                                || matches!(&*func, Value::Function(_)),
                            {
                                // 🚧 I'm not really sure what to do about this here. It's
                                // all really a hack for now anyway.
                                let ty = PrintableValueType(&ty, context);
                                TypeMismatchSnafu {
                                    expected: "<function>".to_string(),
                                    found: ty.to_string(),
                                    span,
                                }
                            }
                        );

                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

                        let elapsed = if let Value::Function(func) = &*func {
                            let now = Instant::now();
                            let _result =
                                eval_function_call(func.clone(), &[], true, span, context, vm)?;
                            now.elapsed()
                        } else if let Value::Lambda(ƛ) = &*func {
                            let now = Instant::now();
                            let _result =
                                eval_lambda_expression(ƛ.clone(), &[], true, span, context, vm)?;
                            now.elapsed()
                        } else {
                            unreachable!()
                        };

                        // let time = format!("{:?}\n", elapsed);
                        // chacha_print(time, context)?;

                        let ty = Ty::new_float(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

                        Ok((new_ref!(Value, Value::Float(elapsed.as_secs_f64())), ty))
                    }
                    "eps" => {
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

                        let ty = Ty::new_s_string(&s_read!(sarzak));
                        let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

                        Ok((new_ref!(Value, Value::String(result)), ty))
                    }
                    "assert_eq" => {
                        debug!("evaluating chacha::assert_eq");
                        // 🚧 Check that there are two arguments
                        let lhs = arg_values.pop_front().unwrap().0 .0;
                        let rhs = arg_values.pop_front().unwrap().0 .0;

                        debug!("lhs: {lhs:?}, rhs {rhs:?}");

                        let value = Value::Boolean(*s_read!(lhs) == *s_read!(rhs));

                        if let Value::Boolean(result) = value {
                            // if value.into() {
                            if result {
                                let ty = Ty::new_boolean(&s_read!(sarzak));
                                let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));

                                Ok((new_ref!(Value, value), ty))
                            } else {
                                let source =
                                    s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
                                let source = s_read!(source);
                                let source = &source.source;

                                let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];

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
                    method => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let read = s_read!(span);
                        let span = read.start as usize..read.end as usize;

                        Err(ChaChaError::NoSuchStaticMethod {
                            ty: ty.to_owned(),
                            method: method.to_owned(),
                            span,
                        })
                    }
                }
            } else if let Some(value) = context.memory().get_meta(ty, func) {
                debug!("StaticMethodCall meta value {value:?}");
                match &*s_read!(value) {
                    Value::Function(ref func) => {
                        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
                        debug!("StaticMethodCall::Function {value:?}");
                        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                        let func = s_read!(lu_dog).exhume_function(&s_read!(func).id).unwrap();
                        debug!("StaticMethodCall meta func {func:?}");
                        let (value, ty) =
                            eval_function_call(func, &args, arg_check, span, context, vm)?;
                        debug!("StaticMethodCall meta value {value:?}");
                        debug!("StaticMethodCall meta ty {ty:?}");
                        Ok((value, ty))
                    }
                    value => {
                        error!("deal with call expression {value:?}");
                        Ok((
                            new_ref!(Value, Value::Empty),
                            Value::Empty.get_type(&s_read!(sarzak), &s_read!(lu_dog)),
                        ))
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
                        let (value, ty) =
                            eval_function_call(func, &args, arg_check, span, context, vm)?;
                        debug!("StaticMethodCall frame value {value:?}");
                        debug!("StaticMethodCall frame ty {ty:?}");
                        Ok((value, ty))
                    }
                    Value::ProxyType(ut) => {
                        debug!("StaticMethodCall proxy {ut:?}");
                        s_write!(ut).call(
                            func,
                            &mut arg_values.iter().map(|v| v.0 .0.clone()).collect(),
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
                unimplemented!();
                // We never will get here.
            }
        }
    };

    call_result
}
