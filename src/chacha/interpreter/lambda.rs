use std::time::Instant;

use ansi_term::Colour;
use snafu::{location, prelude::*, Location};
use tracy_client::span;

use crate::{
    chacha::{
        error::{Result, WrongNumberOfArgumentsSnafu},
        vm::VM,
    },
    interpreter::{
        debug, eval_expression, eval_statement, function, trace, typecheck, ChaChaError, Context,
    },
    lu_dog::{Argument, Lambda, Span, ValueType},
    new_ref, s_read, NewRef, RefType, Value,
};

pub fn eval_lambda_expression(
    ƛ: RefType<Lambda>,
    args: &[RefType<Argument>],
    arg_check: bool,
    span: &RefType<Span>,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    context.increment_call_count();

    debug!("ƛ {ƛ:?}");
    trace!("stack {:?}", context.memory());

    span!("eval_lambda_expression");

    let ƛ = s_read!(ƛ);
    // We know that we have a block.
    let block = &ƛ.r73_block(&s_read!(lu_dog))[0];
    // let stmts = s_read!(block).r18_statement(&s_read!(lu_dog));
    let has_stmts = !s_read!(block).r18_statement(&s_read!(lu_dog)).is_empty();

    if has_stmts {
        // Collect timing info
        let now = Instant::now();
        let expr_count_start = context.get_expression_count();

        context.memory().push_frame();

        // We need to evaluate the arguments, and then push them onto the stack. We
        // also need to typecheck the arguments against the function parameters.
        // We need to look the params up anyway to set the local variables.
        let params = ƛ.r76_lambda_parameter(&s_read!(lu_dog));

        // 🚧 I'd really like to see the source code printed out, with the function
        // call highlighted.
        // And can't we catch this is the compiler?
        ensure!(params.len() == args.len(), {
            let value_ty = &ƛ.r1_value_type(&s_read!(lu_dog))[0];
            let defn_span = &s_read!(value_ty).r62_span(&s_read!(lu_dog))[0];
            let read = s_read!(defn_span);
            let defn_span = read.start as usize..read.end as usize;

            let read = s_read!(span);
            let invocation_span = read.start as usize..read.end as usize;

            WrongNumberOfArgumentsSnafu {
                expected: params.len(),
                got: args.len(),
                defn_span,
                invocation_span,
            }
        });

        let params = if !params.is_empty() {
            let mut params = Vec::with_capacity(params.len());
            let mut next = ƛ
                // .clone()
                .r76_lambda_parameter(&s_read!(lu_dog))
                .iter()
                .find(|p| {
                    s_read!(p)
                        .r75c_lambda_parameter(&s_read!(lu_dog))
                        .is_empty()
                })
                .unwrap()
                .clone();

            loop {
                let var = s_read!(s_read!(next).r12_variable(&s_read!(lu_dog))[0]).clone();
                let value = s_read!(var.r11_x_value(&s_read!(lu_dog))[0]).clone();
                let ty = value.r24_value_type(&s_read!(lu_dog))[0].clone();
                params.push((var.name.clone(), ty.clone()));

                let next_id = { s_read!(next).next };
                if let Some(ref id) = next_id {
                    next = s_read!(lu_dog).exhume_lambda_parameter(id).unwrap();
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
                let expr = s_read!(next).r37_expression(&s_read!(lu_dog))[0].clone();
                let value = eval_expression(expr.clone(), context, vm)?;
                arg_values.push((expr, value));

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
        for ((name, param_ty), (expr, value)) in zipped {
            debug!("type check name {name:?}");
            debug!("type check param_ty {param_ty:?}");
            debug!("type check value {value:?}");

            if arg_check {
                let x_value = &s_read!(expr).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(x_value).r63_span(&s_read!(lu_dog))[0];

                let arg_ty = s_read!(value).get_type(&s_read!(sarzak), &s_read!(lu_dog));
                typecheck(&param_ty, &arg_ty, span, location!(), context)?;
            }

            context.memory().insert(name.clone(), value);
        }

        let mut value = new_ref!(Value, Value::Empty);
        let mut ty = Value::Empty.get_type(&s_read!(sarzak), &s_read!(lu_dog));
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
                    // it, and then let it continue on as if nothing happened.
                    //
                    // Anyway, we need to clean up the stack frame if there was an
                    // error. I'm also considering abusing the error type to pass
                    // through that we hit a return expression. I'm thinking more
                    // and more that this is a Good Idea. Well, maybe just a good
                    // idea. We can basically just do an early, successful return.
                    //
                    // Well, that doesn't work: return applies to the closure.
                    context.memory().pop_frame();

                    // if let ChaChaError::Return { value } = &e {
                    //     let ty = value.get_type(&mut s_write!(lu_dog));
                    //     return Ok((value, ty));
                    // }

                    // Err(e)
                    e
                });

                if let Err(ChaChaError::Return { value, ty }) = &result {
                    return Ok(value.clone());
                }

                value = result?;

                if let Some(ref id) = s_read!(next.clone()).next {
                    next = s_read!(lu_dog).exhume_statement(id).unwrap();
                } else {
                    break;
                }
            }
        }

        // Clean up
        context.memory().pop_frame();
        let elapsed = now.elapsed();
        // Counting 10k expressions per second
        let eps = (context.get_expression_count() - expr_count_start) as f64
            / elapsed.as_micros() as f64
            * 10.0;
        context.new_timing(eps);

        Ok(value)
    } else {
        Ok(new_ref!(Value, Value::Empty))
    }
}
