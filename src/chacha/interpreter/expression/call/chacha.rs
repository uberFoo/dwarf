use std::collections::VecDeque;

use ansi_term::Colour;
use snafu::{location, prelude::*, Location};

use crate::{
    chacha::error::{Result, WrongNumberOfArgumentsSnafu},
    dwarf::{new_lu_dog, Spanned},
    interpreter::{
        chacha_print, debug, function, initialize_interpreter, start_func, ChaChaError, Context,
    },
    lu_dog::{Expression, ObjectStore as LuDogStore},
    new_ref, s_read, Context as InterContext, NewRef, RefType, Value,
};

/// Evaluate a parsed string
///
pub(crate) fn eval_dwarf(
    mut arg_values: VecDeque<Spanned<RefType<Value>>>,
    expression: &RefType<Expression>,
    context: &mut Context,
) -> Result<RefType<Value>> {
    debug!("evaluating dwarf code");

    let value = &s_read!(expression).r11_x_value(&s_read!(context.lu_dog_heel()))[0];
    let span = &s_read!(value).r63_span(&s_read!(context.lu_dog_heel()))[0];
    let read = s_read!(span);
    let span = read.start as usize..read.end as usize;

    ensure!(arg_values.len() > 1, {
        WrongNumberOfArgumentsSnafu {
            expected: 2usize,
            got: arg_values.len(),
            defn_span: 0..0,
            invocation_span: span,
            location: location!(),
        }
    });

    let name = "eval_dwarf";
    let ctx = arg_values.pop_front().unwrap().0;
    let ctx: InterContext = (*s_read!(ctx)).clone().try_into()?;
    let func = arg_values.pop_front().unwrap().0;
    let func: String = (*s_read!(func)).clone().try_into()?;
    let mut args = Vec::new();
    while let Some(arg) = arg_values.pop_front() {
        args.push((*s_read!(arg.0)).clone().try_into()?);
    }

    let mut ctx =
        initialize_interpreter(2, context.get_home().clone(), ctx.clone()).map_err(|e| {
            chacha_print(
                crate::chacha::error::ChaChaErrorReporter(&e, false, &ctx.source(), name)
                    .to_string(),
                context,
            )
            .unwrap();
            ChaChaError::Eval {
                src: ctx.source(),
                span: span.clone(),
            }
        })?;

    ctx.add_args(args);

    let result = start_func(&func, false, &mut ctx.clone()).map_err(|e| {
        chacha_print(
            crate::chacha::error::ChaChaErrorReporter(&e, false, &ctx.source(), name).to_string(),
            context,
        )
        .unwrap();
        ChaChaError::Eval {
            src: ctx.source(),
            span,
        }
    })?;

    Ok(result)
}

/// Parse a string into a LuDogStore
///
pub(crate) fn parse_dwarf(
    mut arg_values: VecDeque<Spanned<RefType<Value>>>,
    expression: &RefType<Expression>,
    context: &Context,
) -> Result<RefType<Value>> {
    debug!("parsing dwarf source");

    let value = &s_read!(expression).r11_x_value(&s_read!(context.lu_dog_heel()))[0];
    let span = &s_read!(value).r63_span(&s_read!(context.lu_dog_heel()))[0];
    let read = s_read!(span);
    let span = read.start as usize..read.end as usize;

    ensure!(arg_values.len() == 1, {
        WrongNumberOfArgumentsSnafu {
            expected: 1usize,
            got: arg_values.len(),
            defn_span: 0..0,
            invocation_span: span,
            location: location!(),
        }
    });

    let name = "parse_dwarf";

    let source_code = arg_values.pop_front().unwrap().0;
    let source_code: String = (*s_read!(source_code)).clone().try_into()?;

    let ast = crate::dwarf::parse_dwarf(name, &source_code).map_err(|e| {
        eprintln!("{}", crate::dwarf::error::DwarfErrorReporter(&e, false));

        ChaChaError::Parse {
            src: source_code.clone(),
            span: span.clone(),
        }
    })?;

    let ctx = new_lu_dog(
        name.to_owned(),
        Some((source_code.clone(), &ast)),
        context.get_home(),
        &s_read!(context.sarzak_heel()),
    )
    .map_err(|errors| {
        for err in errors {
            eprintln!("{}", crate::dwarf::error::DwarfErrorReporter(&err, false));
        }

        ChaChaError::Parse {
            src: source_code,
            span,
        }
    })?;

    Ok(new_ref!(Value, Value::ParsedDwarf(ctx)))
}

/// Compare the two arguments and return a boolean value.
///
/// We put some effort into making error reporting as useful as possible.
pub(crate) fn assert_eq(
    mut arg_values: VecDeque<Spanned<RefType<Value>>>,
    expression: &RefType<Expression>,
    lu_dog: RefType<LuDogStore>,
) -> Result<RefType<Value>> {
    debug!("evaluating chacha::assert_eq");
    ensure!(arg_values.len() == 2, {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let read = s_read!(span);
        let span = read.start as usize..read.end as usize;

        WrongNumberOfArgumentsSnafu {
            expected: 2usize,
            got: arg_values.len(),
            defn_span: 0..0,
            invocation_span: span,
            location: location!(),
        }
    });

    let lhs = arg_values.pop_front().unwrap().0;
    let rhs = arg_values.pop_front().unwrap().0;

    debug!("lhs: {lhs:?}, rhs {rhs:?}");

    let value = Value::Boolean(*s_read!(lhs) == *s_read!(rhs));

    if let Value::Boolean(result) = value {
        if result {
            Ok(new_ref!(Value, value))
        } else {
            let source = s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
            let source = s_read!(source);
            let source = &source.source;

            let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];

            let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

            let read = s_read!(span);
            let span = read.start as usize..read.end as usize;

            Err(ChaChaError::AssertEqual {
                found: lhs,
                expected: rhs,
                code: source[span].to_owned(),
            })
        }
    } else {
        unreachable!()
    }
}

/// Compare the argument to the truth.
///
/// We put some effort into making error reporting as useful as possible.
pub(crate) fn assert(
    mut arg_values: VecDeque<Spanned<RefType<Value>>>,
    expression: &RefType<Expression>,
    lu_dog: RefType<LuDogStore>,
) -> Result<RefType<Value>> {
    debug!("evaluating chacha::assert");
    // 🚧 This should be happening in the extruder.
    ensure!(arg_values.len() == 1, {
        let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];
        let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
        let read = s_read!(span);
        let span = read.start as usize..read.end as usize;

        WrongNumberOfArgumentsSnafu {
            expected: 1usize,
            got: arg_values.len(),
            defn_span: 0..0,
            invocation_span: span,
            location: location!(),
        }
    });

    let truth = arg_values.pop_front().unwrap().0;

    debug!("truth: {truth:?}");
    let truth_value = &*s_read!(truth);
    let truth_value: bool = truth_value.try_into()?;

    let value = Value::Boolean(truth_value);

    if let Value::Boolean(result) = value {
        if result {
            Ok(new_ref!(Value, value))
        } else {
            let source = s_read!(lu_dog).iter_dwarf_source_file().next().unwrap();
            let source = s_read!(source);
            let source = &source.source;

            let value = &s_read!(expression).r11_x_value(&s_read!(lu_dog))[0];

            let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];

            let read = s_read!(span);
            let span = read.start as usize..read.end as usize;

            Err(ChaChaError::AssertTrue {
                found: truth.clone(),
                code: source[span].to_owned(),
            })
        }
    } else {
        unreachable!()
    }
}
