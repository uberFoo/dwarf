use std::collections::VecDeque;

use snafu::{location, Location};

use crate::{
    bubba::VM,
    chacha::{
        error::{ChaChaError, Result},
        value::EnumVariant,
    },
    interpreter::{eval_expression, Context},
    lu_dog::ExpressionEnum,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value, PATH_SEP,
};

pub fn eval(
    match_expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let match_expr = lu_dog.exhume_x_match(match_expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&lu_dog);
    let scrutinee = match_expr.r91_expression(&lu_dog)[0].clone();

    let scrutinee = eval_expression(scrutinee, context, vm)?;

    // Check each pattern for a match.
    // 🚧 Darn. Match arms need to be ordered the same as they are written, and
    // they are not ordered in the model.
    for pattern in patterns {
        let match_expr = s_read!(pattern).r87_expression(&lu_dog)[0].clone();
        let pattern_expr = s_read!(pattern).r92_expression(&lu_dog)[0].clone();

        let match_expr_read = s_read!(match_expr);
        match &match_expr_read.subtype {
            ExpressionEnum::EmptyExpression(ref _id) => {
                let value = eval_expression(pattern_expr, context, vm)?;
                return Ok(value);
            }
            ExpressionEnum::Literal(ref _id) => {
                let value = eval_expression(match_expr.clone(), context, vm)?;
                let value = s_read!(value);
                if *s_read!(scrutinee) == *value {
                    let value = eval_expression(pattern_expr, context, vm)?;
                    return Ok(value);
                }
            }
            ExpressionEnum::StructExpression(ref id) => {
                let struct_expr = lu_dog.exhume_struct_expression(id).unwrap();
                let field_exprs = s_read!(struct_expr).r26_field_expression(&lu_dog);
                // let data_struct = &s_read!(struct_expr).r39_data_structure(&lu_dog)[0];

                // This thing tears apart a value exposing each element as a (string, value) tuple.
                fn decode_expression(value: RefType<Value>) -> (String, Option<RefType<Value>>) {
                    match &*s_read!(value) {
                        Value::Boolean(value) => (value.to_string(), None),
                        Value::Enumeration(value) => match value {
                            // 🚧 I can't tell if this is gross, or a sweet hack.
                            // I think I'm referring to using the name as the scrutinee?
                            EnumVariant::Unit(_, ty, value) => (
                                ty.to_owned(),
                                Some(new_ref!(Value, Value::String(value.to_owned()))),
                            ),
                            // EnumFieldVariant::Struct(value) => (
                            //     s_read!(value).type_name().to_owned(),
                            //     Some(s_read!(value).get_value()),
                            // ),
                            EnumVariant::Tuple((ty, path), value) => {
                                let path = path.split(PATH_SEP).collect::<Vec<&str>>();
                                let mut path = VecDeque::from(path);
                                let name = path.pop_front().unwrap().to_owned();
                                if name.is_empty() {
                                    (
                                        s_read!(value).variant().to_owned(),
                                        Some(s_read!(value).value().clone()),
                                    )
                                } else {
                                    (
                                        name,
                                        Some(new_ref!(
                                            Value,
                                            Value::Enumeration(EnumVariant::Tuple(
                                                (
                                                    ty.clone(),
                                                    path.into_iter()
                                                        .collect::<Vec<&str>>()
                                                        .join(PATH_SEP)
                                                ),
                                                value.clone(),
                                            ))
                                        )),
                                    )
                                }
                            }
                            _ => unimplemented!(),
                        },
                        Value::String(value) => (value.to_owned(), None),
                        huh => panic!("match encountered {huh}"),
                    }
                }

                // Below we are iterating over each element in the expression path
                // and testing it against the scrutinee value.

                let x_path = &lu_dog.exhume_x_path(&s_read!(struct_expr).x_path).unwrap();
                // We know that there is always a pe. It's only in an option so that
                // we can construct everything.
                let mut pe = s_read!(x_path).r97_path_element(&lu_dog)[0].clone();

                let mut matched = false;
                let (name, mut scrutinee) = decode_expression(scrutinee.clone());
                if name == s_read!(pe).name {
                    while s_read!(pe).next.is_some() && scrutinee.is_some() {
                        let id = {
                            let id = &s_read!(pe).next;
                            #[allow(clippy::clone_on_copy)]
                            id.as_ref().unwrap().clone()
                        };
                        pe = lu_dog.exhume_path_element(&id).unwrap();
                        let (name, s) = decode_expression(scrutinee.unwrap());
                        scrutinee = s;

                        if name == s_read!(pe).name {
                            matched = true;
                            continue;
                        } else {
                            matched = false;
                            break;
                        }
                    }
                }

                // Assuming that we matched the path we need to now evaluate the
                // pattern expression.
                match (matched, field_exprs.len()) {
                    (true, 0) => {
                        let value = eval_expression(pattern_expr, context, vm)?;
                        return Ok(value);
                    }
                    (true, _) => {
                        // 🚧 We are only working on the first one?
                        let field_expr = s_read!(field_exprs[0]).r38_expression(&lu_dog)[0].clone();
                        let field_expr_read = s_read!(field_expr);
                        match field_expr_read.subtype {
                            ExpressionEnum::Literal(ref _id) => {
                                let value = eval_expression(field_expr.clone(), context, vm)?;
                                let value = s_read!(value);
                                if *s_read!(scrutinee.unwrap()) == *value {
                                    let value = eval_expression(pattern_expr, context, vm)?;
                                    return Ok(value);
                                }
                            }
                            ExpressionEnum::VariableExpression(ref var) => {
                                let var = lu_dog.exhume_variable_expression(var).unwrap();

                                context.memory().push_frame();
                                context
                                    .memory()
                                    .insert(s_read!(var).name.to_owned(), scrutinee.unwrap());
                                let value = eval_expression(pattern_expr, context, vm)?;

                                context.memory().pop_frame();

                                return Ok(value);
                            }
                            _ => unimplemented!(),
                        }
                    }
                    (false, _) => {}
                }
            }
            ExpressionEnum::VariableExpression(ref id) => {
                let var = lu_dog.exhume_variable_expression(id).unwrap();

                context.memory().push_frame();

                context
                    .memory()
                    .insert(s_read!(var).name.to_owned(), scrutinee);
                let value = eval_expression(pattern_expr, context, vm)?;

                context.memory().pop_frame();
                return Ok(value);
            }
            oops => panic!("{oops:?}"),
        }
    }

    Err(ChaChaError::BadnessHappened {
        message: "fall through match expression".to_owned(),
        location: location!(),
    })
}
