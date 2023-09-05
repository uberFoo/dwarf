use crate::{
    chacha::{error::Result, value::EnumFieldVariant, vm::VM},
    interpreter::{eval_expression, Context},
    lu_dog::{EnumFieldEnum, ExpressionEnum},
    s_read, RefType, SarzakStorePtr, Value,
};

pub fn eval(
    match_expr: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let match_expr = s_read!(lu_dog).exhume_x_match(match_expr).unwrap();
    let match_expr = s_read!(match_expr);

    let patterns = match_expr.r87_pattern(&s_read!(lu_dog));
    let scrutinee = match_expr.r91_expression(&s_read!(lu_dog))[0].clone();

    let scrutinee = eval_expression(scrutinee, context, vm)?;
    // dbg!(&scrutinee);

    // dbg!(match_expr, &patterns);

    for pattern in patterns {
        // 🚧 Somehow we have to decide which one of these matches the scrutinee.
        let match_expr = s_read!(pattern).r87_expression(&s_read!(lu_dog))[0].clone();
        // dbg!(&match_expr);

        let match_expr = s_read!(match_expr);
        if let ExpressionEnum::EnumField(ref enum_field) = &match_expr.subtype {
            let field = s_read!(lu_dog).exhume_enum_field(enum_field).unwrap();
            let field = s_read!(field);
            let woog_enum = &field.r88_enumeration(&s_read!(lu_dog))[0];
            let woog_enum = s_read!(woog_enum);
            let _ty = woog_enum.r1_value_type(&s_read!(lu_dog))[0].clone();

            match field.subtype {
                EnumFieldEnum::Plain(_) => {
                    // dbg!("Plain", &field.name);
                    if let Value::Enum(e) = &*s_read!(scrutinee) {
                        let value = s_read!(e).get_value();
                        // dbg!(&value);
                        let value = s_read!(value);

                        if let Value::EnumVariant(EnumFieldVariant::Plain(field_name)) = &*value {
                            if field_name == &field.name {
                                // dbg!("Matched");
                                let expr =
                                    s_read!(pattern).r92_expression(&s_read!(lu_dog))[0].clone();

                                context.memory().push_frame();

                                let value = eval_expression(expr, context, vm)?;

                                context.memory().pop_frame();
                                return Ok(value);
                            } else {
                                unreachable!()
                            }
                        }
                    } else {
                        unreachable!()
                    }
                }
                // new_ref!(
                //             Value,
                //             Value::EnumVariant(EnumFieldVariant::Plain(field.name.to_string()))
                //         ),
                EnumFieldEnum::StructField(ref sf) => {
                    let struct_field = s_read!(lu_dog).exhume_struct_field(sf).unwrap();
                    let struct_field = s_read!(struct_field);
                    let expr = struct_field.expression.unwrap();
                    let _expr = s_read!(lu_dog).exhume_expression(&expr).unwrap();
                    unimplemented!();
                    // dbg!("StructField", &field.name, expr);
                    //             let (value, _) = eval_expression(expr, context, vm)?;
                    //             let value = s_read!(value);
                    //             if let Value::Struct(struct_value) = &*value {
                    //                 let struct_value = s_read!(struct_value);
                    //                 new_ref!(
                    //                     Value,
                    //                     Value::EnumVariant(EnumFieldVariant::Struct(
                    //                         field.name.to_owned(),
                    //                         new_ref!(UserStruct, struct_value.clone())
                    //                     ))
                    //                 )
                    //             } else {
                    //                 unreachable!()
                    //             }
                }
                EnumFieldEnum::TupleField(ref tf) => {
                    let tuple = s_read!(lu_dog).exhume_tuple_field(tf).unwrap();
                    let _ty = s_read!(tuple).r86_value_type(&s_read!(lu_dog))[0].clone();
                    let expr = s_read!(tuple).r90_expression(&s_read!(lu_dog))[0].clone();
                    // dbg!("TupleField", &field.name, ty, &expr);
                    // let (value, _) = eval_expression(expr, context, vm)?;
                    let expr = s_read!(expr);
                    if let ExpressionEnum::VariableExpression(ref var_expr) = expr.subtype {
                        let var = s_read!(lu_dog)
                            .exhume_variable_expression(var_expr)
                            .unwrap();
                        // dbg!("VariableExpression", &var);
                        if let Value::Enum(e) = &*s_read!(scrutinee) {
                            let value = s_read!(e).get_value();
                            // dbg!(&value);
                            let value = s_read!(value);

                            if let Value::EnumVariant(EnumFieldVariant::Tuple(field_name, value)) =
                                &*value
                            {
                                if field_name == &field.name {
                                    // dbg!("Matched");
                                    let expr = s_read!(pattern).r92_expression(&s_read!(lu_dog))[0]
                                        .clone();

                                    context.memory().push_frame();

                                    context
                                        .memory()
                                        .insert(s_read!(var).name.to_owned(), value.clone());
                                    let value = eval_expression(expr, context, vm)?;

                                    context.memory().pop_frame();

                                    return Ok(value);
                                } else {
                                    unreachable!()
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
            };
        }
    }

    unreachable!()
}

// fn deconstruct_expression(expr: RefCell<Expression>) {}
