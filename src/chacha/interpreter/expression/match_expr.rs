use crate::{
    bubba::VM,
    chacha::{error::Result, value::EnumVariant},
    interpreter::{eval_expression, Context},
    lu_dog::ExpressionEnum,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
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

    // Check each pattern for a match.
    // 🚧 Darn. Match arms need to be ordered the same as they are written, and
    // they are not ordered in the model.
    for pattern in patterns {
        let match_expr = s_read!(pattern).r87_expression(&s_read!(lu_dog))[0].clone();
        let expr = s_read!(pattern).r92_expression(&s_read!(lu_dog))[0].clone();

        let match_expr = s_read!(match_expr);
        match &match_expr.subtype {
            ExpressionEnum::StructExpression(ref id) => {
                let struct_expr = s_read!(lu_dog).exhume_struct_expression(id).unwrap();
                let field_exprs = s_read!(struct_expr).r26_field_expression(&s_read!(lu_dog));
                // let data_struct = &s_read!(struct_expr).r39_data_structure(&s_read!(lu_dog))[0];

                fn decode_value(value: RefType<Value>) -> (String, Option<RefType<Value>>) {
                    match &*s_read!(value) {
                        Value::Enumeration(value) => match value {
                            // 🚧 I can't tell if this is gross, or a sweet hack.
                            EnumVariant::Unit(_, ty, value) => (
                                ty.to_owned(),
                                Some(new_ref!(Value, Value::String(value.to_owned()))),
                            ),
                            // EnumFieldVariant::Struct(value) => (
                            //     s_read!(value).type_name().to_owned(),
                            //     Some(s_read!(value).get_value()),
                            // ),
                            EnumVariant::Tuple((_, ty), value) => (
                                ty.to_owned(),
                                Some(new_ref!(Value, Value::TupleEnum(value.clone()))),
                            ),
                            _ => unimplemented!(),
                        },
                        Value::String(value) => (value.to_owned(), None),
                        Value::TupleEnum(te) => (
                            s_read!(te).variant().to_owned(),
                            Some(s_read!(te).value().clone()),
                        ),
                        _ => unreachable!(),
                    }
                }

                // if let Value::Enum(value) = &*s_read!(scrutinee) {
                let x_path = &s_read!(lu_dog)
                    .exhume_x_path(&s_read!(struct_expr).x_path)
                    .unwrap();
                // We know that there is always a pe. It's only in an option so that
                // we can construct everything.
                let mut pe = s_read!(x_path).r97_path_element(&s_read!(lu_dog))[0].clone();

                let mut matched = false;
                let (name, mut scrutinee) = decode_value(scrutinee.clone());
                if name == s_read!(pe).name {
                    while s_read!(pe).next.is_some() && scrutinee.is_some() {
                        let id = {
                            let id = &s_read!(pe).next;
                            #[allow(clippy::clone_on_copy)]
                            id.as_ref().unwrap().clone()
                        };
                        pe = s_read!(lu_dog).exhume_path_element(&id).unwrap();
                        let (name, s) = decode_value(scrutinee.unwrap());
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

                match (matched, field_exprs.len()) {
                    (true, 0) => {
                        let value = eval_expression(expr, context, vm)?;
                        return Ok(value);
                    }
                    (true, _) => {
                        let field_expr =
                            s_read!(field_exprs[0]).r38_expression(&s_read!(lu_dog))[0].clone();
                        let field_expr = s_read!(field_expr);
                        if let ExpressionEnum::VariableExpression(ref var) = field_expr.subtype {
                            let var = s_read!(lu_dog).exhume_variable_expression(var).unwrap();

                            context.memory().push_frame();

                            context
                                .memory()
                                .insert(s_read!(var).name.to_owned(), scrutinee.unwrap());
                            let value = eval_expression(expr, context, vm)?;

                            context.memory().pop_frame();
                            return Ok(value);
                        }
                    }
                    (false, _) => {}
                }
            }
            oops => panic!("{oops:?}"),
        }
    }

    // 🚧 What's this supposed to be?
    Ok(new_ref!(Value, Value::Boolean(false)))
}
