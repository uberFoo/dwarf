use std::ops::Range;

use ansi_term::Colour;
use snafu::location;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            debug, e_warn, function, inter_expression, link_argument,
            lookup_woog_enum_method_return_type, lookup_woog_struct_method_return_type,
            update_span_value, Context, ExprSpan,
        },
        Expression as ParserExpression, PrintableValueType,
    },
    keywords::{
        FORMAT, GET, INSERT, INVOKE_FUNC, INVOKE_FUNC_MUT, IS_DIGIT, JOIN, LEN, LINES, MAP, MAX,
        OPTION_TYPE, PUSH, REPLACE, SPLIT, SUM, TO_DIGIT, TRIM,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, Expression, List, MethodCall,
        Span, Span as LuDogSpan, ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr,
};

#[allow(clippy::too_many_arguments)]
pub(in crate::dwarf::extruder) fn inter(
    instance: Box<(ParserExpression, Range<usize>)>,
    method: &String,
    meth_span: Range<usize>,
    args: Vec<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("MethodCall Enter: instance: {instance:?}, method: `{method}`");

    let (instance, instance_ty) = inter_expression(
        &new_ref!(ParserExpression, instance.0.to_owned()),
        &instance.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;

    debug!("MethodCall instance: {instance:?}, type: {instance_ty:?}");

    let meth = MethodCall::new(method.to_owned(), lu_dog);
    let call = Call::new_method_call(true, None, Some(&instance.0), &meth, lu_dog);
    let expr = Expression::new_call(true, &call, lu_dog);

    let value = XValue::new_expression(block, &instance_ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    let mut last_arg_uuid: Option<SarzakStorePtr> = None;
    let mut arg_ty = Vec::new();

    // Self
    // This is the self parameter
    // Self -- I can never seem to find this.
    let this = Argument::new(0, &instance.0, &call, None, lu_dog);
    last_arg_uuid = link_argument!(last_arg_uuid, this, lu_dog);
    s_write!(call).argument = Some(s_read!(this).id);

    // Note the position.
    let mut position = 1;
    for arg in args {
        let (arg_expr, ty) = inter_expression(
            &new_ref!(ParserExpression, arg.0.to_owned()),
            &arg.1,
            block,
            context,
            import_stack,
            lu_dog,
        )?;
        let value = XValue::new_expression(block, &ty, &arg_expr.0, lu_dog);
        let _span = LuDogSpan::new(
            arg.1.end as i64,
            arg.1.start as i64,
            &context.source,
            None,
            Some(&value),
            lu_dog,
        );
        let arg = Argument::new(position, &arg_expr.0, &call, None, lu_dog);
        position += 1;

        last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
        // Note that self isn't being push onto this vec.
        arg_ty.push(ty);
    }

    let ret_ty =
        method_call_return_type(instance_ty, method, meth_span, &mut arg_ty, context, lu_dog)?;

    debug!(
        "{}: {method} return type {}",
        Colour::Red.italic().paint("MethodCall"),
        PrintableValueType(true, &ret_ty, context, lu_dog).to_string()
    );

    Ok(((expr, span), ret_ty))
}

pub(in crate::dwarf::extruder) fn method_call_return_type(
    instance_ty: RefType<ValueType>,
    method: &String,
    meth_span: Range<usize>,
    arg_ty: &mut Vec<RefType<ValueType>>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<RefType<ValueType>> {
    debug!(
        "{} instance type {instance_ty:?} ({}) {method}",
        Colour::Red.italic().paint("method_call_return_type"),
        PrintableValueType(true, &instance_ty, context, lu_dog).to_string()
    );
    let ty = match s_read!(instance_ty).subtype {
        ValueTypeEnum::AnyList(_) => match method.as_str() {
            JOIN => {
                let ty = Ty::new_z_string(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            LEN => {
                let ty = Ty::new_integer(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            MAP => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                ValueType::new_any_list(true, lu_dog)
            }
            PUSH => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
                let arg_ty = arg_ty.pop().unwrap();

                arg_ty.clone()
            }
            SUM => instance_ty.clone(),
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ValueTypeEnum::Char(_) => match method.as_str() {
            IS_DIGIT => {
                let ty = Ty::new_boolean(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            TO_DIGIT => {
                let ty = Ty::new_integer(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ValueTypeEnum::Enumeration(ref id) => {
            let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
            let x = lookup_woog_enum_method_return_type(
                &s_read!(woog_enum).name,
                method,
                meth_span,
                context,
                lu_dog,
            )?;

            #[allow(clippy::let_and_return)]
            x
        }
        ValueTypeEnum::EnumGeneric(ref generic) => {
            let generic = lu_dog.exhume_enum_generic(generic).unwrap();
            let woog_enum = &s_read!(generic).r104_enumeration(lu_dog)[0];
            let ty = s_read!(woog_enum).r1_value_type(lu_dog)[0].clone();
            // let ty = s_read!(generic).r1_value_type(lu_dog)[0].clone();
            // let ty_str = PrintableValueType(true, &ty, context, lu_dog);

            // dbg!(ty_str.to_string());
            ty
        }
        ValueTypeEnum::List(ref list) => match method.as_str() {
            JOIN => {
                let ty = Ty::new_z_string(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            LEN => {
                let ty = Ty::new_integer(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            MAP => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
                let inner = arg_ty.pop().unwrap();
                let list = List::new(&inner, lu_dog);
                ValueType::new_list(true, &list, lu_dog)
            }
            PUSH => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
                let arg_ty = arg_ty.pop().unwrap();
                let list = lu_dog.exhume_list(list).unwrap();

                let inner_ty = s_read!(list).ty;
                let inner_ty = lu_dog.exhume_value_type(&inner_ty).unwrap();
                let r_inner_ty = s_read!(inner_ty);

                if s_read!(arg_ty).subtype != r_inner_ty.subtype {
                    // let expected_span = &inner_ty.r62_span(lu_dog)[0];
                    // let expected_span = s_read!(expected_span);
                    // let expected_span = expected_span.start as usize..expected_span.end as usize;
                    let expected_span = 0..0;

                    dbg!(&arg_ty, &r_inner_ty);

                    return Err(vec![DwarfError::TypeMismatch {
                        expected: PrintableValueType(true, &inner_ty, context, lu_dog).to_string(),
                        found: PrintableValueType(true, &arg_ty, context, lu_dog).to_string(),
                        file: context.file_name.to_owned(),
                        expected_span,
                        found_span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                arg_ty.clone()
            }
            SUM => instance_ty.clone(),
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ValueTypeEnum::Map(ref map) => match method.as_str() {
            INSERT => {
                if arg_ty.len() != 2 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 2,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
                let key_ty = arg_ty.pop().unwrap();
                let value_ty = arg_ty.pop().unwrap();
                let map = lu_dog.exhume_map(map).unwrap();
                let map = s_read!(map);

                let map_key_type = map.key_type;
                let map_key_type = lu_dog.exhume_value_type(&map_key_type).unwrap();
                let map_key_type = s_read!(map_key_type);

                if &*s_read!(key_ty) != &*map_key_type {
                    // let expected_span = &map_key_type.r62_span(lu_dog)[0];
                    // let expected_span = s_read!(expected_span);
                    // let expected_span = expected_span.start as usize..expected_span.end as usize;
                    let expected_span = 0..0;

                    return Err(vec![DwarfError::TypeMismatch {
                        expected: PrintableValueType(true, &instance_ty, context, lu_dog)
                            .to_string(),
                        found: PrintableValueType(true, &key_ty, context, lu_dog).to_string(),
                        file: context.file_name.to_owned(),
                        expected_span,
                        found_span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                let map_value_type = map.value_type;
                let map_value_type = lu_dog.exhume_value_type(&map_value_type).unwrap();
                let map_value_type = s_read!(map_value_type);

                if &*s_read!(value_ty) != &*map_value_type {
                    let expected_span = &map_value_type.r62_span(lu_dog)[0];
                    let expected_span = s_read!(expected_span);
                    let expected_span = expected_span.start as usize..expected_span.end as usize;

                    return Err(vec![DwarfError::TypeMismatch {
                        expected: PrintableValueType(true, &instance_ty, context, lu_dog)
                            .to_string(),
                        found: PrintableValueType(true, &value_ty, context, lu_dog).to_string(),
                        file: context.file_name.to_owned(),
                        expected_span,
                        found_span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                ValueType::new_empty(true, lu_dog)
            }
            GET => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                // let map = lu_dog.exhume_map(map).unwrap();
                // let map = s_read!(map);
                // Ideally this becomes part of the type we are returning.
                // let value_ty = map.r116_value_type(lu_dog)[0].clone();

                let Some(ty) = lu_dog.exhume_enumeration_id_by_name(OPTION_TYPE) else {
                    return Err(vec![DwarfError::ObjectNameNotFound {
                        name: OPTION_TYPE.to_owned(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                };
                let option = lu_dog.exhume_enumeration(&ty).unwrap();

                let result = s_read!(option).r1_value_type(lu_dog)[0].clone();

                result
            }
            LEN => {
                let ty = Ty::new_integer(context.sarzak);
                ValueType::new_ty(true, &ty, lu_dog)
            }
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ValueTypeEnum::Range(_) => match method.as_str() {
            MAP => {
                if arg_ty.len() != 1 {
                    return Err(vec![DwarfError::WrongNumberOfArguments {
                        expected: 1,
                        found: arg_ty.len(),
                        file: context.file_name.to_owned(),
                        span: meth_span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
                let inner = arg_ty.pop().unwrap();
                let list = List::new(&inner, lu_dog);
                ValueType::new_list(true, &list, lu_dog)
            }
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ValueTypeEnum::Ty(ref id) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::Integer(_) => match method.as_str() {
                    MAX => {
                        let ty = Ty::new_integer(context.sarzak);
                        ValueType::new_ty(true, &ty, lu_dog)
                    }
                    _ => {
                        return Err(vec![DwarfError::NoSuchMethod {
                            method: method.to_owned(),
                            file: context.file_name.to_owned(),
                            span: meth_span.to_owned(),
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }])
                    }
                },
                Ty::ZString(_) => {
                    match method.as_str() {
                        FORMAT => {
                            let ty = Ty::new_z_string(context.sarzak);
                            ValueType::new_ty(true, &ty, lu_dog)
                        }
                        LEN => {
                            let ty = Ty::new_integer(context.sarzak);
                            ValueType::new_ty(true, &ty, lu_dog)
                        }
                        LINES => {
                            let string = Ty::new_z_string(context.sarzak);
                            let string = ValueType::new_ty(true, &string, lu_dog);
                            let list = List::new(&string, lu_dog);
                            ValueType::new_list(true, &list, lu_dog)
                        }
                        REPLACE => {
                            let ty = Ty::new_z_string(context.sarzak);
                            ValueType::new_ty(true, &ty, lu_dog)
                        }
                        SPLIT => {
                            let string = Ty::new_z_string(context.sarzak);
                            let string = ValueType::new_ty(true, &string, lu_dog);
                            let list = List::new(&string, lu_dog);
                            ValueType::new_list(true, &list, lu_dog)
                        }
                        TRIM => {
                            let ty = Ty::new_z_string(context.sarzak);
                            ValueType::new_ty(true, &ty, lu_dog)
                        }
                        _ => {
                            return Err(vec![DwarfError::NoSuchMethod {
                                method: method.to_owned(),
                                file: context.file_name.to_owned(),
                                span: meth_span.to_owned(),
                                location: location!(),
                                // commentary: "Type `string` has no such method.".to_owned(),
                                program: context.source_string.to_owned(),
                            }]);
                        }
                    }
                }
                _ => {
                    e_warn!("Unknown type for method call {method}");
                    ValueType::new_unknown(true, lu_dog)
                }
            }
        }
        ValueTypeEnum::WoogStruct(id) => {
            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
            let x = lookup_woog_struct_method_return_type(
                &s_read!(woog_struct).name,
                method,
                meth_span,
                context,
                lu_dog,
            )?;

            #[allow(clippy::let_and_return)]
            x
        }
        ValueTypeEnum::XPlugin(_) => match method.as_str() {
            // INVOKE_FUNC => ValueType::new_empty(true, lu_dog),
            INVOKE_FUNC => ValueType::new_unknown(true, lu_dog),
            INVOKE_FUNC_MUT => ValueType::new_unknown(true, lu_dog),
            _ => {
                return Err(vec![DwarfError::NoSuchMethod {
                    method: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span: meth_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        },
        ref ty => {
            return Err(vec![DwarfError::Internal {
                description: format!("unknown type for method call: `{ty:?}`"),
                file: context.file_name.to_owned(),
                span: meth_span.to_owned(),
                location: location!(),
                program: context.source_string.to_owned(),
            }])
        }
    };

    Ok(ty)
}
