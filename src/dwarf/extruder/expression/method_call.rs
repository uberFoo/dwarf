use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            debug, e_warn, function, inter_expression, link_argument,
            lookup_woog_struct_method_return_type, update_span_value, Context, ExprSpan,
        },
        Expression as ParserExpression, PrintableValueType,
    },
    keywords::{FORMAT, IS_DIGIT, LEN, LINES, MAP, MAX, SPLIT, SUM, TO_DIGIT, TRIM},
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
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("MethodCall Enter: instance: {instance:?}, method: `{method}`");

    let (instance, instance_ty) = inter_expression(
        &new_ref!(ParserExpression, instance.0.to_owned()),
        &instance.1,
        block,
        context,
        context_stack,
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
            context_stack,
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
        arg_ty.push(ty);
    }

    let ret_ty =
        method_call_return_type(instance_ty, method, meth_span, &mut arg_ty, context, lu_dog)?;

    debug!(
        "{} return type {}",
        Colour::Red.dimmed().italic().paint("MethodCall"),
        PrintableValueType(&ret_ty, context, lu_dog).to_string()
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
        Colour::Red
            .dimmed()
            .italic()
            .paint("method_call_return_type"),
        PrintableValueType(&instance_ty, context, lu_dog).to_string()
    );
    let ty = match s_read!(instance_ty).subtype {
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
        ValueTypeEnum::List(_) => match method.as_str() {
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
        ValueTypeEnum::Range(_) => match method.as_str() {
            MAP => {
                let inner = ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog);
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
                        FORMAT => {
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
                context.sarzak,
                lu_dog,
            );

            #[allow(clippy::let_and_return)]
            x
        }
        ref ty => {
            e_warn!("Unknown type for method call {method}, {ty:?}");

            ValueType::new_unknown(true, lu_dog)
        }
    };

    Ok(ty)
}
