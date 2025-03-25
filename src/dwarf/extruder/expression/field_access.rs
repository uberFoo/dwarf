use std::ops::Range;

use log::debug;
use snafu::location;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{inter_expression, update_span_value, Context, ExprSpan, PrintableValueType},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, FieldAccess, FieldAccessTarget, Span,
        ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    lhs: Box<(ParserExpression, Range<usize>)>,
    rhs: (String, Range<usize>),
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("ParserExpression::FieldAccess lhs {:?}", lhs);
    debug!("ParserExpression::FieldAccess rhs {:?}", rhs);

    let (lhs, lhs_ty) = inter_expression(
        &new_ref!(ParserExpression, lhs.0.clone()),
        &lhs.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;

    let id = s_read!(lhs_ty).id;
    let ty = lu_dog.exhume_value_type(&id).unwrap();
    let ty_read = s_read!(ty);

    match &ty_read.subtype {
        // We matched on the lhs type.
        ValueTypeEnum::Function(ref _id) => Ok((lhs, ty.clone())),
        ValueTypeEnum::WoogStruct(ref id) => {
            let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
            let fields = s_read!(woog_struct).r7_field(lu_dog);
            let field = fields.iter().find(|f| s_read!(f).name == rhs.0);

            if let Some(field) = field {
                let field = lu_dog.exhume_field(&s_read!(field).id);
                let func = if let Some(impl_) =
                    s_read!(woog_struct).r8c_implementation_block(lu_dog).pop()
                {
                    let funcs = s_read!(impl_).r9_function(lu_dog);
                    funcs.iter().find(|f| s_read!(f).name == rhs.0).cloned()
                } else {
                    None
                };

                debug!("field {:?}", field);
                debug!("func {:?}", func);

                // We need to grab the type from the field: what we have above is the type
                // of the struct.
                if let Some(field) = field {
                    let fat = FieldAccessTarget::new_field(true, &field, lu_dog);
                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    let expr = Expression::new_field_access(true, &expr, lu_dog);
                    let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    update_span_value(&span, &value, location!());

                    Ok(((expr, span), ty))
                } else if let Some(func) = func {
                    let fat = FieldAccessTarget::new_function(true, &func, lu_dog);
                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    let expr = Expression::new_field_access(true, &expr, lu_dog);
                    let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    update_span_value(&span, &value, location!());

                    Ok(((expr, span), ty))
                } else {
                    let span = s_read!(span);
                    let span = span.start as usize..span.end as usize;
                    Err(vec![DwarfError::StructFieldNotFound {
                        field: rhs.0.clone(),
                        file: context.file_name.to_owned(),
                        span,
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }])
                }
            } else {
                Err(vec![DwarfError::StructFieldNotFound {
                    field: rhs.0.clone(),
                    file: context.file_name.to_owned(),
                    span: rhs.1.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        }
        ValueTypeEnum::Ty(id) => {
            debug!("FieldAccess: ValueTypeEnum::Ty() {:?}", id);
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match *ty {
                Ty::Object(id) => {
                    // We get here for objects imported from a plug-in.
                    let woog_struct = lu_dog
                        .iter_woog_struct()
                        .inspect(|ref ws| {
                            debug!("{ws:?}");
                        })
                        .find(|ws| s_read!(ws).object == Some(id))
                        .unwrap();

                    if let Some(field) = lu_dog.exhume_field_id_by_name(&rhs.0) {
                        let field = lu_dog.exhume_field(&field);
                        let func = if let Some(impl_) =
                            s_read!(woog_struct).r8c_implementation_block(lu_dog).pop()
                        {
                            let funcs = s_read!(impl_).r9_function(lu_dog);
                            funcs.iter().find(|f| s_read!(f).name == rhs.0).cloned()
                        } else {
                            None
                        };

                        // We need to grab the type from the field: what we have above is the type
                        // of the struct.
                        if let Some(field) = field {
                            let fat = FieldAccessTarget::new_field(true, &field, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(true, &expr, lu_dog);
                            let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            update_span_value(&span, &value, location!());

                            Ok(((expr, span), ty))
                        } else if let Some(func) = func {
                            let fat = FieldAccessTarget::new_function(true, &func, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(true, &expr, lu_dog);
                            let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            update_span_value(&span, &value, location!());

                            Ok(((expr, span), ty))
                        } else {
                            let span = s_read!(span);
                            let span = span.start as usize..span.end as usize;
                            Err(vec![DwarfError::StructFieldNotFound {
                                field: rhs.0.clone(),
                                file: context.file_name.to_owned(),
                                span,
                                location: location!(),
                                program: context.source_string.to_owned(),
                            }])
                        }
                    } else {
                        Err(vec![DwarfError::StructFieldNotFound {
                            field: rhs.0.clone(),
                            file: context.file_name.to_owned(),
                            span: rhs.1.to_owned(),
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }])
                    }
                }
                _ => {
                    debug!("returning lhs");
                    Ok((lhs, lhs_ty))
                }
            }
        }
        ValueTypeEnum::ZObjectStore(ref id) => {
            let store = lu_dog.exhume_z_object_store(id).unwrap();
            let name = &s_read!(store).name;
            let id = lu_dog.exhume_woog_struct_id_by_name(name).unwrap();
            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
            let fields = s_read!(woog_struct).r7_field(lu_dog);
            let field = fields.iter().find(|f| s_read!(f).name == rhs.0);

            if let Some(field) = field {
                let field = lu_dog.exhume_field(&s_read!(field).id);
                let func = if let Some(impl_) =
                    s_read!(woog_struct).r8c_implementation_block(lu_dog).pop()
                {
                    let funcs = s_read!(impl_).r9_function(lu_dog);
                    funcs.iter().find(|f| s_read!(f).name == rhs.0).cloned()
                } else {
                    None
                };

                debug!("field {:?}", field);
                debug!("func {:?}", func);

                // We need to grab the type from the field: what we have above is the type
                // of the struct.
                if let Some(field) = field {
                    let fat = FieldAccessTarget::new_field(true, &field, lu_dog);
                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    let expr = Expression::new_field_access(true, &expr, lu_dog);
                    let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    update_span_value(&span, &value, location!());

                    Ok(((expr, span), ty))
                } else if let Some(func) = func {
                    let fat = FieldAccessTarget::new_function(true, &func, lu_dog);
                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    let expr = Expression::new_field_access(true, &expr, lu_dog);
                    let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    update_span_value(&span, &value, location!());

                    Ok(((expr, span), ty))
                } else {
                    let span = s_read!(span);
                    let span = span.start as usize..span.end as usize;
                    Err(vec![DwarfError::StructFieldNotFound {
                        field: rhs.0.clone(),
                        file: context.file_name.to_owned(),
                        span,
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }])
                }
            } else {
                Err(vec![DwarfError::StructFieldNotFound {
                    field: rhs.0.clone(),
                    file: context.file_name.to_owned(),
                    span: rhs.1.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        }
        what => {
            dbg!(&what);
            Err(vec![DwarfError::NotAStruct {
                file: context.file_name.to_owned(),
                span: rhs.1.to_owned(),
                ty: PrintableValueType(true, &ty, context, lu_dog).to_string(),
                program: context.source_string.to_owned(),
            }])
        }
    }
}
