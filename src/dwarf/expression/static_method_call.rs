use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            create_generic_enum, debug, e_warn, function, inter_expression, link_argument,
            lookup_woog_struct_method_return_type, typecheck, update_span_value, Context,
            DeSanitize, ExprSpan,
        },
        DwarfInteger, Expression as ParserExpression, PrintableValueType, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, EnumFieldEnum, Expression, List,
        Span, StaticMethodCall, ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, ARGS, ASSERT_EQ, CHACHA, COMPLEX_EX, EPS, EVAL, FN_NEW, NORM_SQUARED, PARSE,
    TIME, UUID_TYPE,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
#[allow(clippy::borrowed_box)]
pub fn inter(
    path: &Box<ParserExpression>,
    method: &str,
    span: RefType<Span>,
    params: &[(ParserExpression, Range<usize>)],
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let path = if let ParserExpression::PathInExpression(path) = path.as_ref() {
        path
    } else {
        panic!(
            "I don't think that we should ever see anything other than a path here: {:?}",
            path
        );
    };

    // 🚧 This is not elegant. There's probably some uber-means of doing getting the
    // span and the string at once.
    let type_name = path
        .iter()
        .map(|p| {
            if let Type::UserType((obj, span)) = p {
                (obj.de_sanitize().to_owned(), span)
            } else {
                panic!(
                "I don't think that we should ever see anything other than a user type here: {:?}",
                path
            );
            }
        })
        .collect::<Vec<_>>();
    let type_span = type_name.first().unwrap().1.start..type_name.last().unwrap().1.end;
    let type_name = type_name
        .into_iter()
        .map(|p| p.0)
        .collect::<Vec<_>>()
        .join("");

    debug!("type_name {:?}", type_name);

    // dbg!(&lu_dog.iter_woog_struct().collect::<Vec<_>>());

    // We need to check if the type name is a struct or an enum.
    if lu_dog.exhume_woog_struct_id_by_name(&type_name).is_some()
        || lu_dog
            .exhume_z_object_store_id_by_name(&type_name)
            .is_some()
        || type_name == CHACHA
        || type_name == COMPLEX_EX
        || type_name == UUID_TYPE
    {
        // Here we are interring a static method call.
        let meth = StaticMethodCall::new(
            method.to_owned(),
            type_name.to_owned(),
            Uuid::new_v4(),
            lu_dog,
        );
        let call = Call::new_static_method_call(true, None, None, &meth, lu_dog);
        let expr = Expression::new_call(&call, lu_dog);

        debug!("name {}", type_name);
        debug!("method {}", method);

        let sarzak = context.sarzak;

        let ty = if type_name == CHACHA {
            match method {
                ARGS => {
                    let ty = Ty::new_s_string(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    let ty = lu_dog
                        .iter_value_type()
                        .find(|t| s_read!(t).subtype == ValueTypeEnum::Ty(ty.read().unwrap().id()))
                        .unwrap();
                    let list = List::new(&ty, lu_dog);
                    ValueType::new_list(&list, lu_dog)
                }
                _ => {
                    e_warn!("ParserExpression type not found");
                    ValueType::new_unknown(lu_dog)
                }
            }
        } else if type_name == UUID_TYPE && method == FN_NEW {
            ValueType::new_ty(&Ty::new_s_uuid(sarzak), lu_dog)
        } else if type_name == COMPLEX_EX {
            match method {
                NORM_SQUARED => ValueType::new_ty(&Ty::new_float(sarzak), lu_dog),
                _ => {
                    e_warn!("ParserExpression type not found");
                    ValueType::new_unknown(lu_dog)
                }
            }
        } else {
            debug!("ParserExpression::StaticMethodCall: looking up type {type_name}");
            lookup_woog_struct_method_return_type(&type_name, method, context.sarzak, lu_dog)
        };

        let mut last_arg_uuid: Option<usize> = None;
        for (position, param) in params.iter().enumerate() {
            let (arg_expr, _ty) = inter_expression(
                &new_ref!(ParserExpression, param.0.to_owned()),
                &param.1,
                block,
                context,
                lu_dog,
            )?;
            let arg = Argument::new(position as DwarfInteger, &arg_expr.0, &call, None, lu_dog);
            if position == 0 {
                s_write!(call).argument = Some(s_read!(arg).id);
            }

            last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
        }

        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
        update_span_value(&span, &value, location!());

        Ok(((expr, span), ty))
    } else {
        // enum A<T> {
        //     Some(T),
        //     None,
        // }
        //
        // These are both valid!
        // let a = A::Some::<i32>(42);
        // let b = A::<i32>::Some(42);

        // 🚧 As this stands it requires the user to use a fully qualified path, even though
        // it's not necessary, even without type solvers. For instance, `Option::Some(42)` is
        // clearly of type `Option<int>`, but we require `Option::<int>::Some(42)`.
        // I'm not sure how to fix this exactly.
        let type_name_no_generics = type_name.split('<').collect::<Vec<_>>()[0];
        if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(type_name_no_generics) {
            let woog_enum = lu_dog.exhume_enumeration(&woog_enum).unwrap();
            // Here we are interring an enum constructor.
            let fuzzy = s_read!(woog_enum).r88_enum_field(lu_dog);
            let field = fuzzy.iter().find(|field| {
                let field = s_read!(field);
                field.name == method
            });

            if let Some(field) = field {
                let subtype = {
                    let x = &s_read!(field).subtype;
                    x.clone()
                };
                let (woog_enum, field) = match subtype {
                    EnumFieldEnum::TupleField(ref id) => {
                        let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();
                        let ty = s_read!(tuple_field).r86_value_type(lu_dog)[0].clone();
                        let span = &s_read!(ty).r62_span(lu_dog)[0];
                        let span = s_read!(span).start as usize..s_read!(span).end as usize;

                        // We only allow a single one. Stupid restriction. Wait for tuples.
                        let param = &params[0];
                        let (expr, expr_ty) = inter_expression(
                            &new_ref!(ParserExpression, param.0.to_owned()),
                            &param.1,
                            block,
                            context,
                            lu_dog,
                        )?;

                        // If the type is `Generic` then we need to create a field with the
                        // type of the expression. We then attach the expression to the new
                        // field and continue.
                        let typhoid = s_read!(ty);
                        if let ValueTypeEnum::Generic(_) = typhoid.subtype {
                            let type_name = if !type_name.contains('<') {
                                let pvt = PrintableValueType(&expr_ty, context, lu_dog);
                                format!("{type_name}<{pvt}>")
                            } else {
                                type_name.to_owned()
                            };
                            let (new_enum, _) = create_generic_enum(&type_name, lu_dog);
                            let field = s_read!(new_enum)
                                .r88_enum_field(lu_dog)
                                .iter()
                                .find(|field| {
                                    let field = s_read!(field);
                                    if field.name == method {
                                        match field.subtype {
                                            EnumFieldEnum::TupleField(ref id) => {
                                                let tuple_field =
                                                    lu_dog.exhume_tuple_field(id).unwrap();
                                                let tuple_field = s_read!(tuple_field);
                                                tuple_field.expression.is_none()
                                            }
                                            _ => false,
                                        }
                                    } else {
                                        false
                                    }
                                })
                                .unwrap()
                                .clone();
                            match &s_read!(field).subtype {
                                EnumFieldEnum::TupleField(ref id) => {
                                    let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();

                                    s_write!(tuple_field).expression = Some(s_read!(expr.0).id);
                                    s_write!(tuple_field).ty = s_read!(expr_ty).id;
                                }
                                _ => unreachable!(),
                            }

                            (new_enum, field)
                        } else {
                            typecheck(
                                (&ty, &span),
                                (&expr_ty, &param.1),
                                location!(),
                                context,
                                lu_dog,
                            )?;

                            s_write!(tuple_field).expression = Some(s_read!(expr.0).id);
                            (woog_enum, field.clone())
                        }
                    }
                    _ => unreachable!(),
                };

                let woog_enum = s_read!(woog_enum).id;
                let ty = lu_dog
                    .iter_value_type()
                    .inspect(|ty| {
                        debug!("ty {:?}", ty);
                    })
                    .find(|ty| {
                        if let ValueTypeEnum::Enumeration(id) = s_read!(ty).subtype {
                            id == woog_enum
                        } else {
                            false
                        }
                    })
                    .unwrap();

                let expr = Expression::new_enum_field(&field, lu_dog);

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                Ok(((expr, span), ty))
            } else {
                let span = s_read!(span).start as usize..s_read!(span).end as usize;
                Err(vec![DwarfError::NoSuchField {
                    name: type_name.to_owned(),
                    name_span: type_span.to_owned(),
                    field: method.to_owned(),
                    span,
                }])
            }
        } else {
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            Err(vec![DwarfError::ObjectNameNotFound {
                name: type_name.to_owned(),
                span,
                location: location!(),
            }])
        }
    }
}
