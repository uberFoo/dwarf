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
        DwarfInteger, Expression as ParserExpression, Type,
    },
    keywords::{
        ARGS, ASSERT, ASSERT_EQ, CHACHA, COMPLEX_EX, EPS, FN_NEW, INTERVAL, NORM_SQUARED, ONE_SHOT,
        SLEEP, SPAWN, SPAWN_NAMED, TIME, TIMER, UUID_TYPE,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, DataStructure, EnumFieldEnum,
        Expression, FieldExpression, List, LocalVariable, PathElement, Span, StaticMethodCall,
        StructExpression, UnnamedFieldExpression, ValueType, ValueTypeEnum, Variable, XFuture,
        XPath, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType,
};

pub fn inter(
    path: &ParserExpression,
    method: &str,
    span: RefType<Span>,
    params: &[(ParserExpression, Range<usize>)],
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let save_path = &path;
    let path = if let ParserExpression::PathInExpression(path) = path {
        path
    } else {
        panic!(
            "I don't think that we should ever see anything other than a path here: {:?}",
            path
        );
    };

    // 🚧 This is not elegant. There's probably some uber-means of getting the
    // span and the string at once.
    let type_vec = path
        .iter()
        .map(|p| {
            if let Type::UserType((obj, span), _generics) = p {
                (obj.de_sanitize().to_owned(), span)
            } else {
                panic!(
                "I don't think that we should ever see anything other than a user type here: {:?}",
                path
            );
            }
        })
        .collect::<Vec<_>>();
    // This is the span over the entire path.
    let type_span = type_vec.first().unwrap().1.start..type_vec.last().unwrap().1.end;

    let type_name = type_vec
        .iter()
        .map(|p| p.0.clone())
        .collect::<Vec<_>>()
        .join("");

    // dbg!(&type_name);

    debug!("type_name {:?}", type_name);

    let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
    let mut elts = type_vec
        .iter()
        .inspect(|(name, _)| {
            debug!("name {:?}", name);
        })
        .map(|(name, _)| PathElement::new(name.to_owned(), None, &x_path, lu_dog))
        .collect::<Vec<RefType<PathElement>>>();
    elts.reverse();
    elts.push(PathElement::new(method.to_owned(), None, &x_path, lu_dog));

    debug!("path elements: {elts:?}");

    let first = elts
        .into_iter()
        .fold(Option::<RefType<PathElement>>::None, |prev, elt| {
            // dbg!(&prev, &elt);
            if let Some(prev) = prev {
                s_write!(prev).next = Some(s_read!(elt).id);
                // dbg!(&prev, &elt);
                Some(elt)
            } else {
                Some(elt)
            }
        });

    // dbg!(&first);

    if let Some(first) = first {
        let first = s_read!(first).id;
        s_write!(x_path).first = Some(first);
    }

    // We need to check if the type name is a struct or an enum.
    if lu_dog.exhume_woog_struct_id_by_name(&type_name).is_some()
        || lu_dog
            .exhume_z_object_store_id_by_name(&type_name)
            .is_some()
        || type_name == CHACHA
        || type_name == COMPLEX_EX
        || type_name == TIMER
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
        let call_expr = Expression::new_call(&call, lu_dog);

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
                ASSERT => {
                    let ty = Ty::new_boolean(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    ValueType::new_ty(&Ty::new_boolean(sarzak), lu_dog)
                }
                ASSERT_EQ => {
                    let ty = Ty::new_boolean(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    ValueType::new_ty(&Ty::new_boolean(sarzak), lu_dog)
                }
                COMPLEX_EX => {
                    let ty = Ty::new_float(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    ValueType::new_ty(&Ty::new_float(sarzak), lu_dog)
                }
                EPS => {
                    let ty = Ty::new_float(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    ValueType::new_ty(&Ty::new_float(sarzak), lu_dog)
                }
                SLEEP => ValueType::new_empty(lu_dog),
                #[cfg(feature = "async")]
                SPAWN => {
                    let inner = ValueType::new_empty(lu_dog);
                    let future = XFuture::new(&inner, lu_dog);
                    ValueType::new_x_future(&future, lu_dog)
                }
                #[cfg(feature = "async")]
                SPAWN_NAMED => {
                    let inner = ValueType::new_empty(lu_dog);
                    let future = XFuture::new(&inner, lu_dog);
                    ValueType::new_x_future(&future, lu_dog)
                }
                TIME => {
                    let ty = Ty::new_float(sarzak);
                    // 🚧 Ideally we'd cache this when we startup.
                    ValueType::new_ty(&Ty::new_float(sarzak), lu_dog)
                }
                _ => {
                    let span = s_read!(span).start as usize..s_read!(span).end as usize;
                    return Err(vec![DwarfError::ObjectNameNotFound {
                        name: type_name.to_owned(),
                        span,
                        location: location!(),
                    }]);
                    // e_warn!("ParserExpression type not found");
                    // ValueType::new_unknown(lu_dog)
                }
            }
        } else if type_name == TIMER {
            match method {
                #[cfg(feature = "async")]
                INTERVAL | ONE_SHOT => {
                    let inner = ValueType::new_empty(lu_dog);
                    let future = XFuture::new(&inner, lu_dog);
                    ValueType::new_x_future(&future, lu_dog)
                }
                _ => {
                    let span = s_read!(span).start as usize..s_read!(span).end as usize;
                    return Err(vec![DwarfError::ObjectNameNotFound {
                        name: type_name.to_owned(),
                        span,
                        location: location!(),
                    }]);
                    // e_warn!("ParserExpression type not found");
                    // ValueType::new_unknown(lu_dog)
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
                // Here I'm setting the pointer to the first argument.
                s_write!(call).argument = Some(s_read!(arg).id);
            }

            last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
        }

        let value = XValue::new_expression(block, &ty, &call_expr, lu_dog);
        update_span_value(&span, &value, location!());

        Ok(((call_expr, span), ty))
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
        //
        // Seems like I can just look at the type of the inner expression, and
        // use that to build a concrete type.
        //
        // Here we are interring an enum constructor.
        let type_name_no_generics = type_name.split('<').collect::<Vec<_>>()[0];
        if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(type_name_no_generics) {
            let woog_enum = lu_dog.exhume_enumeration(&woog_enum).unwrap();
            let foo = s_read!(woog_enum).r88_enum_field(lu_dog);
            let field = foo.iter().find(|field| {
                let field = s_read!(field);
                field.name == method
            });

            if let Some(field) = field {
                let subtype = {
                    let x = &s_read!(field).subtype;
                    x.clone()
                };
                let (woog_enum, expr) = match subtype {
                    EnumFieldEnum::TupleField(ref id) => {
                        let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();
                        let ty = s_read!(tuple_field).r86_value_type(lu_dog)[0].clone();
                        let span = &s_read!(ty).r62_span(lu_dog)[0];
                        let span = s_read!(span).start as usize..s_read!(span).end as usize;

                        // We only allow a single one. Stupid restriction. Wait for tuples.
                        // For each tuple element we will create a local variable
                        // in the block scope.
                        let param = &params[0];
                        if let ParserExpression::LocalVariable(name) = &param.0 {
                            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                            let var = Variable::new_local_variable(name.to_owned(), &local, lu_dog);
                            let value = XValue::new_variable(block, &ty, &var, lu_dog);
                            // let name = name.to_owned();
                            // let expr = VariableExpression::new(name, lu_dog);
                            // let expr = Expression::new_variable_expression(&expr, lu_dog);
                            // let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            Span::new(
                                param.1.end as i64,
                                param.1.start as i64,
                                &context.source,
                                None,
                                Some(&value),
                                lu_dog,
                            );
                        };

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
                        let foo = s_read!(ty);
                        if let ValueTypeEnum::Generic(_) = foo.subtype {
                            let type_name = path.iter().map(|p| {
                                if let Type::UserType((obj, _), generics) = p {
                                    let mut name = obj.de_sanitize().to_owned();
                                    let generics = generics.iter().map(|g| {
                                        g.0.to_string()
                                    }).collect::<Vec<_>>().join(", ");
                                    if !generics.is_empty() {
                                        name.push_str("<");
                                        name.push_str(&generics);
                                        name.push_str(">");
                                    }
                                    name
                                } else {
                                    panic!("I don't think that we should ever see anything other than a user type here: {:?}", p);
                                }
                            }).collect::<Vec<_>>().join("");

                            let (new_enum, _) = create_generic_enum(&type_name, save_path, lu_dog);
                            (new_enum, expr)
                        } else {
                            typecheck(
                                (&ty, &span),
                                (&expr_ty, &param.1),
                                location!(),
                                context,
                                lu_dog,
                            )?;

                            (woog_enum, expr)
                        }
                    }
                    _ => unreachable!(),
                };

                let woog_enum_id = s_read!(woog_enum).id;
                let ty = lu_dog
                    .iter_value_type()
                    .inspect(|ty| {
                        debug!("ty {:?}", ty);
                    })
                    .find(|ty| {
                        if let ValueTypeEnum::Enumeration(id) = s_read!(ty).subtype {
                            id == woog_enum_id
                        } else {
                            false
                        }
                    })
                    .unwrap();

                // let expr = Expression::new_enum_field(&field, lu_dog);

                let data_struct = DataStructure::new_enumeration(&woog_enum, lu_dog);
                let struct_expr =
                    StructExpression::new(Uuid::new_v4(), &data_struct, &x_path, lu_dog);
                let nfe = UnnamedFieldExpression::new(0, lu_dog);
                let strawberry = FieldExpression::new_unnamed_field_expression(
                    &expr.0,
                    &struct_expr,
                    &nfe,
                    lu_dog,
                );

                let expr = Expression::new_field_expression(&strawberry, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                Span::new(
                    s_read!(span).end,
                    s_read!(span).start,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                // update_span_value(&span, &value, location!());

                let expr = Expression::new_struct_expression(&struct_expr, lu_dog);
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
