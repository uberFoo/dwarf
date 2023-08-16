use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            create_generic_enum, debug, function, inter_expression, link_argument,
            lookup_woog_struct_method_return_type, typecheck, Context, DeSanitize, ExprSpan,
        },
        DwarfInteger, Expression as ParserExpression, PrintableValueType, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, EnumField as LuDogEnumField,
        EnumFieldEnum, Enumeration, Expression, Plain, Span, StaticMethodCall, TupleField,
        ValueType, ValueTypeEnum, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType,
};

const UUID_TYPE: &str = "Uuid";
const FN_NEW: &str = "new";

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

    let (type_name, type_span) = if let Some(Type::UserType((obj, span))) = path.last() {
        (obj.de_sanitize().to_owned(), span)
    } else {
        panic!(
            "I don't think that we should ever see anything other than a user type here: {:?}",
            path
        );
    };

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
    // To fix this we'd need to

    debug!("type_name {:?}", type_name);
    let type_name_no_generics = type_name.split('<').collect::<Vec<_>>()[0];

    // dbg!(&type_name, &method);

    // We need to check if the type name is a struct or an enum.
    if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(&type_name_no_generics) {
        let woog_enum = lu_dog.exhume_enumeration(&woog_enum).unwrap();
        // dbg!(&woog_enum);
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
            // dbg!(&field);
            let field = match subtype {
                EnumFieldEnum::TupleField(ref id) => {
                    let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();
                    let ty = s_read!(tuple_field).r86_value_type(lu_dog)[0].clone();
                    dbg!(&lu_dog);
                    dbg!(&tuple_field, &ty);
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
                        let type_name = if !type_name.contains("<") {
                            let pvt = PrintableValueType(&expr_ty, context, lu_dog);
                            format!("{type_name}<{pvt}>")
                        } else {
                            type_name.to_owned()
                        };
                        let new_enum = create_generic_enum(&type_name, lu_dog);
                        dbg!(&type_name, &new_enum);
                        let new_enum = s_read!(new_enum);
                        let field = new_enum
                            .r88_enum_field(lu_dog)
                            .iter()
                            .inspect(|field| {
                                let field = s_read!(field);
                                dbg!(field);
                            })
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
                        match &(*s_read!(field)).subtype {
                            EnumFieldEnum::TupleField(ref id) => {
                                let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();
                                dbg!(&tuple_field);
                                s_write!(tuple_field).expression = Some(s_read!(expr.0).id);
                                s_write!(tuple_field).ty = s_read!(expr_ty).id;
                                dbg!(&tuple_field);
                            }
                            _ => unreachable!(),
                        }

                        field

                        // let woog_enum = Enumeration::new(name, None, lu_dog);
                        // let _ = ValueType::new_enumeration(&woog_enum, lu_dog);
                        // let new_field = TupleField::new(Some(&expr.0), &expr_ty, lu_dog);
                        // // dbg!(&new_field, &woog_enum);

                        // for field in &fuzzy {
                        //     let field = s_read!(field);
                        //     if field.name != method {
                        //         match field.subtype {
                        //             EnumFieldEnum::Plain(ref plain) => {
                        //                 let plain = lu_dog.exhume_plain(plain).unwrap();
                        //                 let new_plain = Plain::new(s_read!(plain).x_value, lu_dog);
                        //                 LuDogEnumField::new_plain(
                        //                     field.name.to_owned(),
                        //                     &woog_enum,
                        //                     &new_plain,
                        //                     lu_dog,
                        //                 );
                        //             }
                        //             _ => {}
                        //         };
                        //     }
                        // }

                        // LuDogEnumField::new_tuple_field(
                        //     s_read!(field).name.to_owned(),
                        //     &woog_enum,
                        //     &new_field,
                        //     lu_dog,
                        // )
                    } else {
                        if context.check_types {
                            typecheck(
                                (&ty, &span),
                                (&expr_ty, &param.1),
                                location!(),
                                context,
                                lu_dog,
                            )?;
                        }

                        s_write!(tuple_field).expression = Some(s_read!(expr.0).id);
                        field.clone()
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
            s_write!(span).x_value = Some(s_read!(value).id);

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

        // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
        // So we are down to this. I suppose that we can check the obj against
        // what's been entered thus far. Really this should be a second pass
        // then. For now, I'm going to hack something in...
        // We could do something with the imports...
        // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
        let ty = if type_name == UUID_TYPE && method == FN_NEW {
            ValueType::new_ty(&Ty::new_s_uuid(context.sarzak), lu_dog)
        } else {
            debug!("ParserExpression::StaticMethodCall: looking up type {type_name}");

            lookup_woog_struct_method_return_type(&type_name, method, lu_dog)

            // Look up the struct in the imported models.
            // I'll revisit this model business after I get the basics working.
            // for model in models {
            //     if let Some(obj) = model.exhume_object_id_by_name(&type_name) {
            //         let id = if let Some(s) = lu_dog
            //             .iter_woog_struct()
            //             .find(|s| s_read!(s).object == Some(obj))
            //         {
            //             s_read!(s).id
            //         } else {
            //             model.exhume_ty(&obj).unwrap().id()
            //         };

            //         ty = lu_dog.exhume_value_type(&id).unwrap().clone();
            //         break;
            //     }
            // }
            // ty
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
        s_write!(span).x_value = Some(s_read!(value).id);

        Ok(((expr, span), ty))
    }
}
