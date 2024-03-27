use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};
use uuid::Uuid;

#[cfg(feature = "async")]
use crate::{
    keywords::{ASLEEP, INTERVAL, ONE_SHOT, SPAWN, SPAWN_NAMED},
    lu_dog::XFuture,
};

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            debug, e_warn, expression::method_call::method_call_return_type, function,
            inter_expression, link_argument, lookup_woog_struct_method_return_type,
            make_value_type, typecheck, update_span_value, Context, ExprSpan,
        },
        items::enuum::create_generic_enum,
        DwarfInteger, Expression as ParserExpression, Generics, Type,
    },
    keywords::{
        ARGS, ASSERT, ASSERT_EQ, CHACHA, COMPLEX_EX, EPS, EVAL, FN_NEW, FQ_UUID_TYPE, LOAD_PLUGIN,
        NEW, NORM_SQUARED, PARSE, PLUGIN, SLEEP, TIME, TIMER, TYPEOF, UUID_TYPE,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Argument, Block, Call, DataStructure, EnumField,
        EnumFieldEnum, Enumeration, Expression, FieldExpression, List, LocalVariable, MethodCall,
        PathElement, Span as LuDogSpan, Span, StaticMethodCall, StructExpression,
        UnnamedFieldExpression, ValueType, ValueTypeEnum, Variable, XPath, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, PATH_SEP,
};

#[allow(clippy::too_many_arguments)]
pub fn inter(
    path: &ParserExpression,
    method: &str,
    span: RefType<Span>,
    params: &[(ParserExpression, Range<usize>)],
    block: &RefType<Block>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let ParserExpression::PathInExpression(path) = path else {
        panic!(
            "I don't think that we should ever see anything other than a path here: {:?}",
            path
        );
    };

    let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);

    let mut recursion = |path: &Vec<(Type, Option<(Vec<(Type, Range<usize>)>, Range<usize>)>)>,
                         x_path: RefType<XPath>|
     -> (String, Range<usize>, Vec<RefType<PathElement>>) {
        let mut span = 0..0;
        let mut pes = Vec::new();
        let path = path.iter().map(|p| {
                if let Type::UserType((obj, ut_span), generics) = &p.0 {
                    let mut name = obj.to_owned();
                    let pe = PathElement::new(obj.to_owned(), None, &x_path, lu_dog);
                    pes.push(pe);
                    span.start = ut_span.start;
                    let generics = generics.iter().map(|g| {
                        span.end = g.1.end;
                        g.0.to_string()
                    }).collect::<Vec<_>>().join(", ");
                    if !generics.is_empty() {
                        name.push('<');
                        name.push_str(&generics);
                        name.push('>');
                    }
                    name
                } else {
                    panic!("I don't think that we should ever see anything other than a user type here: {:?}", p);
                }
            }).collect::<Vec<_>>().join("::");

        (path, span, pes)
    };

    let (type_name, type_span, mut elts) = recursion(path, x_path.clone());

    let base_name = if let Some(name) = type_name.split('<').next() {
        name
    } else {
        &type_name
    };
    let type_name = if let Some(path) = context.scopes.get(base_name) {
        path.to_owned() + type_name.as_str()
    } else {
        context.path.clone() + type_name.as_str()
    };

    elts.push(PathElement::new(method.to_owned(), None, &x_path, lu_dog));

    debug!("path elements: {elts:?}");

    if let Some(first) = elts.first() {
        debug!("first {first:?}");
        let first = s_read!(first).id;
        s_write!(x_path).first = Some(first);
    }

    // Stitch together the pointers.
    elts.into_iter()
        .fold(Option::<RefType<PathElement>>::None, |prev, elt| {
            if let Some(prev) = prev {
                let id = {
                    let e = s_read!(elt);
                    e.id
                };
                s_write!(prev).next = Some(id);
                Some(elt)
            } else {
                Some(elt)
            }
        });

    debug!("type_name {type_name}");
    debug!("method {method}");

    let generics = path
        .iter()
        .flat_map(|p| {
            if let (Type::UserType((_, _), generics), _) = p {
                generics
            } else {
                panic!(
                "I don't think that we should ever see anything other than a user type here: {path:?}",
            );
            }
        })
        .collect::<Vec<_>>();

    // 🚧 This smells bad.
    // We need to check if the type name is a struct or an enum.
    if lu_dog.exhume_woog_struct_id_by_name(&type_name).is_some()
        || lu_dog
            .exhume_z_object_store_id_by_name(&type_name)
            .is_some()
        || type_name == CHACHA
        || type_name == COMPLEX_EX
        || type_name == TIMER
        || type_name == UUID_TYPE
        || type_name == FQ_UUID_TYPE
    {
        // Here we are interring a static method call.
        let meth =
            StaticMethodCall::new(method.to_owned(), type_name.clone(), Uuid::new_v4(), lu_dog);
        let call = Call::new_static_method_call(true, None, None, &meth, lu_dog);
        let call_expr = Expression::new_call(true, &call, lu_dog);

        let sarzak = context.sarzak;

        // Process the args.
        let mut arg_types = Vec::new();
        let mut last_arg_uuid: Option<SarzakStorePtr> = None;
        for (position, param) in params.iter().enumerate() {
            let (arg_expr, ty) = inter_expression(
                &new_ref!(ParserExpression, param.0.to_owned()),
                &param.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;
            arg_types.push(ty);

            let arg = Argument::new(position as DwarfInteger, &arg_expr.0, &call, None, lu_dog);
            if position == 0 {
                // Here I'm setting the pointer to the first argument.
                s_write!(call).argument = Some(s_read!(arg).id);
            }

            last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
        }

        // Get the type from the method.
        let ty = match type_name.as_str() {
            CHACHA => {
                match method {
                    ARGS => {
                        let ty = Ty::new_z_string(sarzak);
                        // 🚧 Ideally we'd cache this when we startup.
                        let ty = lu_dog
                            .iter_value_type()
                            .find(|t| {
                                s_read!(t).subtype == ValueTypeEnum::Ty(ty.read().unwrap().id())
                            })
                            .unwrap();
                        let list = List::new(&ty, lu_dog);
                        ValueType::new_list(true, &list, lu_dog)
                    }
                    #[cfg(feature = "async")]
                    ASLEEP => {
                        let inner = ValueType::new_empty(true, lu_dog);
                        let future = XFuture::new(&inner, lu_dog);
                        ValueType::new_x_future(true, &future, lu_dog)
                    }
                    ASSERT => ValueType::new_ty(true, &Ty::new_boolean(sarzak), lu_dog),
                    ASSERT_EQ => ValueType::new_ty(true, &Ty::new_boolean(sarzak), lu_dog),
                    EPS => ValueType::new_ty(true, &Ty::new_float(sarzak), lu_dog),
                    EVAL => ValueType::new_unknown(true, lu_dog),
                    LOAD_PLUGIN => ValueType::new_unknown(true, lu_dog),
                    PARSE => ValueType::new_unknown(true, lu_dog),
                    SLEEP => ValueType::new_empty(true, lu_dog),
                    #[cfg(feature = "async")]
                    SPAWN => {
                        let inner = arg_types[0].clone();
                        let future = XFuture::new(&inner, lu_dog);
                        ValueType::new_x_future(true, &future, lu_dog)
                    }
                    #[cfg(feature = "async")]
                    SPAWN_NAMED => {
                        let inner = arg_types[0].clone();
                        let future = XFuture::new(&inner, lu_dog);
                        ValueType::new_x_future(true, &future, lu_dog)
                    }
                    TIME => ValueType::new_ty(true, &Ty::new_float(sarzak), lu_dog),
                    TYPEOF => ValueType::new_ty(true, &Ty::new_z_string(sarzak), lu_dog),
                    method => {
                        let span = s_read!(span).start as usize..s_read!(span).end as usize;
                        return Err(vec![DwarfError::NoSuchMethod {
                            method: method.to_owned(),
                            file: context.file_name.to_owned(),
                            span,
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }]);
                        // e_warn!("ParserExpression type not found");
                        // ValueType::new_unknown(lu_dog)
                    }
                }
            }
            COMPLEX_EX => match method {
                NORM_SQUARED => ValueType::new_ty(true, &Ty::new_float(sarzak), lu_dog),
                method => {
                    e_warn!("ComplexEx method `{method}` not found");
                    ValueType::new_unknown(true, lu_dog)
                }
            },
            TIMER => {
                match method {
                    #[cfg(feature = "async")]
                    INTERVAL | ONE_SHOT => {
                        let inner = ValueType::new_empty(true, lu_dog);
                        let future = XFuture::new(&inner, lu_dog);
                        ValueType::new_x_future(true, &future, lu_dog)
                    }
                    _ => {
                        let span = s_read!(span).start as usize..s_read!(span).end as usize;
                        return Err(vec![DwarfError::ObjectNameNotFound {
                            name: type_name.to_owned(),
                            file: context.file_name.to_owned(),
                            span,
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }]);
                        // e_warn!("ParserExpression type not found");
                        // ValueType::new_unknown(lu_dog)
                    }
                }
            }
            UUID_TYPE | FQ_UUID_TYPE if method == FN_NEW => {
                ValueType::new_ty(true, &Ty::new_z_uuid(sarzak), lu_dog)
            }
            _ => {
                debug!("ParserExpression::StaticMethodCall: looking up type {type_name}");
                lookup_woog_struct_method_return_type(&type_name, method, context.sarzak, lu_dog)
            }
        };

        let value = XValue::new_expression(block, &ty, &call_expr, lu_dog);
        update_span_value(&span, &value, location!());

        Ok(((call_expr, span), ty))
    } else if {
        if let Some(prefix) = type_name.split('<').next() {
            if let Some(plugin) = prefix.split(PATH_SEP).last() {
                if plugin == PLUGIN {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    } {
        let meth =
            StaticMethodCall::new(method.to_owned(), type_name.clone(), Uuid::new_v4(), lu_dog);
        let call = Call::new_static_method_call(true, None, None, &meth, lu_dog);
        let call_expr = Expression::new_call(true, &call, lu_dog);

        // Process the args.
        let mut arg_types = Vec::new();
        let mut last_arg_uuid: Option<SarzakStorePtr> = None;
        for (position, param) in params.iter().enumerate() {
            let (arg_expr, ty) = inter_expression(
                &new_ref!(ParserExpression, param.0.to_owned()),
                &param.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;
            arg_types.push(ty);

            let arg = Argument::new(position as DwarfInteger, &arg_expr.0, &call, None, lu_dog);
            if position == 0 {
                // Here I'm setting the pointer to the first argument.
                s_write!(call).argument = Some(s_read!(arg).id);
            }

            last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
        }

        let ty = match method {
            NEW => {
                let plugin_type = &generics.first().unwrap().0;
                let Type::Generic((plugin_type, _)) = plugin_type else {
                    panic!(
            "I don't think that we should ever see anything other than a user type here: {generics:?}",
        );
                };

                let plugin = lu_dog.exhume_x_plugin_id_by_name(plugin_type).unwrap();
                let plugin = lu_dog.exhume_x_plugin(&plugin).unwrap();
                ValueType::new_x_plugin(true, &plugin, lu_dog)
            }
            method => {
                e_warn!("Plugin method `{method}` not found");
                ValueType::new_unknown(true, lu_dog)
            }
        };

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
        let no_generics = type_name.split('<').next().unwrap();
        let woog_enum = if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(&type_name) {
            lu_dog.exhume_enumeration(&woog_enum).unwrap()
        } else if lu_dog.exhume_enumeration_id_by_name(no_generics).is_some() {
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            create_generic_enum(&type_name, no_generics, lu_dog)?.0
        } else {
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            return Err(vec![DwarfError::ObjectNameNotFound {
                name: type_name.strip_prefix(PATH_SEP).unwrap().to_owned(),
                file: context.file_name.to_owned(),
                span,
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        };

        let foo = s_read!(woog_enum).r88_enum_field(lu_dog);
        let field = foo.iter().find(|field| {
            let field = s_read!(field);
            field.name == method
        });
        if let Some(field) = field {
            inter_field(
                field,
                params,
                block,
                path,
                woog_enum,
                &span,
                &x_path,
                context,
                context_stack,
                lu_dog,
            )
        } else {
            // Lookup the functions on the enum and see if the field matches a func.
            let foo = &s_read!(woog_enum).r84c_implementation_block(lu_dog)[0];
            let foo = s_read!(foo).r9_function(lu_dog);
            let func = foo.iter().find(|func| {
                let func = s_read!(func);
                func.name == method
            });

            if let Some(func) = func {
                let func_ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                let meth = MethodCall::new(method.to_owned(), lu_dog);
                let call = Call::new_method_call(true, None, None, &meth, lu_dog);
                let expr = Expression::new_call(true, &call, lu_dog);

                // Call the function with it's args.
                // Process the args.
                let mut arg_types = Vec::new();
                let mut last_arg_uuid: Option<SarzakStorePtr> = None;
                for (position, param) in params.iter().enumerate() {
                    let (arg_expr, ty) = inter_expression(
                        &new_ref!(ParserExpression, param.0.to_owned()),
                        &param.1,
                        block,
                        context,
                        context_stack,
                        lu_dog,
                    )?;
                    arg_types.push(ty);

                    let arg =
                        Argument::new(position as DwarfInteger, &arg_expr.0, &call, None, lu_dog);
                    if position == 0 {
                        // Here I'm setting the pointer to the first argument.
                        s_write!(call).argument = Some(s_read!(arg).id);
                        s_write!(call).expression = Some(s_read!(arg_expr.0).id);
                    }

                    last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
                }

                let r_span = s_read!(span).start as usize..s_read!(span).end as usize;
                let ret_ty = method_call_return_type(
                    func_ty,
                    &method.to_owned(),
                    r_span,
                    &mut arg_types,
                    context,
                    lu_dog,
                )?;

                let value = XValue::new_expression(block, &ret_ty, &expr, lu_dog);
                Span::new(
                    s_read!(span).end,
                    s_read!(span).start,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );

                Ok(((expr, span), ret_ty))
            } else {
                let span = s_read!(span).start as usize..s_read!(span).end as usize;
                Err(vec![DwarfError::NoSuchField {
                    name: type_name.to_owned(),
                    name_span: type_span.to_owned(),
                    field: method.to_owned(),
                    file: context.file_name.to_owned(),
                    span,
                    location: location!(),
                    program: context.source_string.to_owned(),
                }])
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn inter_field(
    field: &RefType<EnumField>,
    params: &[(ParserExpression, Range<usize>)],
    block: &RefType<Block>,
    path: &[(Type, Option<Generics>)],
    woog_enum: RefType<Enumeration>,
    span: &RefType<Span>,
    x_path: &RefType<XPath>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let subtype = &s_read!(field).subtype.clone();
    let (woog_enum, expr) = match subtype {
        EnumFieldEnum::TupleField(ref id) => {
            let tuple_field = lu_dog.exhume_tuple_field(id).unwrap();
            let ty = s_read!(tuple_field).r86_value_type(lu_dog)[0].clone();

            let ty = match s_read!(ty).subtype {
                ValueTypeEnum::EnumGeneric(ref id) => {
                    // The type is generic. This means that we need to create a
                    // type substituting the generic for a concrete type that
                    // must be present in the path.
                    //
                    // This is annoying because we just have the generic and not
                    // any simple way to correlate it with the path element we
                    // need. My best idea is to iterate over the generics for the
                    // base type and find the ordinal of the generic we have.
                    // We can then iterate over the path and find the correct type
                    // to substitute.
                    //
                    // This is extra annoying because we don't have the means to
                    // get to the first generic. So this is busted until I do
                    // something about that.
                    let generic = lu_dog.exhume_enum_generic(id).unwrap();
                    let woog_enum = s_read!(generic).r104_enumeration(lu_dog)[0].clone();

                    if s_read!(woog_enum).name != context.in_impl {
                        let mut ordinal = 0;
                        let mut iter = s_read!(woog_enum).r105_enum_generic(lu_dog)[0].clone();
                        loop {
                            if &*s_read!(iter) == &*s_read!(generic) {
                                break;
                            }
                            ordinal += 1;
                            if s_read!(iter).next.is_none() {
                                break;
                            }
                            let id = s_read!(iter).next.unwrap();
                            iter = lu_dog.exhume_enum_generic(&id).unwrap();
                        }

                        let (Type::UserType(_, generics), _) = &path[0] else {
                            return Err(vec![DwarfError::Internal {
                                description: "Expected generics".to_owned(),
                                location: location!(),
                            }]);
                        };

                        if !generics.is_empty() {
                            let target = &generics[ordinal];
                            let span = &target.1;
                            let target = match &target.0 {
                                Type::Generic((target, _)) => {
                                    Type::UserType((target.to_owned(), span.clone()), vec![])
                                }
                                t => t.clone(),
                            };
                            let target = make_value_type(
                                &target,
                                span,
                                None,
                                context,
                                context_stack,
                                lu_dog,
                            )?;
                            LuDogSpan::new(
                                span.end as i64,
                                span.start as i64,
                                &context.source,
                                Some(&target),
                                None,
                                lu_dog,
                            );

                            target
                        } else {
                            ty.clone()
                        }
                    } else {
                        ty.clone()
                    }
                }
                _ => ty.clone(),
            };

            let span = &s_read!(ty).r62_span(lu_dog)[0];
            let span = s_read!(span).start as usize..s_read!(span).end as usize;

            // For each tuple element we will create a local variable
            // in the block scope.
            // We only allow a single one. Stupid restriction. Wait for tuples.
            let param = &params[0];
            if let ParserExpression::LocalVariable(name) = &param.0 {
                let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                let var = Variable::new_local_variable(name.to_owned(), &local, lu_dog);
                let value = XValue::new_variable(block, &ty, &var, lu_dog);
                Span::new(
                    param.1.end as i64,
                    param.1.start as i64,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                debug!("local {name} {ty:?} {value:?}");
            };

            let (expr, expr_ty) = inter_expression(
                &new_ref!(ParserExpression, param.0.to_owned()),
                &param.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            // If the type is `Generic` then we need to create a field with the
            // type of the expression. We then attach the expression to the new
            // field and continue.
            let foo = s_read!(ty);
            if let ValueTypeEnum::EnumGeneric(_) = foo.subtype {
                // Context has an in_impl flag so that I can tell if we should be
                // creating a new type, or using the existing one that happens to
                // be the archetype.
                let Type::UserType((base_name, _), _) = &path[0].0 else {
                    panic!("I don't think that we should ever see anything other than a user type here: {:?}", path[0]);
                };

                if &context.in_impl == base_name {
                    let id = lu_dog.exhume_enumeration_id_by_name(&base_name).unwrap();
                    (lu_dog.exhume_enumeration(&id).unwrap(), expr)
                } else {
                    let base_path = if let Some(path) = context.scopes.get(base_name) {
                        path
                    } else {
                        &context.path
                    };

                    let type_name = base_path.to_owned() + path.iter().map(|p| {
                        if let Type::UserType((obj, _), generics) = &p.0 {
                            let mut name = obj.to_owned();
                            let generics = generics.iter().map(|g| {
                                g.0.to_string()
                            }).collect::<Vec<_>>().join(", ");
                            if !generics.is_empty() {
                                name.push('<');
                                name.push_str(&generics);
                                name.push('>');
                            }
                            name
                        } else {
                            panic!("I don't think that we should ever see anything other than a user type here: {:?}", p);
                        }
                    }).collect::<Vec<_>>().join("").as_str();

                    let base_name = base_path.clone() + base_name;

                    let (new_enum, _) = create_generic_enum(&type_name, &base_name, lu_dog)?;

                    (new_enum, expr)
                }
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
        a => unreachable!("{a:?}"),
    };

    let woog_enum_id = s_read!(woog_enum).id;
    let ty = lu_dog
        .iter_value_type()
        .find(|ty| {
            if let ValueTypeEnum::Enumeration(id) = s_read!(ty).subtype {
                id == woog_enum_id
            } else {
                false
            }
        })
        .unwrap();

    // let expr = Expression::new_enum_field(&field, lu_dog);

    let data_struct = DataStructure::new_enumeration(true, &woog_enum, lu_dog);
    let struct_expr = StructExpression::new(Uuid::new_v4(), &data_struct, x_path, lu_dog);
    let nfe = UnnamedFieldExpression::new(0, lu_dog);
    let strawberry =
        FieldExpression::new_unnamed_field_expression(&expr.0, &struct_expr, &nfe, lu_dog);

    let expr = Expression::new_field_expression(true, &strawberry, lu_dog);
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

    let expr = Expression::new_struct_expression(true, &struct_expr, lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(span, &value, location!());

    Ok(((expr, span.clone()), ty))
}
