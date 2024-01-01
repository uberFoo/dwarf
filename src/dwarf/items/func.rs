use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            debug, function, inter_statements, make_value_type, typecheck, Context,
            FunctionDefinition, Span, FUNC, OBJECT, PROXY, STORE,
        },
        AttributeMap, BlockType, Expression as ParserExpression, InnerAttribute, Spanned,
        Statement as ParserStatement, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Body, ExternalImplementation, Function, Generic,
        ImplementationBlock, Item as WoogItem, LocalVariable, Parameter, Span as LuDogSpan,
        ValueType, Variable, XFuture, XValue,
    },
    new_ref, s_read, s_write, Dirty, DwarfInteger, NewRef, RefType,
};

macro_rules! link_parameter {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_parameter(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

#[allow(clippy::too_many_arguments)]
pub fn inter_func(
    a_sink: &BlockType,
    name: &str,
    attributes: &AttributeMap,
    params: &[(Spanned<String>, Spanned<Type>)],
    return_type: &Spanned<Type>,
    generics: Option<&HashMap<String, Type>>,
    stmts: Option<&Spanned<ParserExpression>>,
    impl_block: Option<&RefType<ImplementationBlock>>,
    impl_ty: Option<&RefType<ValueType>>,
    span: &Span,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_func {}", name);

    let a_sink = match a_sink {
        BlockType::Async => true,
        BlockType::Sync => false,
    };

    let external = if let Some(proxy_vec) = attributes.get(PROXY) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
            debug!("proxy");

            if let Some(store_vec) = attributes.get(STORE) {
                if let Some((_, ref value)) = store_vec.get(0) {
                    let store_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.store.: {store_name}");

                    if let Some(func_vec) = attributes.get(FUNC) {
                        if let Some((_, ref value)) = func_vec.get(0) {
                            let func_name: String = value.try_into().map_err(|e| vec![e])?;
                            debug!("proxy.func: {func_name}");

                            if let Some(obj_vec) = attributes.get(OBJECT) {
                                if let Some((_, ref value)) = obj_vec.get(0) {
                                    let obj_name: String = value.try_into().map_err(|e| vec![e])?;
                                    debug!("proxy.object: {obj_name}");

                                    let external = ExternalImplementation::new(
                                        func_name, store_name, obj_name, lu_dog,
                                    );
                                    Some(Body::new_external_implementation(
                                        a_sink, &external, lu_dog,
                                    ))
                                } else {
                                    unreachable!();
                                }
                            } else {
                                return Err(vec![DwarfError::Generic {
                                    description: "No object specified".to_owned(),
                                }]);
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        return Err(vec![DwarfError::Generic {
                            description: "No function specified".to_owned(),
                        }]);
                    }
                } else {
                    unreachable!();
                }
            } else {
                return Err(vec![DwarfError::Generic {
                    description: "No store specified".to_owned(),
                }]);
            }
        } else {
            unreachable!();
        }
    } else {
        None
    };

    let type_str = return_type.0.to_string();
    let ret_span = &return_type.1;
    let ret_ty = if let Some(generics) = generics {
        if generics.get(&type_str).is_some() {
            let g = Generic::new(type_str, None, None, lu_dog);
            let ty = ValueType::new_generic(&g, lu_dog);
            LuDogSpan::new(
                ret_span.end as i64,
                ret_span.start as i64,
                &context.source,
                Some(&ty),
                None,
                lu_dog,
            );

            ty
        } else {
            context.location = location!();
            make_value_type(
                &return_type.0,
                ret_span,
                impl_ty,
                context,
                context_stack,
                lu_dog,
            )?
        }
    } else {
        context.location = location!();
        make_value_type(
            &return_type.0,
            ret_span,
            impl_ty,
            context,
            context_stack,
            lu_dog,
        )?
    };

    let (func, block) =
        if let Some((ParserExpression::Block(block_a_sink, stmts, vars, tys), span)) = &stmts {
            let a_sink = a_sink
                || match block_a_sink {
                    BlockType::Async => true,
                    BlockType::Sync => false,
                };

            let block = Block::new(a_sink, Uuid::new_v4(), None, None, lu_dog);
            // Insert variables into the top of the block -- this is a for loop
            // thing.
            for (var, ty) in vars.iter().zip(tys.iter()) {
                let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                let var = Variable::new_local_variable(var.to_owned(), &local, lu_dog);
                let _value = XValue::new_variable(&block, &ty.0, &var, lu_dog);
                // 🚧 We should really be passing a span in the Block so that
                // we can link this XValue to it.
            }

            let body = Body::new_block(a_sink, &block, lu_dog);
            let func = Function::new(name.to_owned(), &body, None, impl_block, &ret_ty, lu_dog);
            context.dirty.push(Dirty::Func(func.clone()));
            let _ = ValueType::new_function(&func, lu_dog);

            (func, Some((block, stmts, span)))
        } else if let Some(body) = external {
            (
                Function::new(name.to_owned(), &body, None, impl_block, &ret_ty, lu_dog),
                None,
            )
        } else {
            return Err(vec![DwarfError::Generic {
                description: "No body specified".to_owned(),
            }]);
        };

    let _ = WoogItem::new_function(&context.source, &func, lu_dog);
    // Create a type for our function
    let ty = ValueType::new_function(&func, lu_dog);
    LuDogSpan::new(
        span.end as i64,
        span.start as i64,
        &context.source,
        Some(&ty),
        None,
        lu_dog,
    );

    // Check the parameters
    //
    let mut errors = Vec::new();
    let mut last_param_uuid: Option<usize> = None;

    for (position, ((param_name, name_span), (param_ty, ty_span))) in params.iter().enumerate() {
        debug!("param name {}", param_name);
        debug!("param ty {}", param_ty);

        // We need to introduce the values into the block, so that we don't
        // error out when parsing the statements.
        //
        let type_str = param_ty.to_string();
        let param_ty = if let Some(generics) = generics {
            if generics.get(&type_str).is_some() {
                let g = Generic::new(type_str, None, None, lu_dog);
                ValueType::new_generic(&g, lu_dog)
            } else {
                context.location = location!();
                match make_value_type(param_ty, ty_span, impl_ty, context, context_stack, lu_dog) {
                    Ok(ty) => ty,
                    Err(mut e) => {
                        errors.append(&mut e);
                        continue;
                    }
                }
            }
        } else {
            context.location = location!();
            match make_value_type(param_ty, ty_span, impl_ty, context, context_stack, lu_dog) {
                Ok(ty) => ty,
                Err(mut e) => {
                    errors.append(&mut e);
                    continue;
                }
            }
        };

        LuDogSpan::new(
            ty_span.end as i64,
            ty_span.start as i64,
            &context.source,
            Some(&param_ty),
            None,
            lu_dog,
        );

        debug!("param_ty {:?}", param_ty);

        let param = Parameter::new(position as DwarfInteger, &func, None, &param_ty, lu_dog);

        if position == 0 {
            s_write!(func).first_param = Some(s_read!(param).id);
        }

        debug!("param {:?}", param);
        debug!("param_ty {:?}", param_ty);

        let var = Variable::new_parameter(param_name.to_owned(), &param, lu_dog);
        debug!("var {:?}", var);

        if let Some((ref block, _, _)) = block {
            let value = XValue::new_variable(block, &param_ty, &var, lu_dog);
            LuDogSpan::new(
                name_span.end as i64,
                name_span.start as i64,
                &context.source,
                None,
                Some(&value),
                lu_dog,
            );
        }

        last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
    }

    // Note that we don't do anything if we didn't create a block, and that we
    // don't create a block if we don't have any statements.
    if let Some((block, stmts, stmt_span)) = block {
        let stmts: Vec<RefType<ParserStatement>> = stmts
            .iter()
            .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
            .collect();

        let (block_ty, block_span) =
            inter_statements(&stmts, stmt_span, &block, context, context_stack, lu_dog)?;

        let block_ty = match a_sink {
            true => {
                let future = XFuture::new(&block_ty, lu_dog);
                ValueType::new_x_future(&future, lu_dog)
            }
            false => block_ty,
        };

        typecheck(
            (&ret_ty, ret_span),
            (&block_ty, &block_span),
            location!(),
            context,
            lu_dog,
        )?;
    }

    debug!("func `{name}` saved");

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[allow(clippy::too_many_arguments)]
pub fn parse_func_signature(
    name: &str,
    params: &[(Spanned<String>, Spanned<Type>)],
    generics: Option<&HashMap<String, Type>>,
    return_type: &Spanned<Type>,
    impl_ty: Option<&RefType<ValueType>>,
    context: &mut Context,
    context_stack: &Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("parse_func_signature {}", name);

    let type_str = return_type.0.to_string();
    let span = &return_type.1;
    let ret_ty = if let Some(generics) = generics {
        if generics.get(&type_str).is_some() {
            let g = Generic::new(type_str, None, None, lu_dog);
            let ty = ValueType::new_generic(&g, lu_dog);
            LuDogSpan::new(
                span.end as i64,
                span.start as i64,
                &context.source,
                Some(&ty),
                None,
                lu_dog,
            );

            ty
        } else {
            context.location = location!();
            make_value_type(
                &return_type.0,
                span,
                impl_ty,
                context,
                context_stack,
                lu_dog,
            )?
        }
    } else {
        context.location = location!();
        make_value_type(
            &return_type.0,
            span,
            impl_ty,
            context,
            context_stack,
            lu_dog,
        )?
    };

    let mut param_tuples = Vec::new();
    for ((param_name, _), (param_ty, ty_span)) in params {
        let type_str = param_ty.to_string();
        let span = ty_span;
        let param_ty = if let Some(generics) = generics {
            if generics.get(&type_str).is_some() {
                let g = Generic::new(type_str, None, None, lu_dog);
                ValueType::new_generic(&g, lu_dog)
            } else {
                context.location = location!();
                make_value_type(param_ty, span, impl_ty, context, context_stack, lu_dog)?
            }
        } else {
            context.location = location!();
            make_value_type(param_ty, span, impl_ty, context, context_stack, lu_dog)?
        };

        LuDogSpan::new(
            span.end as i64,
            span.start as i64,
            &context.source,
            Some(&param_ty),
            None,
            lu_dog,
        );

        param_tuples.push((param_name.to_owned(), param_ty));
    }

    let defn = FunctionDefinition {
        name: name.to_owned(),
        params: param_tuples,
        return_type: ret_ty,
    };
    context.func_defs.insert(name.to_owned(), defn);

    Ok(())
}
