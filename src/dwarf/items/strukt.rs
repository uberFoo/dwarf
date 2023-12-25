use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use sarzak::domain::DomainBuilder;
use snafu::{location, Location};

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            debug, function, make_value_type, Context, Span, StructFields, JSON_EXT, MODEL,
            MODEL_DIR, OBJECT, PLUGIN, PROXY, STORE, TYPE,
        },
        AttributeMap, InnerAttribute, Spanned, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Field, Generic, Item as WoogItem, Span as LuDogSpan,
        StructGeneric, ValueType, WoogStruct, XPlugin, ZObjectStore,
    },
    s_read, s_write, Desanitize, Dirty, RefType,
};

macro_rules! link_struct_generic {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_struct_generic(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

pub fn inter_struct(
    name: &str,
    _span: &Span,
    attributes: &AttributeMap,
    fields: &[(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: Option<&HashMap<String, Type>>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("struct {name}");

    // If there is a proxy attribute then we'll use it's info to attach an object
    // from the store to this UDT.
    if let Some(proxy_vec) = attributes.get(PROXY) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
            // Get the store value
            if let Some(store_vec) = attributes.get(STORE) {
                if let Some((_, ref value)) = store_vec.get(0) {
                    let store_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.store: {store_name}");

                    if let Some(name_vec) = attributes.get(OBJECT) {
                        if let Some((_, ref value)) = name_vec.get(0) {
                            let proxy_obj: String = value.try_into().map_err(|e| vec![e])?;
                            let proxy_obj = proxy_obj.desanitize();
                            // let proxy_obj = proxy_obj.to_upper_camel_case();
                            debug!("proxy.object: {proxy_obj}");
                            if let Some(model) = context.models.get(&store_name) {
                                if let Some(ref obj_id) =
                                    model.0.exhume_object_id_by_name(&proxy_obj)
                                {
                                    let obj = model.0.exhume_object(obj_id).unwrap();
                                    let woog_struct = WoogStruct::new(
                                        name.to_owned(),
                                        None,
                                        Some(&*obj.read().unwrap()),
                                        lu_dog,
                                    );
                                    context.dirty.push(Dirty::Struct(woog_struct.clone()));
                                    let _ = WoogItem::new_woog_struct(
                                        &context.source,
                                        &woog_struct,
                                        lu_dog,
                                    );
                                    let _ty = ValueType::new_woog_struct(&woog_struct, lu_dog);
                                    // 🚧 We may want to consider putting a span in the attribute map.
                                    // LuDogSpan::new(
                                    //     span.end as i64,
                                    //     span.start as i64,
                                    //     &context.source,
                                    //     Some(&ty),
                                    //     None,
                                    //     lu_dog,
                                    // );

                                    // We are pushing these onto a stack of fields so that we can typecheck
                                    // them after all of the structs have been interred.
                                    context.struct_fields.push(StructFields {
                                        woog_struct,
                                        fields: fields.to_owned(),
                                        generics: generics.cloned(),
                                        location: location!(),
                                    });

                                    debug!("found proxy object");

                                    Ok(())
                                } else {
                                    Err(vec![DwarfError::Generic {
                                        description: format!(
                                            "Object `{}` not found in store",
                                            proxy_obj
                                        ),
                                    }])
                                }
                            } else {
                                Err(vec![DwarfError::Generic {
                                    description: format!(
                                        "Model `{}` not found in store",
                                        store_name
                                    ),
                                }])
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        Err(vec![DwarfError::Generic {
                            description: "No object specified".to_owned(),
                        }])
                    }
                } else {
                    unreachable!();
                }
            } else if let Some(ty_vec) = attributes.get(TYPE) {
                if let Some((_, ref value)) = ty_vec.get(0) {
                    let type_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.ty: {type_name}");
                }
                Ok(())
            } else {
                Err(vec![DwarfError::Generic {
                    description: "No store specified".to_owned(),
                }])
            }
        } else {
            unreachable!();
        }

        // Below we are interring as an ObjectStore, according to it's annotation.
    } else if let Some(store_vec) = attributes.get(STORE) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = store_vec.get(0) {
            if let Some(model_vec) = attributes.get(MODEL) {
                if let Some((_, ref value)) = model_vec.get(0) {
                    let model_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("store.model: {model_name}");

                    // Load the model.
                    let mut path = context.cwd.clone();
                    path.pop();
                    path.push(MODEL_DIR);
                    path.push("this is annoying");
                    path.set_file_name(&model_name);
                    path.set_extension(JSON_EXT);

                    let domain = DomainBuilder::new()
                        .cuckoo_model(path)
                        .map_err(|e| {
                            vec![DwarfError::Generic {
                                description: e.to_string(),
                            }]
                        })?
                        .build_v2()
                        .map_err(|e| {
                            vec![DwarfError::Generic {
                                description: e.to_string(),
                            }]
                        })?;

                    context
                        .models
                        .insert(domain.name().to_owned(), (domain.sarzak().clone(), None));

                    // 🚧 Really should check to see if it's already there.
                    let store = ZObjectStore::new(model_name, name.to_owned(), lu_dog);
                    context.dirty.push(Dirty::Store(s_read!(store).id));
                    let _ = ValueType::new_z_object_store(&store, lu_dog);

                    Ok(())
                } else {
                    unreachable!();
                }
            } else {
                Err(vec![DwarfError::Generic {
                    description: "No model specified".to_owned(),
                }])
            }
        } else {
            unreachable!();
        }
    } else {
        // This is just a plain vanilla user defined type.
        let woog_struct = WoogStruct::new(name.to_owned(), None, None, lu_dog);
        context.dirty.push(Dirty::Struct(woog_struct.clone()));
        let _ = ValueType::new_woog_struct(&woog_struct, lu_dog);

        let mut first = true;
        let mut first_generic = None;
        let mut last_generic_uuid: Option<usize> = None;
        if let Some(generics) = generics {
            for generic in generics.keys() {
                let generic = StructGeneric::new(generic.to_owned(), None, &woog_struct, lu_dog);
                if first {
                    first = false;
                    first_generic = Some(s_read!(generic).id);
                }
                last_generic_uuid = link_struct_generic!(last_generic_uuid, generic, lu_dog);
            }

            s_write!(woog_struct).first_generic = first_generic;
        }

        context.struct_fields.push(StructFields {
            woog_struct,
            fields: fields.to_owned(),
            generics: generics.cloned(),
            location: location!(),
        });

        Ok(())
    }
}

pub fn inter_struct_fields(
    woog_struct: RefType<WoogStruct>,
    fields: &[(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: Option<&HashMap<String, Type>>,
    location: Location,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    let mut errors = Vec::new();
    for ((name, _), (type_, span), attrs) in fields {
        // let name = name.de_sanitize();

        debug!("field {name}");

        use std::ops::Range;
        let mut proxy_thang =
            |proxy_vec: &[(Range<usize>, InnerAttribute)]| -> Result<RefType<ValueType>> {
                if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
                    // Get the plugin value
                    if let Some(plugin_vec) = attributes.get(PLUGIN) {
                        if let Some((_, ref value)) = plugin_vec.get(0) {
                            let plugin_name: String = value.try_into().map_err(|e| vec![e])?;
                            debug!("proxy.plugin: {plugin_name}");
                            if let Type::UserType(tok, _generics) = type_ {
                                // let ty_name = tok.0.de_sanitize();
                                let ty_name = tok.0.to_owned();
                                if ty_name == "Plugin" {
                                    let plugin = XPlugin::new(plugin_name, lu_dog);
                                    let ty = ValueType::new_x_plugin(&plugin, lu_dog);
                                    LuDogSpan::new(
                                        span.end as i64,
                                        span.start as i64,
                                        &context.source,
                                        Some(&ty),
                                        None,
                                        lu_dog,
                                    );

                                    Ok(ty)
                                } else {
                                    Err(vec![DwarfError::Generic {
                                        description: format!(
                                            "Expected `Plugin`, found `{ty_name}`.",
                                        ),
                                    }])
                                }
                            } else {
                                Err(vec![DwarfError::Generic {
                                    description: format!("Expected `Plugin`, found `{type_}`.",),
                                }])
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        Err(vec![DwarfError::Generic {
                            description: "Expected `plugin` attribute".to_owned(),
                        }])
                    }
                } else {
                    unreachable!();
                }
            };

        let type_str = type_.to_string();
        let ty = if let Some(generics) = generics {
            if let Some(_definition_type) = generics.get(&type_str) {
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
            } else if let Some(proxy_vec) = attrs.get(PROXY) {
                proxy_thang(proxy_vec)?
            } else {
                context.location = location;
                match make_value_type(type_, span, None, context, lu_dog) {
                    Ok(ty) => ty,
                    Err(mut err) => {
                        errors.append(&mut err);
                        continue;
                    }
                }
            }
        } else if let Some(proxy_vec) = attrs.get(PROXY) {
            proxy_thang(proxy_vec)?
        } else {
            context.location = location;
            match make_value_type(type_, span, None, context, lu_dog) {
                Ok(ty) => ty,
                Err(mut err) => {
                    errors.append(&mut err);
                    continue;
                }
            }
        };

        let _field = Field::new(name.to_owned(), &woog_struct, &ty, lu_dog);
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}
