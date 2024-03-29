use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{debug, function, make_value_type, Context},
        AttributeMap, EnumField, Spanned, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, EnumField as LuDogEnumField, EnumFieldEnum, EnumGeneric,
        Enumeration, Field, Span as LuDogSpan, StructField, TupleField, Unit, ValueType,
        WoogStruct,
    },
    s_read, s_write, Dirty, DwarfInteger, RefType, SarzakStorePtr, PATH_SEP,
};

macro_rules! link_enum_generic {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_enum_generic(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

pub fn inter_enum(
    name: &Spanned<String>,
    _attributes: &AttributeMap,
    variants: &[(Spanned<String>, Option<EnumField>)],
    enum_generics: Option<&HashMap<String, Type>>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    if let Some(path) = context.scopes.insert(name.0.clone(), context.path.clone()) {
        if path == context.path {
            return Ok(());
        }

        return Err(vec![DwarfError::MultiplyDefinedSymbol {
            name: name.0.clone(),
            span: name.1.clone(),
            path: context.path.clone(),
            orig_path: path,
            file: context.file_name.to_owned(),
            location: location!(),
        }]);
    }

    let name = context.path.clone() + &name.0;
    debug!("inter_enum {name}");

    let woog_enum = Enumeration::new(name.to_owned(), context.path.clone(), None, lu_dog);
    context.dirty.push(Dirty::Enum(woog_enum.clone()));
    let _ = ValueType::new_enumeration(true, &woog_enum, lu_dog);

    let mut local_generics = HashMap::default();
    let mut first = true;
    let mut first_generic = None;
    let mut last_generic_uuid: Option<SarzakStorePtr> = None;
    if let Some(generics) = enum_generics {
        for name in generics.keys() {
            let generic = EnumGeneric::new(name.to_owned(), &woog_enum, None, lu_dog);
            let ty = ValueType::new_enum_generic(true, &generic, lu_dog);
            local_generics.insert(name.to_owned(), ty.clone());
            //         LuDogSpan::new(
            //             span.end as i64,
            //             span.start as i64,
            //             &context.source,
            //             Some(&ty),
            //             None,
            //             lu_dog,
            //         );

            if first {
                first = false;
                first_generic = Some(s_read!(generic).id);
            }
            last_generic_uuid = link_enum_generic!(last_generic_uuid, generic, lu_dog);
        }

        s_write!(woog_enum).first_generic = first_generic;
    }

    for (number, ((field_name, span), field)) in variants.iter().enumerate() {
        match field {
            Some(EnumField::Struct(ref fields)) => {
                let name = if let Some(name) = name.strip_prefix(PATH_SEP) {
                    name
                } else {
                    &name
                };
                let struct_name = format!("{}{}{}", name, PATH_SEP, field_name);
                let struct_path = format!("::{}{}{}", name, PATH_SEP, field_name);

                if let Some(path) = context
                    .scopes
                    .insert(struct_name.clone(), struct_path.clone())
                {
                    return Err(vec![DwarfError::MultiplyDefinedSymbol {
                        name: struct_name.clone(),
                        span: span.clone(),
                        path: struct_name.clone(),
                        orig_path: path,
                        file: context.file_name.to_owned(),
                        location: location!(),
                    }]);
                }

                let woog_struct =
                    WoogStruct::new(struct_name.to_owned(), struct_path, None, None, lu_dog);
                context.dirty.push(Dirty::Struct(woog_struct.clone()));
                let ty = ValueType::new_woog_struct(true, &woog_struct, lu_dog);
                LuDogSpan::new(
                    span.end as i64,
                    span.start as i64,
                    &context.source,
                    Some(&ty),
                    None,
                    lu_dog,
                );

                for ((name, _), (ty, ty_span), _attrs) in fields {
                    context.location = location!();
                    let ty = make_value_type(ty, ty_span, None, context, context_stack, lu_dog)?;
                    let _ = Field::new(name.to_owned(), &woog_struct, &ty, lu_dog);
                }
                let field = StructField::new(field_name.to_owned(), lu_dog);
                LuDogEnumField::new_struct_field(field_name.to_owned(), &woog_enum, &field, lu_dog);
            }
            Some(EnumField::Tuple(type_)) => {
                let ty = match type_ {
                    (Type::UserType((ty, span), generics), _) if generics.is_empty() => {
                        // Pass the user type to the lookup business if this isn't a generic parameter.
                        if let Some(ty) = local_generics.get(ty) {
                            LuDogSpan::new(
                                span.end as i64,
                                span.start as i64,
                                &context.source,
                                Some(ty),
                                None,
                                lu_dog,
                            );
                            ty.clone()
                        } else {
                            context.location = location!();
                            let ty = make_value_type(
                                &type_.0,
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
                                Some(&ty),
                                None,
                                lu_dog,
                            );

                            ty
                        }
                    }
                    _ => {
                        context.location = location!();
                        let ty =
                            make_value_type(&type_.0, span, None, context, context_stack, lu_dog)?;
                        LuDogSpan::new(
                            span.end as i64,
                            span.start as i64,
                            &context.source,
                            Some(&ty),
                            None,
                            lu_dog,
                        );

                        ty
                    }
                };

                let field = TupleField::new(Uuid::new_v4(), &ty, lu_dog);
                LuDogEnumField::new_tuple_field(field_name.to_owned(), &woog_enum, &field, lu_dog);
            }
            _ => {
                let unit = Unit::new(number as DwarfInteger, lu_dog);
                let _ = LuDogEnumField::new_unit(field_name.to_owned(), &woog_enum, &unit, lu_dog);
            }
        }
    }

    Ok(())
}

use regex::Regex;

use crate::dwarf::extruder::RE;

pub(crate) fn create_generic_enum(
    enum_name: &str,
    base_enum: &str,
    lu_dog: &mut LuDogStore,
) -> Result<(RefType<Enumeration>, RefType<ValueType>)> {
    // Check to see if this already exists
    if let Some(id) = lu_dog.exhume_enumeration_id_by_name(enum_name) {
        let found_enum = lu_dog.exhume_enumeration(&id).unwrap();
        let ty = ValueType::new_enumeration(true, &found_enum, lu_dog);

        return Ok((found_enum, ty));
    }

    debug!("interring generic enum {enum_name}");

    let re = match RE.get() {
        Some(re) => re,
        None => {
            let re = Regex::new(r"^(::)?(\w+::)*\w+<(.*)>$").unwrap();
            match RE.set(re) {
                Ok(_) => {}
                Err(e) => {
                    panic!("Failed to set RE: {}", e);
                }
            }
            RE.get().unwrap()
        }
    };

    // The regex matches the generic type, and group three is the inner type.
    // One may iterate through all of them with a while let loop to get to the
    // innermost types.
    let captures = re.captures(enum_name).unwrap();
    let inner = &captures[3];
    let types = inner.split(',').map(|s| s.trim()).collect::<Vec<_>>();

    let mut path = base_enum.split(PATH_SEP).collect::<Vec<_>>();
    path.pop();
    let path = path.join(PATH_SEP) + PATH_SEP;

    let new_enum = Enumeration::new(enum_name.to_owned(), path.to_owned(), None, lu_dog);
    let ty = ValueType::new_enumeration(true, &new_enum, lu_dog);

    // We are cheating here. we are overloading the EnumGenerics type and relationship
    // to store the type's of the generics. As strings.
    let mut first = true;
    let mut first_generic = None;
    let mut last_generic_uuid: Option<SarzakStorePtr> = None;
    for ty in types {
        let generic = EnumGeneric::new(ty.to_owned(), &new_enum, None, lu_dog);
        let _ = ValueType::new_enum_generic(true, &generic, lu_dog);

        if first {
            first = false;
            first_generic = Some(s_read!(generic).id);
        }
        last_generic_uuid = link_enum_generic!(last_generic_uuid, generic, lu_dog);
    }
    s_write!(new_enum).first_generic = first_generic;

    // Down here we are copying the enumeration's fields from base to new.
    let Some(ref id) = lu_dog.exhume_enumeration_id_by_name(base_enum) else {
        panic!("enum not found");
    };

    let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
    for field in s_read!(woog_enum).r88_enum_field(lu_dog) {
        let field = s_read!(field);
        match field.subtype {
            EnumFieldEnum::Unit(ref id) => {
                let orig = lu_dog.exhume_unit(id).unwrap();
                let new = Unit::new(s_read!(orig).x_value, lu_dog);
                let _ = LuDogEnumField::new_unit(field.name.to_owned(), &new_enum, &new, lu_dog);
            }
            EnumFieldEnum::StructField(ref id) => {
                let orig = lu_dog.exhume_struct_field(id).unwrap();
                let new = StructField::new(s_read!(orig).name.to_owned(), lu_dog);
                let _ = LuDogEnumField::new_struct_field(
                    field.name.to_owned(),
                    &new_enum,
                    &new,
                    lu_dog,
                );
            }
            EnumFieldEnum::TupleField(ref id) => {
                // Note that we are borrowing whatever expression may exist on
                // the original, non-generic tuple field.
                let orig = lu_dog.exhume_tuple_field(id).unwrap();
                let new = TupleField::new(
                    Uuid::new_v4(),
                    &s_read!(orig).r86_value_type(lu_dog)[0],
                    lu_dog,
                );
                let _ =
                    LuDogEnumField::new_tuple_field(field.name.to_owned(), &new_enum, &new, lu_dog);
            }
        }
    }

    Ok((new_enum, ty))
}
