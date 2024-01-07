use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::Result,
        extruder::{debug, function, make_value_type, Context},
        AttributeMap, EnumField, Expression as ParserExpression, Spanned, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, EnumField as LuDogEnumField, EnumFieldEnum, EnumGeneric,
        Enumeration, Field, Generic, Span as LuDogSpan, StructField, TupleField, Unit, ValueType,
        WoogStruct,
    },
    s_read, s_write, Dirty, DwarfInteger, RefType, PATH_SEP,
};

macro_rules! link_generic {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_generic(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

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
    name: &str,
    _attributes: &AttributeMap,
    variants: &[(Spanned<String>, Option<EnumField>)],
    enum_generics: Option<&HashMap<String, Type>>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_enum {name}");

    let woog_enum = Enumeration::new(name.to_owned(), context.path.clone(), None, None, lu_dog);
    context.dirty.push(Dirty::Enum(woog_enum.clone()));
    let _ = ValueType::new_enumeration(&woog_enum, lu_dog);

    let mut first = true;
    let mut first_generic = None;
    let mut last_generic_uuid: Option<usize> = None;
    if let Some(generics) = enum_generics {
        for generic in generics.keys() {
            let generic = EnumGeneric::new(generic.to_owned(), &woog_enum, None, lu_dog);
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
                let mut type_path = context.path.clone();
                type_path += name;
                type_path += PATH_SEP;

                let woog_struct =
                    WoogStruct::new(field_name.to_owned(), type_path, None, None, lu_dog);
                context.dirty.push(Dirty::Struct(woog_struct.clone()));
                let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);
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
                    (Type::UserType(_, generics), _outer_span) if !generics.is_empty() => {
                        let mut first = true;
                        let mut first_generic = ValueType::new_empty(lu_dog);
                        let mut last_generic_uuid: Option<usize> = None;
                        for generic in generics {
                            let (generic, _span) =
                                if let Type::UserType((name, span), _) = &generic.0 {
                                    (name, span)
                                } else {
                                    unreachable!();
                                };
                            let generic = Generic::new(generic.to_owned(), None, None, lu_dog);
                            let ty = ValueType::new_generic(&generic, lu_dog);
                            // let span = LuDogSpan::new(
                            //     span.end as i64,
                            //     span.start as i64,
                            //     &context.source,
                            //     Some(&ty),
                            //     None,
                            //     lu_dog,
                            // );
                            // update_span_value(&span, &ty, location!());

                            if first {
                                first = false;
                                first_generic = ty.clone()
                            }
                            last_generic_uuid = link_generic!(last_generic_uuid, generic, lu_dog);
                        }

                        first_generic
                    }
                    (Type::UserType((ty, span), generics), _) if generics.is_empty() => {
                        // Pass the user type to the lookup business if this isn't a generic parameter.
                        if let Some(generics) = enum_generics {
                            if let Some(generic) = generics.get(ty) {
                                context.location = location!();
                                let ty = make_value_type(
                                    generic,
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

// Note that the name is expected to contain the generic component.
pub(crate) fn create_generic_enum(
    enum_name: &str,
    enum_path: &ParserExpression,
    context: &mut Context,
    context_stack: &mut [(String, RefType<LuDogStore>)],
    lu_dog: &mut LuDogStore,
) -> (RefType<Enumeration>, RefType<ValueType>) {
    // Check to see if this already exists
    if let Some(id) = lu_dog.exhume_enumeration_id_by_name(enum_name) {
        let found_enum = lu_dog.exhume_enumeration(&id).unwrap();
        let ty = ValueType::new_enumeration(&found_enum, lu_dog);

        return (found_enum, ty);
    }

    let base_enum_impl = if let ParserExpression::PathInExpression(path) = enum_path {
        if let Type::UserType(enum_name, _) = &path[0].0 {
            let id = if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(&enum_name.0) {
                woog_enum
            } else {
                let mut iter = context_stack.iter();
                loop {
                    if let Some((path, lu_dog)) = iter.next() {
                        dbg!(&path, &enum_name.0);
                        if let Some(woog_enum) =
                            s_read!(lu_dog).exhume_enumeration_id_by_name(&enum_name.0)
                        {
                            dbg!("found it");
                            break woog_enum;
                        }
                    } else {
                        dbg!("not found");
                        unreachable!();
                    }
                }
            };

            let base_enum = lu_dog.exhume_enumeration(&id).unwrap();
            let impl_block_vec = s_read!(base_enum).r84_implementation_block(lu_dog);
            if !impl_block_vec.is_empty() {
                Some(impl_block_vec[0].clone())
            } else {
                None
            }
        } else {
            unreachable!();
        }
    } else {
        unreachable!();
    };

    debug!("interring generic enum {enum_name}");

    let new_enum = Enumeration::new(
        enum_name.to_owned(),
        context.path.clone(),
        None,
        base_enum_impl.as_ref(),
        lu_dog,
    );
    let ty = ValueType::new_enumeration(&new_enum, lu_dog);

    let name_without_generics = enum_name.split('<').collect::<Vec<_>>()[0];

    debug!("name_without_generics {:?}", name_without_generics);
    let id = if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(name_without_generics) {
        woog_enum
    } else {
        let mut iter = context_stack.iter();
        loop {
            if let Some((path, lu_dog)) = iter.next() {
                dbg!(&path, name_without_generics);
                if let Some(woog_enum) =
                    s_read!(lu_dog).exhume_enumeration_id_by_name(name_without_generics)
                {
                    dbg!("found it");
                    break woog_enum;
                }
            } else {
                dbg!("not found");
                unreachable!();
            }
        }
    };
    let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
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

    (new_enum, ty)
}
