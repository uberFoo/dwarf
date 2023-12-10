use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::Result,
        extruder::{debug, function, make_value_type, Context},
        AttributeMap, EnumField, Spanned, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, EnumField as LuDogEnumField, Enumeration, Field, Generic,
        Span as LuDogSpan, StructField, TupleField, Unit, ValueType, WoogStruct,
    },
    s_read, s_write, Dirty, DwarfInteger,
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

pub fn inter_enum(
    name: &str,
    _attributes: &AttributeMap,
    variants: &[(Spanned<String>, Option<EnumField>)],
    enum_generics: Option<&HashMap<String, Type>>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_enum {name}");

    let woog_enum = Enumeration::new(name.to_owned(), None, lu_dog);
    context.dirty.push(Dirty::Enum(woog_enum.clone()));
    let _ = ValueType::new_enumeration(&woog_enum, lu_dog);

    for (number, ((field_name, span), field)) in variants.iter().enumerate() {
        match field {
            Some(EnumField::Struct(ref fields)) => {
                // We create a struct in the store here so that it's available for construction
                // in the struct expression code.
                // Note that the name of the struct includes the name of the enum, with a path
                // separator. This is cheap. I really need to think about how paths and imports
                // and the like are going to work. The model will need some updating methinks.
                let woog_struct =
                    WoogStruct::new(format!("{}::{}", name, field_name), None, None, lu_dog);
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
                    let ty = make_value_type(ty, ty_span, None, context, lu_dog)?;
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
                                let ty = make_value_type(generic, span, None, context, lu_dog)?;
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
                                let ty = make_value_type(&type_.0, span, None, context, lu_dog)?;
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
                            let ty = make_value_type(&type_.0, span, None, context, lu_dog)?;
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
                        let ty = make_value_type(&type_.0, span, None, context, lu_dog)?;
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
