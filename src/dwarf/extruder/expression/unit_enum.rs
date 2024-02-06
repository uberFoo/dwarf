use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{debug, function, update_span_value, Context, ExprSpan},
        Expression as ParserExpression, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, DataStructure, Expression, PathElement, Span,
        StructExpression, ValueType, ValueTypeEnum, XPath, XValue,
    },
    s_read, s_write, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
#[allow(clippy::borrowed_box)]
#[allow(clippy::too_many_arguments)]
pub fn inter(
    enum_path: &Box<(ParserExpression, Range<usize>)>,
    field_name: String,
    field_span: Range<usize>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("UnitEnum {enum_path:?}, Field {field_name}");
    let (path, path_span) =
        if let (ParserExpression::PathInExpression(path), span) = enum_path.as_ref() {
            (path, span)
        } else {
            panic!(
                "I don't think that we should ever see anything other than a path here: {:?}",
                enum_path
            );
        };

    debug!("path {path:?}");

    let Type::UserType((base_name, _), _) = &path[0].0 else {
        panic!(
            "I don't think that we should ever see anything other than a user type here: {:?}",
            path[0]
        );
    };
    let base_path = if let Some(path) = context.scopes.get(base_name) {
        path
    } else {
        &context.path
    };

    let full_enum_name = base_path.clone() + path.iter().map(|p| {
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

    debug!("enum_name {full_enum_name:?}, path {path:?}");

    let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
    let mut elts = path
        .iter()
        .inspect(|ty| {
            debug!("ty {:?}", ty);
        })
        .map(|ty| {
            if let Type::UserType((name, _), _generics) = &ty.0 {
                dbg!("fierce", &name);
                PathElement::new(name.to_owned(), None, &x_path, lu_dog)
            } else {
                unreachable!()
            }
        })
        .collect::<Vec<RefType<PathElement>>>();
    elts.push(PathElement::new(
        field_name.to_owned(),
        None,
        &x_path,
        lu_dog,
    ));

    let first = Some(elts[0].clone());
    let _last = elts
        .into_iter()
        .fold(Option::<RefType<PathElement>>::None, |prev, elt| {
            if let Some(prev) = prev {
                let elt = s_read!(elt);
                s_write!(prev).next = Some(elt.id);
            }
            Some(elt)
        });

    if let Some(first) = first {
        let first = s_read!(first).id;
        s_write!(x_path).first = Some(first);
    }

    if let Some(woog_enum_id) = {
        if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(&full_enum_name) {
            Some(woog_enum)
        } else if let Some(ty) = full_enum_name.split('<').next() {
            lu_dog.exhume_enumeration_id_by_name(ty)
        } else {
            None
        }
    } {
        // let woog_enum_id = if full_enum_name != full_enum_name {
        //     if let Some(id) = lu_dog.exhume_enumeration_id_by_name(&full_enum_name) {
        //         id
        //     } else {
        //         dbg!("β");
        //         let (new_enum, _) =
        //             create_generic_enum(&full_enum_name, &full_enum_name, context, lu_dog);
        //         let x = s_read!(new_enum).id;
        //         #[allow(clippy::let_and_return)]
        //         x
        //     }
        // } else {
        //     woog_enum_id
        // };

        let woog_enum = lu_dog.exhume_enumeration(&woog_enum_id).unwrap();

        let data_struct = DataStructure::new_enumeration(true, &woog_enum, lu_dog);

        let foo = s_read!(woog_enum).r88_enum_field(lu_dog);
        let field = foo.iter().find(|field| {
            let field = s_read!(field);
            field.name == field_name
        });

        if field.is_some() {
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

            // let expr = Expression::new_enum_field(field, lu_dog);
            let struct_expr = StructExpression::new(Uuid::new_v4(), &data_struct, &x_path, lu_dog);
            let expr = Expression::new_struct_expression(true, &struct_expr, lu_dog);
            debug!("expression {expr:?}");

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        } else {
            Err(vec![DwarfError::NoSuchField {
                name: full_enum_name.to_owned(),
                name_span: path_span.to_owned(),
                field: field_name.to_owned(),
                file: context.file_name.to_owned(),
                span: field_span.to_owned(),
                location: location!(),
                program: context.source_string.to_owned(),
            }])
        }
    } else {
        Err(vec![DwarfError::EnumNotFound {
            name: full_enum_name.to_owned(),
            file: context.file_name.to_owned(),
            span: path_span.to_owned(),
            location: location!(),
            program: context.source_string.to_owned(),
        }])
    }
}
