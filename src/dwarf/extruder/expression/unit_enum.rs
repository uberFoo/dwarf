use std::ops::Range;

use log::debug;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{update_span_value, Context, ExprSpan},
        items::enuum::create_generic_enum,
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
    context_stack: &mut [(String, RefType<LuDogStore>)],
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("UnitEnum {:?}, Field {field_name}", enum_path);
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

    // 🚧 Looks like we are validating each element of the path, but int, in Option::<int> isn't a
    // UserType, so I'm confused.
    // I think that if you look, it's actually coming in as Option<int>, which can be a UserType.
    let full_enum_name = path.iter().map(|p| {
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
            }).collect::<Vec<_>>().join("");

    let enum_root = if let Some(root) = full_enum_name.split('<').next() {
        root
    } else {
        full_enum_name.as_str()
    };

    debug!("enum_name {:?}", full_enum_name);
    // dbg!(&enum_root, &full_enum_name, &enum_path);
    let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
    let mut elts = path
        .iter()
        .inspect(|ty| {
            debug!("ty {:?}", ty);
        })
        .map(|ty| {
            if let Type::UserType((name, _), _generics) = &ty.0 {
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
        if let Some(woog_enum) = lu_dog.exhume_enumeration_id_by_name(enum_root) {
            Some(woog_enum)
        } else {
            let mut iter = context_stack.iter();
            loop {
                if let Some((path, lu_dog)) = iter.next() {
                    dbg!(&path, &enum_root);
                    if let Some(woog_enum) =
                        s_read!(lu_dog).exhume_enumeration_id_by_name(enum_root)
                    {
                        dbg!("found it");
                        break Some(woog_enum);
                    }
                } else {
                    dbg!("not found");
                    break None;
                }
            }
        }
    } {
        let woog_enum_id = if enum_root != full_enum_name {
            if let Some(id) = lu_dog.exhume_enumeration_id_by_name(&full_enum_name) {
                id
            } else {
                let (new_enum, _) = create_generic_enum(
                    &full_enum_name,
                    &enum_path.0,
                    context,
                    context_stack,
                    lu_dog,
                );
                let x = s_read!(new_enum).id;
                #[allow(clippy::let_and_return)]
                x
            }
        } else {
            woog_enum_id
        };

        let woog_enum = lu_dog.exhume_enumeration(&woog_enum_id).unwrap();

        let data_struct = DataStructure::new_enumeration(&woog_enum, lu_dog);

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
            let expr = Expression::new_struct_expression(&struct_expr, lu_dog);
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
            }])
        }
    } else {
        Err(vec![DwarfError::EnumNotFound {
            name: full_enum_name.to_owned(),
            file: context.file_name.to_owned(),
            span: path_span.to_owned(),
        }])
    }
}
