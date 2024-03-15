use std::ops::Range;

use log::debug;
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            create_generic_struct, inter_expression, typecheck, update_span_value, Context,
            ExprSpan,
        },
        Expression as ParserExpression, PrintableValueType, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, DataStructure, Expression, FieldExpression,
        NamedFieldExpression, Span as LuDogSpan, Span, StructExpression, ValueType, ValueTypeEnum,
        XPath, XValue,
    },
    new_ref, s_read, s_write, NewRef, RefType, PATH_ROOT,
};

pub fn inter(
    name: Box<(ParserExpression, Range<usize>)>,
    fields: Vec<((String, Range<usize>), (ParserExpression, Range<usize>))>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let name_span = &name.1;
    let (_path, base, name) = if let ParserExpression::LocalVariable(obj) = &name.0 {
        (PATH_ROOT.to_owned(), obj.to_owned(), obj.to_owned())
    } else if let ParserExpression::PathInExpression(types) = &name.0 {
        let mut path = String::new();
        let mut base = String::new();
        let mut name = String::new();

        for (i, ty) in types.iter().enumerate() {
            if let Type::UserType((type_name, _), generics) = &ty.0 {
                name.extend([type_name.as_str()]);
                if i != types.len() - 1 {
                    path.extend([type_name.as_str()]);
                    path.extend(["::"]);
                    name.extend(["::"]);
                }
                if i == types.len() - 1 {
                    base = type_name.clone();
                }
                if !generics.is_empty() {
                    name.push('<');
                    for (i, (generic, _)) in generics.iter().enumerate() {
                        // if let Type::Generic((generic_name, _)) = generic {
                        name.extend([generic.to_string()]);

                        if i != generics.len() - 1 {
                            name.extend([", "]);
                        }
                        // }
                    }
                    name.push('>');
                }
            } else {
                return Err(vec![DwarfError::Internal {
                    description: format!(
                        "Expected a user type in struct expression, found {:?}",
                        ty
                    ),
                    location: location!(),
                }]);
            }
        }

        (path, base, name)
    } else {
        return Err(vec![DwarfError::Internal {
            description: format!(
                "Expected a local variable in struct expression, found {:?}",
                name.0
            ),
            location: location!(),
        }]);
    };

    // If name is in our scopes then we use the stored path, and add base.
    // I'm not quite sure what separates base from name.
    // Looking at the code above, it appears that base is just that, the base
    // of the struct expression, which can be a path -- that's cool. Name on the
    // other hand would include the generics, if there were any. The thing is
    // that I don't see how you can have generics to a struct expression... 🤔
    //
    // In any case, what we end up with is how use statements work. We bring
    // the item into our scope, and here we canonicalize the path.
    //
    // The thing is, even locally defined things are in the context scope, so
    // I don't know that the else makes sense. Well, code coverage says that
    // we hit it, so I don't know what it means.
    //
    // Well, what it means is that something isn't is scope, so we tack on the
    // current path to the base type. Nominally that path is "::". I'd need to
    // dig into when that's updated to maybe get some insight into what's going
    // on when the thing isn't in scope.
    let base = if let Some(path) = context.scopes.get(&name) {
        path.to_owned() + name.as_str()
    } else {
        context.path.clone() + base.as_str()
    };

    debug!("ParserExpression::Struct {}", name);

    // 🚧 Base or name? This is black magic at this point.
    let id = match lu_dog.exhume_woog_struct_id_by_name(&base) {
        Some(id) => id,
        None => match lu_dog.exhume_woog_struct_id_by_name(&name) {
            Some(id) => id,
            None => {
                return Err(vec![DwarfError::UnknownType {
                    ty: name.to_owned(),
                    file: context.file_name.to_owned(),
                    span: name_span.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }]);
            }
        },
    };

    let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
    let struct_fields = s_read!(woog_struct).r7_field(lu_dog);

    let data_struct = DataStructure::new_woog_struct(true, &woog_struct, lu_dog);

    // 🚧 Fix this.
    let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
    let struct_expr = StructExpression::new(Uuid::new_v4(), &data_struct, &x_path, lu_dog);

    let mut generic_substitutions = HashMap::default();

    for (field_name, field_expr) in fields {
        let field_expr_span = field_expr.1.to_owned();
        let (field_expr, ty) = inter_expression(
            &new_ref!(ParserExpression, field_expr.0.to_owned()),
            &field_expr_span,
            block,
            context,
            context_stack,
            lu_dog,
        )?;

        let pvt = PrintableValueType(true, &ty, context, lu_dog).to_string();

        debug!(
            "field `{}` is of type `{pvt}`, expr: {field_expr:?}",
            field_name.0
        );

        if let Some(field) = struct_fields
            .iter()
            .find(|f| s_read!(f).name == field_name.0)
        {
            let field_ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();
            let naked_ty = s_read!(field_ty);

            // Primarily we are here to check the type of the field against
            // the type of the expression. If only it were so easily done.
            // The issue is generics. If the field is generic, then we need
            // to initially use the type of the expression to fill in the
            // type of the generic parameter. *After* that however, we
            // need to use this new type to typecheck all subsequent uses
            // of the pattern.
            if let ValueTypeEnum::StructGeneric(ref id) = naked_ty.subtype {
                // OK. We are instantiating a generic. We need to create the new type
                let generic = lu_dog.exhume_struct_generic(id).unwrap();
                generic_substitutions.insert(s_read!(generic).name.to_owned(), ty.clone());
            } else {
                // We only need the type check if the type of the field in the struct
                // is not generic. Otherwise we are explicitly defining the type above.

                // We need to replace any occurrences of the generic type with the
                // concrete type in case there is any recursion going on. I'm just
                // not sure how to do that yet.

                typecheck(
                    (&field_ty, &field_name.1),
                    (&ty, &field_expr_span),
                    location!(),
                    context,
                    lu_dog,
                )?;
            }
        } else {
            return Err(vec![DwarfError::NoSuchField {
                name: name.to_string(),
                name_span: name_span.to_owned(),
                field: field_name.0.to_owned(),
                file: context.file_name.to_owned(),
                span: field_name.1.to_owned(),
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        }

        let nfe = NamedFieldExpression::new(field_name.0.to_owned(), lu_dog);
        let field =
            FieldExpression::new_named_field_expression(&field_expr.0, &struct_expr, &nfe, lu_dog);

        let expr = Expression::new_field_expression(true, &field, lu_dog);
        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
        update_span_value(&span, &value, location!());

        // # Span Bug
        // This is exceptional, at least so for. What's happening is that
        // the span is already pointing at a value. We've been clobbering
        // it successfully until the following case:
        // ```
        // // Does not work
        // Foo { bar: Bar::new()}
        // // Works
        // Foo { bar: Uuid::new()}
        // ```
        // So what do we do? Well I tried not overwriting, and lot's of
        // stuff broke. And really, it's two different values pointing at
        // the same span. So I just cloned it and inserted it.
        //
        // The thing that bothers me is where else might this be happening?
        if s_read!(field_expr.1).x_value.is_none() {
            s_write!(field_expr.1).x_value = Some(s_read!(value).id);
        } else {
            cfg_if::cfg_if! {
                if #[cfg(not(feature="debug"))] {
                    lu_dog.inter_span(|id| {
                        let mut span = s_read!(field_expr.1).clone();
                        span.x_value = Some(s_read!(value).id);
                        span.id = id;
                        new_ref!(LuDogSpan, span)
                    });
                } else {
                    let span = LuDogSpan::new(
                        s_read!(field_expr.1).end,
                        s_read!(field_expr.1).start,
                        &context.source,
                        None,
                        Some(&value),
                        lu_dog,
                    );
                }
            }
        }
    }

    let woog_struct = if !generic_substitutions.is_empty() {
        let span = s_read!(span);
        let span = span.start as usize..span.end as usize;
        let (woog_struct, _) = create_generic_struct(
            &woog_struct,
            &generic_substitutions,
            &span,
            context,
            context.sarzak,
            lu_dog,
        );

        woog_struct
    } else {
        woog_struct
    };

    // I love that the type of the thing is the same as the thing itself.
    let expr = Expression::new_struct_expression(true, &struct_expr, lu_dog);
    let ty = ValueType::new_woog_struct(true, &woog_struct, lu_dog);

    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
