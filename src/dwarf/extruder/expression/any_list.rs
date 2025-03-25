use std::ops::Range;

use ansi_term::Colour;
use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{
            debug, function, inter_expression, link_list_element, update_span_value, Context,
            ExprSpan,
        },
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, ListElement, ListExpression, Span,
        Span as LuDogSpan, ValueType, XValue,
    },
    new_ref, s_read, s_write, NewRef, RefType, SarzakStorePtr,
};

pub fn inter(
    elements: &[(ParserExpression, Range<usize>)],
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("anylist {:?}", elements);
    if elements.is_empty() {
        panic!("Just don't do this. It doesn't even merit an error.");
    } else {
        let mut elements = elements.iter();

        let element = elements.next().unwrap();
        let ((first, first_span), first_ty) = inter_expression(
            &new_ref!(ParserExpression, element.0.to_owned()),
            &element.1,
            block,
            context,
            import_stack,
            lu_dog,
        )?;

        let element = ListElement::new(0, &first, None, lu_dog);
        let expr = Expression::new_list_element(true, &element, lu_dog);
        let value = XValue::new_expression(block, &first_ty, &expr, lu_dog);

        // We need to clone the span because it's already been used
        // by the underlying value.
        LuDogSpan::new(
            s_read!(first_span).end,
            s_read!(first_span).start,
            &context.source,
            None,
            Some(&value),
            lu_dog,
        );

        let list_expr = ListExpression::new(
            Some(&element),
            &ValueType::new_unknown(true, lu_dog),
            lu_dog,
        );

        let mut last_element_uuid: Option<SarzakStorePtr> = Some(s_read!(element).id);
        let mut position = 1;
        for element in elements {
            let ((elt, elt_span), elt_ty) = inter_expression(
                &new_ref!(ParserExpression, element.0.to_owned()),
                &element.1,
                block,
                context,
                import_stack,
                lu_dog,
            )?;

            let element = ListElement::new(position, &elt, None, lu_dog);
            position += 1;

            last_element_uuid = link_list_element!(last_element_uuid, element, lu_dog);
            let expr = Expression::new_list_element(true, &element, lu_dog);
            let value = XValue::new_expression(block, &elt_ty, &expr, lu_dog);
            LuDogSpan::new(
                s_read!(elt_span).end,
                s_read!(elt_span).start,
                &context.source,
                None,
                Some(&value),
                lu_dog,
            );
        }

        let expr = Expression::new_list_expression(true, &list_expr, lu_dog);
        let ty = ValueType::new_any_list(true, lu_dog);
        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
        update_span_value(&span, &value, location!());

        Ok(((expr, span), ty))
    }
}
