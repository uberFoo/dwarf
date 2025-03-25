use std::ops::Range;

use log::debug;
use snafu::location;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{inter_expression, update_span_value, Context, ExprSpan, PrintableValueType},
        Expression as ParserExpression, WrappedValueType,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, ForLoop, Span, ValueType,
        ValueTypeEnum, XValue,
    },
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    iter: (String, Range<usize>),
    collection: Box<(ParserExpression, Range<usize>)>,
    body: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("For");

    let cspan = &collection.1;
    let collection = new_ref!(ParserExpression, collection.0.clone());

    let (collection, collection_ty) =
        inter_expression(&collection, cspan, block, context, import_stack, lu_dog)?;

    let collection_ty = match s_read!(collection_ty).subtype {
        ValueTypeEnum::List(ref id) => {
            let list = lu_dog.exhume_list(id).unwrap();
            let list = s_read!(list);
            list.r36_value_type(lu_dog)[0].clone()
        }
        ValueTypeEnum::Range(_) => {
            // 🚧  I'm punting here. I think range can be something other than an int.
            // For example, what if you wanted a..f? I need to think about this, and
            // check what rust does. I'm actually too tired right now to think about
            // it. Related to range_type_bug.
            // 🚧 Of course rust does not work on chars. Doesn't mean I don't want to.
            ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog)
        }
        ValueTypeEnum::Ty(ref id) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::ZString(_) => ValueType::new_char(true, lu_dog),
                _ => {
                    let ty = PrintableValueType(true, &collection_ty, context, lu_dog).to_string();
                    return Err(vec![DwarfError::NotAList {
                        file: context.file_name.to_owned(),
                        span: cspan.to_owned(),
                        ty,
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
            }
        }
        _ => {
            let ty = PrintableValueType(true, &collection_ty, context, lu_dog).to_string();
            return Err(vec![DwarfError::NotAList {
                file: context.file_name.to_owned(),
                span: cspan.to_owned(),
                ty,
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        }
    };

    let bspan = &body.1;
    let body = match &body.0 {
        ParserExpression::Block(a_sink, body, vars, tys) if vars.is_empty() && tys.is_empty() => {
            ParserExpression::Block(
                a_sink.to_owned(),
                body.to_owned(),
                vec![iter.0.to_owned()],
                vec![WrappedValueType(collection_ty)],
            )
        }
        _ => unreachable!(),
    };
    let body = new_ref!(ParserExpression, body.to_owned());

    let ((body, _), _body_ty) =
        inter_expression(&body, bspan, block, context, import_stack, lu_dog)?;

    // I think that the model should be changed so that the For Loop takes
    // an Expression rather than a Body.
    let for_loop = ForLoop::new(iter.0.to_owned(), &body, &collection.0, lu_dog);
    let expr = Expression::new_for_loop(true, &for_loop, lu_dog);
    let ty = ValueType::new_empty(true, lu_dog);

    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
