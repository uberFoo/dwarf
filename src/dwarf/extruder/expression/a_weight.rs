use std::ops::Range;

use ansi_term::Colour;
use snafu::location;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{debug, function, inter_expression, update_span_value, Context, ExprSpan},
        Expression as ParserExpression, PrintableValueType,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, AWait, Block, Expression, Span, ValueType, ValueTypeEnum,
        XValue,
    },
    new_ref, s_read, NewRef, RefType,
};

// Let's just say that I don't get this lint. The docs say you have to box it
// first, but what about when it's already boxed? I don't get it.
pub fn inter(
    expr_p: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("await: {expr_p:?}");

    let (expr, ty) = inter_expression(
        &new_ref!(ParserExpression, expr_p.0.to_owned()),
        &expr_p.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;

    if !matches!(s_read!(ty).subtype, ValueTypeEnum::XFuture(_)) {
        let ty = PrintableValueType(true, &ty, context, lu_dog);
        Err(vec![DwarfError::AwaitNotFuture {
            file: context.file_name.to_owned(),
            found: ty.to_string(),
            span: expr_p.1.clone(),
            program: context.source_string.to_owned(),
        }])
    } else {
        let future = match s_read!(ty).subtype {
            ValueTypeEnum::XFuture(ref id) => lu_dog.exhume_x_future(id).unwrap(),
            _ => unreachable!(),
        };
        let ty = s_read!(future).r2_value_type(lu_dog)[0].clone();
        let expr = AWait::new(&expr.0, lu_dog);
        let expr = Expression::new_a_wait(true, &expr, lu_dog);
        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
        update_span_value(&span, &value, location!());

        Ok(((expr, span), ty))
    }
}
