use std::ops::Range;

use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, ExpressionBit, FormatBit,
        FormatString, Literal, Span, ValueType, XValue,
    },
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr,
};

macro_rules! link_format_bits {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_format_bit(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

pub fn inter(
    bits: &Vec<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let format_string = FormatString::new(None, lu_dog);
    let literal = Literal::new_format_string(true, &format_string, lu_dog);
    let expr = Expression::new_literal(true, &literal, lu_dog);
    let ty = ValueType::new_ty(true, &Ty::new_z_string(context.sarzak), lu_dog);
    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    let mut last_format_bit_uuid: Option<SarzakStorePtr> = None;
    for (bit, span) in bits {
        let ((expr, _), _) = inter_expression(
            &new_ref!(ParserExpression, bit.to_owned()),
            &span,
            block,
            context,
            import_stack,
            lu_dog,
        )?;

        let expr_bit = ExpressionBit::new(&expr, lu_dog);
        let format_bit = FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog);

        if last_format_bit_uuid.is_none() {
            s_write!(format_string).first_format_bit = Some(s_read!(format_bit).id);
        }
        last_format_bit_uuid = link_format_bits!(last_format_bit_uuid, format_bit, lu_dog);
    }

    Ok(((expr, span), ty))
}
