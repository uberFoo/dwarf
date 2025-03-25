use std::ops::Range;

use log::debug;
use snafu::location;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        extruder::{
            inter_expression, typecheck, update_span_value, Context, ExprSpan, PrintableValueType,
        },
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Expression, ExpressionEnum, Span, ValueType,
        ValueTypeEnum, XIf, XValue,
    },
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType,
};

pub fn inter(
    conditional: Box<(ParserExpression, Range<usize>)>,
    true_block: Box<(ParserExpression, Range<usize>)>,
    false_block: Option<Box<(ParserExpression, Range<usize>)>>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("conditional {:?}", conditional);
    let cspan = &conditional.1;
    let conditional = new_ref!(ParserExpression, conditional.0.to_owned());
    let (conditional, conditional_ty) =
        inter_expression(&conditional, cspan, block, context, import_stack, lu_dog)?;
    debug!("ParserExpression::If {:?}", conditional_ty);

    // Check that the conditional expression evaluates to a boolean.
    // Note that this first check is necessary to unwrap the sarzak type
    // from the lu_dog type.
    if let ValueTypeEnum::Ty(ref ty) = s_read!(conditional_ty).subtype {
        let s_ty = context.sarzak.exhume_ty(ty).unwrap();
        let s_ty = s_ty.read().unwrap();
        if let Ty::Boolean(_) = &*s_ty {
            // Good Times.
        } else {
            let bty = ValueType::new_ty(true, &Ty::new_boolean(context.sarzak), lu_dog);
            let bty = PrintableValueType(true, &bty, context, lu_dog);
            let ty = PrintableValueType(true, &conditional_ty, context, lu_dog);
            return Err(vec![DwarfError::TypeMismatch {
                expected: bty.to_string(),
                found: ty.to_string(),
                file: context.file_name.to_owned(),
                expected_span: cspan.to_owned(),
                found_span: cspan.to_owned(),
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        }
    } else {
        let bty = ValueType::new_ty(true, &Ty::new_boolean(context.sarzak), lu_dog);
        let bty = PrintableValueType(true, &bty, context, lu_dog);
        let ty = PrintableValueType(true, &conditional_ty, context, lu_dog);
        return Err(vec![DwarfError::TypeMismatch {
            expected: bty.to_string(),
            found: ty.to_string(),
            file: context.file_name.to_owned(),
            expected_span: cspan.to_owned(),
            found_span: cspan.to_owned(),
            location: location!(),
            program: context.source_string.to_owned(),
        }]);
    }

    let tspan = &true_block.1;
    let true_block = new_ref!(ParserExpression, true_block.0.to_owned());
    let (true_block, true_ty) =
        inter_expression(&true_block, tspan, block, context, import_stack, lu_dog)?;
    let true_block = if let ExpressionEnum::Block(true_block) = s_read!(true_block.0).subtype {
        true_block
    } else {
        panic!("Expected a block expression");
    };
    let true_block = lu_dog.exhume_block(&true_block).unwrap();

    let false_block = if let Some(false_block) = false_block {
        let fspan = &false_block.1;
        let false_block = new_ref!(ParserExpression, false_block.0.to_owned());
        let (false_block, _false_ty) =
            inter_expression(&false_block, fspan, block, context, import_stack, lu_dog)?;
        Some(false_block.0)
    } else {
        None
    };

    let if_expr = XIf::new(false_block.as_ref(), &conditional.0, &true_block, lu_dog);
    let expr = Expression::new_x_if(true, &if_expr, lu_dog);

    let ty = true_ty;

    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), ty))
}
