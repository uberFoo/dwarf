use std::ops::Range;

use log::debug;
use snafu::location;

use crate::{
    dwarf::{
        error::Result,
        extruder::{inter_expression, typecheck, update_span_value, Context, ExprSpan},
        Expression as ParserExpression,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore, Block, Comparison, Expression, Operator, Span, ValueType,
        XValue,
    },
    new_ref,
    sarzak::Ty,
    NewRef, RefType,
};

pub fn inter(
    expr: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    let (expr, ty) = inter_expression(
        &new_ref!(ParserExpression, expr.0.to_owned()),
        &expr.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    Ok((expr, ty))
}
