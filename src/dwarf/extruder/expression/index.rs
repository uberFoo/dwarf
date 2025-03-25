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
        store::ObjectStore as LuDogStore, Block, Expression, Index, Span, ValueType, ValueTypeEnum,
        XValue,
    },
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType,
};

pub fn inter(
    target_p: Box<(ParserExpression, Range<usize>)>,
    index_p: Box<(ParserExpression, Range<usize>)>,
    span: RefType<Span>,
    block: &RefType<Block>,
    context: &mut Context,
    import_stack: &mut Vec<String>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("index {target_p:?}, {index_p:?}");
    let (target, target_ty) = inter_expression(
        &new_ref!(ParserExpression, target_p.0.to_owned()),
        &target_p.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;
    debug!("target: {target:?}, ty: {target_ty:?}");
    let (index, index_ty) = inter_expression(
        &new_ref!(ParserExpression, index_p.0.to_owned()),
        &index_p.1,
        block,
        context,
        import_stack,
        lu_dog,
    )?;

    let int_ty = ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog);

    let index_span = s_read!(index.1).start as usize..s_read!(index.1).end as usize;
    typecheck(
        (&int_ty, &index_span),
        (&index_ty, &index_p.1),
        location!(),
        context,
        lu_dog,
    )?;

    // We need to dereference the list and return the underlying type.
    let target_ty = if let ValueTypeEnum::List(ref ty) = s_read!(target_ty).subtype {
        let list = lu_dog.exhume_list(ty).unwrap();
        let ty = &s_read!(list).r36_value_type(lu_dog)[0];
        ty.clone()
    } else if let ValueTypeEnum::Ty(ref ty) = s_read!(target_ty).subtype {
        let ty = context.sarzak.exhume_ty(ty).unwrap();
        let ty = ty.read().unwrap();
        if let Ty::ZString(_) = &*ty {
            ValueType::new_char(true, lu_dog)
        } else {
            let ty = PrintableValueType(true, &target_ty, context, lu_dog).to_string();
            return Err(vec![DwarfError::NotAList {
                file: context.file_name.to_owned(),
                span: target_p.1.clone(),
                ty,
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        }
    } else {
        let ty = PrintableValueType(true, &target_ty, context, lu_dog).to_string();
        return Err(vec![DwarfError::NotAList {
            file: context.file_name.to_owned(),
            span: target_p.1.clone(),
            ty,
            location: location!(),
            program: context.source_string.to_owned(),
        }]);
    };

    let index = Index::new(&index.0, &target.0, lu_dog);
    let expr = Expression::new_index(true, &index, lu_dog);
    let value = XValue::new_expression(block, &target_ty, &expr, lu_dog);
    update_span_value(&span, &value, location!());

    Ok(((expr, span), target_ty))
}
