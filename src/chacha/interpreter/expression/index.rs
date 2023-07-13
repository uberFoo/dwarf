use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::{error::Result, vm::VM},
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    lu_dog::ValueType,
    new_ref, s_read, s_write,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_index(
    index: &SarzakStorePtr,
    context: &mut Context,
    vm: &mut VM,
) -> Result<(RefType<Value>, RefType<ValueType>)> {
    let lu_dog = context.lu_dog_heel().clone();

    let index = s_read!(lu_dog).exhume_index(index).unwrap();
    let index = s_read!(index);
    let target = s_read!(lu_dog).exhume_expression(&index.target).unwrap();
    let index_expr = s_read!(lu_dog).exhume_expression(&index.index).unwrap();

    debug!("index {target:?}[{index:?}]");

    let (index, _ty) = eval_expression(index_expr.clone(), context, vm)?;
    let index = s_read!(index).clone();
    match &index {
        Value::Integer(index) => {
            let index = *index as usize;
            let (list, ty) = eval_expression(target.clone(), context, vm)?;
            let list = s_read!(list);
            if let Value::Vector(vec) = list.clone() {
                if index < vec.len() {
                    Ok((vec[index].to_owned(), ty))
                } else {
                    let value = &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    Err(ChaChaError::IndexOutOfBounds {
                        index,
                        len: vec.len(),
                        span,
                        location: location!(),
                    })
                }
            } else if let Value::String(str) = &*list {
                let str = unicode_segmentation::UnicodeSegmentation::graphemes(str.as_str(), true)
                    .collect::<Vec<&str>>();

                if index < str.len() {
                    let ty = Ty::new_s_string();
                    let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));
                    Ok((
                        new_ref!(Value, Value::String(str[index..index + 1].join(""),)),
                        ty,
                    ))
                } else {
                    let value = &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    Err(ChaChaError::IndexOutOfBounds {
                        index,
                        len: str.len(),
                        span,
                        location: location!(),
                    })
                }
            } else {
                let value = &s_read!(target).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                let read = s_read!(span);
                let span = read.start as usize..read.end as usize;

                Err(ChaChaError::NotIndexable {
                    span,
                    location: location!(),
                })
            }
        }
        Value::Range(_) => {
            let range: Range<usize> = index.try_into()?;
            let (list, ty) = eval_expression(target.clone(), context, vm)?;
            let list = s_read!(list);
            if let Value::Vector(vec) = list.clone() {
                if range.end < vec.len() {
                    Ok((new_ref!(Value, Value::Vector(vec[range].to_owned())), ty))
                } else {
                    let value = &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    Err(ChaChaError::IndexOutOfBounds {
                        index: range.end,
                        len: vec.len(),
                        span,
                        location: location!(),
                    })
                }
            } else if let Value::String(str) = &*list {
                let str = unicode_segmentation::UnicodeSegmentation::graphemes(str.as_str(), true)
                    .collect::<Vec<&str>>();

                if range.end < str.len() {
                    let ty = Ty::new_s_string();
                    let ty = ValueType::new_ty(&ty, &mut s_write!(lu_dog));
                    Ok((new_ref!(Value, Value::String(str[range].join(""),)), ty))
                } else {
                    let value = &s_read!(index_expr).r11_x_value(&s_read!(lu_dog))[0];
                    let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                    let read = s_read!(span);
                    let span = read.start as usize..read.end as usize;

                    Err(ChaChaError::IndexOutOfBounds {
                        index: range.end,
                        len: str.len(),
                        span,
                        location: location!(),
                    })
                }
            } else {
                let value = &s_read!(target).r11_x_value(&s_read!(lu_dog))[0];
                let span = &s_read!(value).r63_span(&s_read!(lu_dog))[0];
                let read = s_read!(span);
                let span = read.start as usize..read.end as usize;

                Err(ChaChaError::NotIndexable {
                    span,
                    location: location!(),
                })
            }
        }
        _ => Err(ChaChaError::BadJuJu {
            message: "Index is not an integer".to_owned(),
            location: location!(),
        }),
    }
}
