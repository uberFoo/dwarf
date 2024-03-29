use std::ops::Range;

use ansi_term::Colour;
use snafu::{location, Location};

use crate::{
    chacha::error::Result,
    interpreter::{debug, eval_expression, function, ChaChaError, Context},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval_index(index: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let index = s_read!(lu_dog).exhume_index(index).unwrap();
    let index = s_read!(index);
    let target = s_read!(lu_dog).exhume_expression(&index.target).unwrap();
    let index_expr = s_read!(lu_dog).exhume_expression(&index.index).unwrap();

    debug!("index {target:?}[{index:?}]");

    let index = eval_expression(index_expr.clone(), context)?;
    let index = s_read!(index).clone();
    match &index {
        Value::Integer(index) => {
            let index = *index as usize;
            let list = eval_expression(target.clone(), context)?;
            let list = s_read!(list);
            if let Value::Vector { ty: _, inner: vec } = &list.clone() {
                let vec = s_read!(vec);
                if index < vec.len() {
                    Ok(vec[index].to_owned())
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
                    Ok(new_ref!(
                        Value,
                        Value::Char(str[index..index + 1].join("").chars().next().unwrap(),)
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
            let list = eval_expression(target.clone(), context)?;
            let list = s_read!(list);
            if let Value::Vector { ty, inner: vec } = &list.clone() {
                let vec = s_read!(vec);
                if range.end < vec.len() {
                    Ok(new_ref!(
                        Value,
                        Value::Vector {
                            ty: ty.clone(),
                            inner: new_ref!(Vec<RefType<Value>>, vec[range].to_owned())
                        }
                    ))
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
                    Ok(new_ref!(Value, Value::String(str[range].join(""),)))
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
        _ => unreachable!(),
    }
}
