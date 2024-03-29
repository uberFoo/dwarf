use ansi_term::Colour;
use snafu::{location, prelude::*, Location};

use crate::{
    chacha::error::{Result, UnimplementedSnafu},
    interpreter::{debug, eval_expression, function, Context},
    lu_dog::ValueTypeEnum,
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(expr: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();
    let sarzak = context.sarzak_heel().clone();

    let expr = s_read!(lu_dog).exhume_type_cast(expr).unwrap();
    debug!("ExpressionEnum::TypeCast {expr:?}");

    let lhs = s_read!(expr).r68_expression(&s_read!(lu_dog))[0].clone();
    let as_ty = s_read!(expr).r69_value_type(&s_read!(lu_dog))[0].clone();

    let lhs = eval_expression(lhs, context)?;

    let value = match &s_read!(as_ty).subtype {
        ValueTypeEnum::Ty(ref ty) => {
            let ty = s_read!(sarzak).exhume_ty(ty).unwrap();
            let x = match &*ty.read().unwrap() {
                Ty::Boolean(_) => {
                    let value: bool = (&*s_read!(lhs)).try_into()?;
                    new_ref!(Value, value.into())
                }
                Ty::Float(_) => {
                    let value: f64 = (&*s_read!(lhs)).try_into()?;
                    new_ref!(Value, value.into())
                }
                Ty::Integer(_) => {
                    let value: i64 = (&*s_read!(lhs)).try_into()?;
                    new_ref!(Value, value.into())
                }
                Ty::ZString(_) => {
                    let value: String = (&*s_read!(lhs)).try_into()?;
                    new_ref!(Value, value.into())
                }
                Ty::ZUuid(_) => {
                    let value: uuid::Uuid = (&*s_read!(lhs)).try_into()?;
                    new_ref!(Value, value.into())
                }
                ref alpha => {
                    ensure!(
                        false,
                        UnimplementedSnafu {
                            message: format!("deal with type cast as: {:?}", alpha),
                            location: location!(),
                        }
                    );
                    unreachable!();
                }
            };
            x
        }
        ref alpha => {
            ensure!(
                false,
                UnimplementedSnafu {
                    message: format!("deal with type cast as: {:?}", alpha),
                    location: location!(),
                }
            );
            unreachable!();
        }
    };

    Ok(value)
}
