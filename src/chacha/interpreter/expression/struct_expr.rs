use ansi_term::Colour;

use crate::{
    chacha::{
        error::Result,
        value::{EnumVariant, TupleEnum},
    },
    interpreter::{debug, eval_expression, function, Context, UserStruct},
    lu_dog::types::{DataStructureEnum, FieldExpressionEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub fn eval(expr: &SarzakStorePtr, context: &mut Context) -> Result<RefType<Value>> {
    let lu_dog = context.lu_dog_heel().clone();

    let expr = s_read!(lu_dog).exhume_struct_expression(expr).unwrap();
    let field_exprs = s_read!(expr).r26_field_expression(&s_read!(lu_dog));
    let data_struct = s_read!(expr).r39_data_structure(&s_read!(lu_dog))[0].clone();

    let subtype = &s_read!(data_struct).subtype;
    match subtype {
        DataStructureEnum::Enumeration(ref id) => {
            let woog_enum = s_read!(lu_dog).exhume_enumeration(id).unwrap();
            let x_path = &s_read!(lu_dog)
                .exhume_x_path(&s_read!(expr).x_path)
                .unwrap();
            // We know that there is always a pe. It's only in an option so that
            // we can construct everything.
            let mut pe = s_read!(x_path).r97_path_element(&s_read!(lu_dog))[0].clone();
            let mut path = vec![s_read!(pe).name.to_owned()];

            // Build the path from the elements.
            while s_read!(pe).next.is_some() {
                let id = {
                    let id = &s_read!(pe).next;
                    #[allow(clippy::clone_on_copy)]
                    id.as_ref().unwrap().clone()
                };
                pe = s_read!(lu_dog).exhume_path_element(&id).unwrap();
                path.push(s_read!(pe).name.to_owned());
            }

            let ty = s_read!(woog_enum).r1_value_type(&s_read!(lu_dog))[0].clone();
            let variant = path.pop().unwrap();
            let path = path.join("::");

            //
            // This is where we give the enums actual values -- the rubber hits the road.
            Ok(if field_exprs.is_empty() {
                new_ref!(
                    Value,
                    Value::Enumeration(EnumVariant::Unit(ty, path, s_read!(pe).name.to_owned()))
                )
            } else {
                // Get value and expression for each field expression.
                let field_values = field_exprs
                    .iter()
                    .map(|f| {
                        let expr = s_read!(f).r15_expression(&s_read!(lu_dog))[0].clone();
                        let value = eval_expression(expr.clone(), context)?;

                        debug!("StructExpression field value: {}", s_read!(value),);
                        Ok(value)
                    })
                    .collect::<Result<Vec<_>>>()?;

                // 🚧 Punting here -- we are just doing tuple enums for now. And
                // only single ones at that. I should just lift the restriction.
                // Tuples are only a notional thing anyway I think.
                let value = field_values[0].clone();

                let user_enum = TupleEnum::new(variant, value);
                let user_enum = new_ref!(TupleEnum<Value>, user_enum);
                new_ref!(
                    Value,
                    Value::Enumeration(EnumVariant::Tuple((ty, path), user_enum))
                )
            })
        }
        DataStructureEnum::WoogStruct(_) => {
            // Get name, value for each field expression.
            let field_exprs = field_exprs
                .iter()
                .map(|f| {
                    let expr = s_read!(f).r15_expression(&s_read!(lu_dog))[0].clone();
                    let value = eval_expression(expr.clone(), context)?;

                    let name = if let FieldExpressionEnum::NamedFieldExpression(ref id) =
                        s_read!(f).subtype
                    {
                        let nfe = s_read!(lu_dog).exhume_named_field_expression(id).unwrap();
                        let name = s_read!(nfe);
                        name.name.clone()
                    } else {
                        unreachable!()
                    };

                    debug!("StructExpression field value: {}", s_read!(value),);
                    Ok((name, value))
                })
                .collect::<Result<Vec<_>>>()?;

            let woog_struct =
                if let DataStructureEnum::WoogStruct(ref id) = s_read!(data_struct).subtype {
                    s_read!(lu_dog).exhume_woog_struct(id).unwrap()
                } else {
                    unreachable!()
                };
            let woog_struct = s_read!(woog_struct);
            let ty = woog_struct.r1_value_type(&s_read!(lu_dog))[0].clone();

            let name = &woog_struct.name;

            let mut user_type = UserStruct::new(name, &ty);
            for (name, value) in field_exprs {
                user_type.define_field(&name, value);
            }

            Ok(new_ref!(
                Value,
                Value::Struct(new_ref!(UserStruct<Value>, user_type))
            ))
        }
    }
}
