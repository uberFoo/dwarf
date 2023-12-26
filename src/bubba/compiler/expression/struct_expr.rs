use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    chacha::value::{EnumVariant, TupleEnum, UserStruct},
    lu_dog::{DataStructureEnum, FieldExpressionEnum},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value,
};

pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_struct_expression(expr).unwrap();
    let expr = s_read!(expr);

    let field_exprs = expr.r26_field_expression(&lu_dog);
    let data_struct = expr.r39_data_structure(&lu_dog)[0].clone();
    let data_struct = s_read!(data_struct);

    match &data_struct.subtype {
        DataStructureEnum::Enumeration(ref id) => {
            let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
            let x_path = &lu_dog.exhume_x_path(&expr.x_path).unwrap();
            // We know that there is always a pe. It's only in an option so that
            // we can construct everything.
            let mut pe = s_read!(x_path).r97_path_element(&lu_dog)[0].clone();
            let mut path = vec![s_read!(pe).name.to_owned()];

            // Get the last path element.
            while s_read!(pe).next.is_some() {
                let id = {
                    let id = &s_read!(pe).next;
                    #[allow(clippy::clone_on_copy)]
                    id.as_ref().unwrap().clone()
                };
                pe = lu_dog.exhume_path_element(&id).unwrap();
                path.push(s_read!(pe).name.to_owned());
            }

            let ty = s_read!(woog_enum).r1_value_type(&lu_dog)[0].clone();
            let variant = path.pop().unwrap();
            let path = path.join("::");

            //
            // This is where we give the enums actual values -- the rubber hits the road.
            if field_exprs.is_empty() {
                let value = new_ref!(
                    Value,
                    Value::Enumeration(EnumVariant::Unit(ty, path, s_read!(pe).name.to_owned()))
                );
                thonk.add_instruction(Instruction::Push(value));
            } else {
                unimplemented!()
                // // Get value and expression for each field expression.
                // let field_values = field_exprs
                //     .iter()
                //     .map(|f| {
                //         let expr = s_read!(f).r15_expression(&lu_dog)[0].clone();
                //         let value = eval_expression(expr.clone(), context, vm)?;

                //         debug!("StructExpression field value: {}", s_read!(value),);
                //         Ok(value)
                //     })
                //     .collect::<Result<Vec<_>>>()?;

                // // 🚧 Punting here -- we are just doing tuple enums for now. And
                // // only single ones at that. I should just lift the restriction.
                // // Tuples are only a notional thing anyway I think.
                // // (field_values[0].clone(), UserEnumType::Tuple)
                // let value = field_values[0].clone();

                // let user_enum = TupleEnum::new(variant, value);
                // let user_enum = new_ref!(TupleEnum, user_enum);
                // new_ref!(
                //     Value,
                //     Value::Enumeration(EnumVariant::Tuple((ty, path), user_enum))
                // )
            }
        }
        DataStructureEnum::WoogStruct(_) => {
            unimplemented!()
            //     // Get name, value for each field expression.
            //     let field_exprs = field_exprs
            //         .iter()
            //         .map(|f| {
            //             let expr = s_read!(f).r15_expression(&lu_dog)[0].clone();
            //             let value = eval_expression(expr.clone(), context, vm)?;

            //             let name = if let FieldExpressionEnum::NamedFieldExpression(ref id) =
            //                 s_read!(f).subtype
            //             {
            //                 let nfe = lu_dog.exhume_named_field_expression(id).unwrap();
            //                 let name = s_read!(nfe);
            //                 name.name.clone()
            //             } else {
            //                 unreachable!()
            //             };

            //             debug!("StructExpression field value: {}", s_read!(value),);
            //             Ok((name, value))
            //         })
            //         .collect::<Result<Vec<_>>>()?;

            //     let woog_struct = if let DataStructureEnum::WoogStruct(ref id) = data_struct.subtype {
            //         lu_dog.exhume_woog_struct(id).unwrap()
            //     } else {
            //         unreachable!()
            //     };
            //     let ty = s_read!(woog_struct).r1_value_type(&lu_dog)[0].clone();

            //     let mut user_type = UserStruct::new(&s_read!(woog_struct).name, &ty);
            //     for (name, value) in field_exprs {
            //         user_type.define_field(&name, value);
            //     }

            //     Ok(new_ref!(
            //         Value,
            //         Value::Struct(new_ref!(UserStruct, user_type))
            //     ))
        }
    };

    Ok(())
}
