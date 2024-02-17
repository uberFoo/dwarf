use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
    },
    chacha::value::EnumVariant,
    lu_dog::{DataStructureEnum, FieldExpressionEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_struct_expr"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_struct_expression(expr).unwrap();
    let expr = s_read!(expr);

    let field_exprs = expr.r26_field_expression(&lu_dog);
    let data_struct = expr.r39_data_structure(&lu_dog)[0].clone();
    let data_struct = s_read!(data_struct);

    match &data_struct.subtype {
        DataStructureEnum::Enumeration(ref id) => {
            tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("creating enum"), file!(), line!(), column!());

            let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
            let woog_enum = s_read!(woog_enum);

            let x_path = &lu_dog.exhume_x_path(&expr.x_path).unwrap();
            // We know that there is always a pe. It's only in an option so that
            // we can construct everything.
            let mut pe = s_read!(x_path).r97_path_element(&lu_dog)[0].clone();
            let mut path = vec![s_read!(pe).name.to_owned()];

            while s_read!(pe).next.is_some() {
                let id = {
                    let id = &s_read!(pe).next;
                    #[allow(clippy::clone_on_copy)]
                    id.as_ref().unwrap().clone()
                };
                pe = lu_dog.exhume_path_element(&id).unwrap();
                path.push(s_read!(pe).name.to_owned());
            }

            let ty = woog_enum.r1_value_type(&lu_dog)[0].clone();
            let variant = path.pop().unwrap();
            let variant = new_ref!(Value, Value::String(variant));
            let path = path.join("::");
            let path = format!("{}{path}", woog_enum.x_path);

            //
            // This is where the rubber hits the road -- we give the enums actual values.
            if field_exprs.is_empty() {
                // 🚧 This is weird. I create the value and push it, and below I push all
                // the bits and have an instruction to create the thing. Which is it gonna
                // be? Now that I think of it, I think the implementation of the nte instruction
                // checks the cardinality of the fields, and if it's zero it generates a
                // unit enum. So that's two fishy things.
                let value = new_ref!(
                    Value,
                    Value::Enumeration(EnumVariant::Unit(ty, path, s_read!(pe).name.to_owned()))
                );
                thonk.insert_instruction(Instruction::Push(value), location!());
            } else {
                let field_count = field_exprs.len();
                for f in field_exprs {
                    let expr = s_read!(f).r15_expression(&lu_dog)[0].clone();
                    compile_expression(&expr, thonk, context)?;
                }

                let ty = new_ref!(Value, Value::ValueType((*s_read!(ty)).to_owned()));
                let path = new_ref!(Value, Value::String(path));
                thonk.insert_instruction(Instruction::Push(ty), location!());
                thonk.insert_instruction(Instruction::Push(path), location!());
                thonk.insert_instruction(Instruction::Push(variant), location!());

                thonk.insert_instruction_with_span(
                    Instruction::NewTupleEnum(field_count),
                    span,
                    location!(),
                );
            }
        }
        DataStructureEnum::WoogStruct(_) => {
            tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("creating struct"), file!(), line!(), column!());

            let woog_struct = if let DataStructureEnum::WoogStruct(ref id) = data_struct.subtype {
                lu_dog.exhume_woog_struct(id).unwrap()
            } else {
                unreachable!()
            };
            let woog_struct = s_read!(woog_struct);

            // Get name, value for each field expression.
            let field_count = field_exprs.len();
            for f in field_exprs {
                let f = s_read!(f);
                let expr = f.r15_expression(&lu_dog)[0].clone();

                compile_expression(&expr, thonk, context)?;

                let FieldExpressionEnum::NamedFieldExpression(ref name) = f.subtype else {
                    unreachable!()
                };
                let name = lu_dog.exhume_named_field_expression(name).unwrap();
                let name = s_read!(name).name.clone();
                let name = new_ref!(Value, Value::String(name));
                thonk.insert_instruction(Instruction::Push(name), location!());
            }

            let ty = woog_struct.r1_value_type(&lu_dog)[0].clone();
            let ty = new_ref!(Value, Value::ValueType((*s_read!(ty)).to_owned()));
            thonk.insert_instruction(Instruction::Push(ty), location!());

            let name = &woog_struct.name;
            let name = new_ref!(Value, name.to_owned().into());
            thonk.insert_instruction(Instruction::Push(name), location!());

            thonk.insert_instruction_with_span(
                Instruction::NewUserType(field_count),
                span,
                location!(),
            );
        }
    };

    Ok(None)
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, setup_logging},
            *,
        },
        chacha::value::UserStruct,
        dwarf::{new_lu_dog, parse_dwarf},
        s_write,
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn struct_expression() {
        setup_logging();
        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   struct Foo {
                       x: int,
                       y: float,
                   }
                   fn main() -> Foo {
                       Foo {
                           y: 0.42,
                           x: 42,
                       }
                   }";
        let ast = parse_dwarf("struct_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "struct_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 8);
        let run = run_vm(&program);
        assert!(run.is_ok());

        let mut lu_dog = s_write!(ctx.lu_dog);
        let woog_struct = lu_dog.exhume_woog_struct_id_by_name("::Foo").unwrap();
        let woog_struct = lu_dog.exhume_woog_struct(&woog_struct).unwrap();
        let ty = crate::lu_dog::ValueType::new_woog_struct(true, &woog_struct, &mut lu_dog);
        let mut result = UserStruct::new("Foo", &ty);
        result.define_field("x", new_ref!(Value, Value::Integer(42)));
        result.define_field("y", new_ref!(Value, Value::Float(0.42)));
        let result = Value::Struct(new_ref!(UserStruct, result));

        assert_eq!(&*s_read!(run.unwrap()), &result,);
    }
}
