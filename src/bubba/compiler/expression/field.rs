use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::{FieldAccessTargetEnum, ValueType},
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, Value, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile_field_access(
    field: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_field_access"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let field = lu_dog.exhume_field_access(field).unwrap();
    let field = s_read!(field);

    // This is the expression upon which we access the field
    let expr = lu_dog.exhume_expression(&field.expression).unwrap();
    compile_expression(&expr, thonk, context)?;

    let fat = &field.r65_field_access_target(&lu_dog)[0];
    let field_name = match s_read!(fat).subtype {
        FieldAccessTargetEnum::EnumField(ref field) => {
            let field = lu_dog.exhume_enum_field(field).unwrap();
            let field = s_read!(field);
            field.name.to_owned()
        }
        FieldAccessTargetEnum::Field(ref field) => {
            let field = lu_dog.exhume_field(field).unwrap();
            let field = s_read!(field);
            field.name.to_owned()
        }
        FieldAccessTargetEnum::Function(ref func) => {
            let func = lu_dog.exhume_function(func).unwrap();
            let func = s_read!(func);
            func.name.to_owned()
        }
    };
    thonk.insert_instruction_with_span(
        Instruction::Push(new_ref!(Value, Value::String(field_name))),
        span.clone(),
        location!(),
    );

    thonk.insert_instruction_with_span(Instruction::FieldRead, span, location!());

    Ok(None)
}

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile_field_expression(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_field_expression"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let field_expr = lu_dog.exhume_field_expression(expr).unwrap();
    let expr = s_read!(field_expr).r38_expression(&lu_dog)[0].clone();

    compile_expression(&expr, thonk, context)?;

    Ok(None)
}

#[cfg(test)]
mod test {
    use crate::{
        bubba::compiler::{
            test::{get_dwarf_home, run_vm, setup_logging},
            *,
        },
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[test]
    fn test_struct_field_read() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "struct Foo {
                       bar: int,
                   }
                   fn main() -> int {
                       let foo = Foo { bar: 42 };
                       foo.bar
                   }";
        let ast = parse_dwarf("test_struct_field_read", ore).unwrap();
        let ctx = new_lu_dog(
            "test_struct_field_read".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        let result = run_vm(&program);
        assert!(result.is_ok());
        assert_eq!(*s_read!(result.unwrap()), 42.into());
    }
}
