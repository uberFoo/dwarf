use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, CThonk, Context, Result},
        instr::Instruction,
        value::Value,
    },
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(context)))]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_typecast"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_type_cast(expr).unwrap();
    let expr = s_read!(expr);
    let lhs = expr.r68_expression(&lu_dog)[0].clone();
    let as_ty = expr.r69_value_type(&lu_dog)[0].clone();

    compile_expression(&lhs, thonk, context)?;

    thonk.insert_instruction_with_span(
        Instruction::TypeCast(new_ref!(Value, Value::ValueType((*s_read!(as_ty)).clone()))),
        span,
        location!(),
    );

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
    fn test_typecast_boolean() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> bool {
                        42 as bool
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
        dbg!(&result);
        assert!(result.is_ok());
        assert_eq!(*s_read!(result.unwrap()), true.into());
    }

    #[test]
    fn test_typecast_integer() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                        \"42\" as int
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
        dbg!(&result);
        assert!(result.is_ok());
        assert_eq!(*s_read!(result.unwrap()), 42.into());
    }

    #[test]
    fn test_typecast_float() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> float {
                        \"42\" as float
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
        dbg!(&result);
        assert!(result.is_ok());
        assert_eq!(*s_read!(result.unwrap()), 42.0.into());
    }

    #[test]
    fn test_typecast_string() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                        42 as string
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
        dbg!(&result);
        assert!(result.is_ok());
        assert_eq!(*s_read!(result.unwrap()), "42".into());
    }

    #[test]
    fn test_typecast_uuid() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> Uuid {
                        \"7056c5ba-4f49-404f-abd5-e5c6a5879fac\" as Uuid
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
        dbg!(&result);
        assert!(result.is_ok());
        assert_eq!(
            *s_read!(result.unwrap()),
            uuid::Uuid::parse_str("7056c5ba-4f49-404f-abd5-e5c6a5879fac")
                .unwrap()
                .into()
        );
    }
}
