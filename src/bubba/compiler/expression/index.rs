use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, BubbaCompilerError, CThonk, Context, Result},
        instr::Instruction,
    },
    chacha::interpreter::{ModelContext, PrintableValueType},
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref, s_read,
    sarzak::Ty,
    ModelStore, NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

#[cfg_attr(not(test), tracing::instrument(skip(context)))]
pub(in crate::bubba::compiler) fn compile(
    index: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_index"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);
    let sarzak = context.sarzak_heel().clone();
    let sarzak = s_read!(sarzak);

    let index = lu_dog.exhume_index(index).unwrap();
    let index = s_read!(index);

    let target = lu_dog.exhume_expression(&index.target).unwrap();
    let list_type = compile_expression(&target, thonk, context)?;

    let index_expr = lu_dog.exhume_expression(&index.index).unwrap();
    let index_type = compile_expression(&index_expr, thonk, context)?;

    match index_type {
        Some(v_ty) => match v_ty.subtype {
            ValueTypeEnum::Ty(ref ty) => {
                let ty = sarzak.exhume_ty(ty).unwrap();
                let ty = s_read!(ty);
                match &*ty {
                    Ty::Integer(_) => {
                        thonk.insert_instruction_with_span(
                            Instruction::ListIndex,
                            span,
                            location!(),
                        );
                    }
                    _ => {
                        let ctx = ModelContext::new(
                            context.lu_dog_heel(),
                            context.sarzak_heel(),
                            new_ref!(ModelStore, context.extruder_context.models.clone()),
                        );
                        let ty = PrintableValueType(true, new_ref!(ValueType, v_ty), &ctx);
                        Err(BubbaCompilerError::NotIndexable {
                            ty: ty.to_string(),
                            span,
                            location: Location::new(file!(), line!(), column!()),
                        })?
                    }
                }
            }
            ValueTypeEnum::Range(_) => {
                thonk.insert_instruction_with_span(Instruction::ListIndexRange, span, location!());
            }
            _ => {
                let ctx = ModelContext::new(
                    context.lu_dog_heel(),
                    context.sarzak_heel(),
                    new_ref!(ModelStore, context.extruder_context.models.clone()),
                );
                let ty = PrintableValueType(true, new_ref!(ValueType, v_ty), &ctx);
                Err(BubbaCompilerError::NotIndexable {
                    ty: ty.to_string(),
                    span,
                    location: location!(),
                })?
            }
        },
        None => Err(BubbaCompilerError::InternalCompilerError {
            message: "index type is None".to_owned(),
            location: location!(),
        })?,
    }

    Ok(list_type)
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
    fn index_into_list() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       let y = [];
                       let x = [1, 2, 3];
                       x[1]
                   }";
        let ast = parse_dwarf("index_into_list", ore).unwrap();
        let ctx = new_lu_dog(
            "index_into_list".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 13);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &2.into());
    }

    #[test]
    fn index_out_of_bounds() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       let x = [1, 2, 3];
                       x[3]
                   }";
        let ast = parse_dwarf("index_out_of_bounds", ore).unwrap();
        let ctx = new_lu_dog(
            "index_out_of_bounds".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{}", run_vm(&program).unwrap_err());
    }

    #[test]
    fn index_into_string() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> char {
                       let x = \"foo\";
                       x[1]
                   }";
        let ast = parse_dwarf("index_into_string", ore).unwrap();
        let ctx = new_lu_dog(
            "index_into_string".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Char('o'));
    }
}
