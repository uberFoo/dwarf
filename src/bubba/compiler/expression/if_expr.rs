use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{compile_expression, get_span, CThonk, Context, Result},
        instr::Instruction,
    },
    lu_dog::ValueType,
    new_ref, s_read, NewRef, RefType, SarzakStorePtr, Value, POP_CLR,
};

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    expr: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_if_expr"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let expr = lu_dog.exhume_x_if(expr).unwrap();
    let expr = s_read!(expr);

    let cond_expr = lu_dog.exhume_expression(&expr.test).unwrap();

    compile_expression(&cond_expr, thonk, context)?;

    thonk.insert_instruction(
        Instruction::Push(new_ref!(Value, Value::Boolean(true))),
        location!(),
    );
    thonk.insert_instruction(Instruction::TestEqual, location!());

    // Compile the false block
    let false_thonk = if let Some(ref expr) = expr.false_block {
        context.push_scope();
        let mut false_thonk = CThonk::new("if_false".to_owned());
        let block = lu_dog.exhume_expression(expr).unwrap();

        compile_expression(&block, &mut false_thonk, context)?;

        let fp = false_thonk.get_frame_size();
        for _ in 0..fp {
            thonk.increment_frame_size();
        }
        context.pop_scope();
        Some(false_thonk)
    } else {
        None
    };

    // Compile the true block.
    context.push_scope();
    let mut true_thonk = CThonk::new("if_true".to_owned());
    let block = lu_dog.exhume_block(&expr.true_block).unwrap();
    let block = s_read!(block).r15_expression(&lu_dog)[0].clone();

    compile_expression(&block, &mut true_thonk, context)?;

    let fp = true_thonk.get_frame_size();
    for _ in 0..fp {
        thonk.increment_frame_size();
    }
    let true_block_len = true_thonk.get_instruction_card() as isize;
    context.pop_scope();

    thonk.insert_instruction(Instruction::JumpIfFalse(true_block_len + 1), location!());
    thonk.append(true_thonk);
    if let Some(false_thonk) = &false_thonk {
        thonk.insert_instruction(
            Instruction::Jump(false_thonk.get_instruction_card() as isize),
            location!(),
        );
    }
    if let Some(false_thonk) = false_thonk {
        thonk.append(false_thonk);
    }

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
    fn if_expression_true_arm() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       if true {
                           1
                       } else {
                           2
                       }
                   }";
        let ast = parse_dwarf("if_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "if_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 8);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn if_expression_false_arm() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       if false {
                           1
                       } else {
                           2
                       }
                   }";
        let ast = parse_dwarf("if_expression_else_arm", ore).unwrap();
        let ctx = new_lu_dog(
            "if_expression_else_arm".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 8);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(2));
    }

    #[test]
    fn if_expression_complex() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       if 1 == 1 {
                           print(\"true\");
                            if 0 == 1 {
                                print(\"false\");
                                x = 3
                            } else {
                                print(\"true\");
                                x = 10;
                                for i in 0..9 {
                                    x = x - 1;
                                }
                                print(\"past one\");
                            };
                       } else {
                           for i in 0..10 {
                               x = x + i;
                           }
                           print(\"false\");
                           x = 2;
                       };

                       x
                   }";
        let ast = parse_dwarf("if_expression_complex_condition", ore).unwrap();
        let ctx = new_lu_dog(
            "if_expression_complex_condition".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 68);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }
}
