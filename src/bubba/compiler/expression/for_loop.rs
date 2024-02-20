use snafu::{location, Location};

use crate::{
    bubba::{
        compiler::{
            compile_expression, get_span, BubbaError, CThonk, Context, Result, INT, STRING,
        },
        instr::Instruction,
        value::Value,
    },
    lu_dog::{ValueType, ValueTypeEnum},
    new_ref, s_read,
    sarzak::Ty,
    NewRef, RefType, SarzakStorePtr, Span, POP_CLR,
};

const LIST_VAR: &str = "$$list_value";

#[tracing::instrument]
pub(in crate::bubba::compiler) fn compile(
    for_loop: &SarzakStorePtr,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    tracing::debug!(target: "instr", "{}\n  --> {}:{}:{}", POP_CLR.paint("compile_for_loop"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel().clone();
    let lu_dog = s_read!(lu_dog);

    let sarzak = context.sarzak_heel().clone();
    let sarzak = s_read!(sarzak);

    let for_loop = lu_dog.exhume_for_loop(for_loop).unwrap();
    let for_loop = s_read!(for_loop);
    let iter_ident = for_loop.ident.to_owned();
    let body = lu_dog.exhume_expression(&for_loop.block).unwrap();
    let list = lu_dog.exhume_expression(&for_loop.expression).unwrap();
    let list_span = get_span(&list, &lu_dog);

    // Whatever comes of this we expect two values on the stack. The first is
    // the end of the range, and the second is the start of the range.
    // This works as-is for range. For a list we need to do some extra work,
    // below.
    let ty = compile_expression(&list, thonk, context)?;
    // This unwrap is really going to become unnecessary as I plan on having
    // compile_expression return just a Result. I'm just being lazy now.
    let ty = ty.unwrap();

    context.push_scope();
    let mut inner_thonk = CThonk::new(format!("for_{}", iter_ident));
    let iter = format!("iter_{}", iter_ident);

    let int = context.get_type(INT).unwrap().clone();
    let iter_index = match context.insert_symbol(iter, int) {
        (true, index) => {
            inner_thonk.increment_frame_size();
            index
        }
        (false, index) => index,
    };

    let iter_ident_index = match ty.subtype {
        ValueTypeEnum::List(ref list) => {
            let list = lu_dog.exhume_list(list).unwrap();
            let list = s_read!(list);
            let ty = lu_dog.exhume_value_type(&list.ty).unwrap();
            let ty = s_read!(ty);

            match &ty.subtype {
                ValueTypeEnum::Enumeration(ref en) => {
                    let en = lu_dog.exhume_enumeration(en).unwrap();
                    let en = s_read!(en);
                    let ty = en.r1_value_type(&lu_dog)[0].clone();
                    let ty = (*s_read!(ty)).clone();
                    match context.insert_symbol(iter_ident, ty) {
                        (true, index) => {
                            inner_thonk.increment_frame_size();
                            index
                        }
                        (false, index) => index,
                    }
                }
                ValueTypeEnum::Ty(ref ty) => {
                    let ty = sarzak.exhume_ty(ty).unwrap();
                    let ty = ty.read().unwrap();

                    match &*ty {
                        Ty::Integer(_) => {
                            let int = context.get_type(INT).unwrap().clone();
                            match context.insert_symbol(iter_ident, int) {
                                (true, index) => {
                                    inner_thonk.increment_frame_size();
                                    index
                                }
                                (false, index) => index,
                            }
                        }
                        Ty::ZString(_) => {
                            let string = context.get_type(STRING).unwrap().clone();
                            match context.insert_symbol(iter_ident, string) {
                                (true, index) => {
                                    inner_thonk.increment_frame_size();
                                    index
                                }
                                (false, index) => index,
                            }
                        }
                        ty => todo!("list element ty: {:?}", ty),
                    }
                }
                ValueTypeEnum::XFuture(ref future) => {
                    let future = lu_dog.exhume_x_future(future).unwrap();
                    let future = s_read!(future);
                    let ty = future.r2_value_type(&lu_dog)[0].clone();
                    let ty = (*s_read!(ty)).clone();
                    match context.insert_symbol(iter_ident, ty) {
                        (true, index) => {
                            inner_thonk.increment_frame_size();
                            index
                        }
                        (false, index) => index,
                    }
                }
                ty => {
                    return Err(BubbaError::InternalCompilerError {
                        message: format!("For loop expression is not a list: {ty:?}"),
                        location: location!(),
                    }
                    .into());
                }
            }
        }
        ValueTypeEnum::Range(_) => {
            let int = context.get_type(INT).unwrap().clone();
            match context.insert_symbol(iter_ident, int) {
                (true, index) => {
                    inner_thonk.increment_frame_size();
                    index
                }
                (false, index) => index,
            }
        }
        _ => {
            return Err(BubbaError::InternalCompilerError {
                message: "For loop expression is not a list".to_owned(),
                location: location!(),
            }
            .into())
        }
    };

    // Store the starting value
    // Here's where that extra work comes in when iterating over a list.

    match ty.subtype {
        ValueTypeEnum::List(ref list) => {
            let list = lu_dog.exhume_list(list).unwrap();
            let list = s_read!(list);
            let ty = lu_dog.exhume_value_type(&list.ty).unwrap();
            let ty = s_read!(ty);

            let list_var_idx = match ty.subtype {
                ValueTypeEnum::Enumeration(ref en) => {
                    let en = lu_dog.exhume_enumeration(en).unwrap();
                    let en = s_read!(en);
                    let ty = en.r1_value_type(&lu_dog)[0].clone();
                    let ty = (*s_read!(ty)).clone();
                    match context.insert_symbol(LIST_VAR.into(), ty) {
                        (true, index) => {
                            inner_thonk.increment_frame_size();
                            index
                        }
                        (false, index) => index,
                    }
                }
                ValueTypeEnum::Ty(ref ty) => {
                    let ty = sarzak.exhume_ty(ty).unwrap();
                    let ty = ty.read().unwrap();

                    match &*ty {
                        Ty::Integer(_) => {
                            let int = context.get_type(INT).unwrap().clone();
                            match context.insert_symbol(LIST_VAR.into(), int) {
                                (true, index) => {
                                    inner_thonk.increment_frame_size();
                                    index
                                }
                                (false, index) => index,
                            }
                        }
                        Ty::ZString(_) => {
                            let string = context.get_type(STRING).unwrap().clone();
                            match context.insert_symbol(LIST_VAR.to_owned(), string) {
                                (true, index) => {
                                    inner_thonk.increment_frame_size();
                                    index
                                }
                                (false, index) => index,
                            }
                        }
                        ty => todo!("list element ty: {:?}", ty),
                    }
                }
                ValueTypeEnum::XFuture(ref future) => {
                    let future = lu_dog.exhume_x_future(future).unwrap();
                    let future = s_read!(future);
                    let ty = future.r2_value_type(&lu_dog)[0].clone();
                    let ty = (*s_read!(ty)).clone();
                    match context.insert_symbol(LIST_VAR.to_owned(), ty) {
                        (true, index) => {
                            inner_thonk.increment_frame_size();
                            index
                        }
                        (false, index) => index,
                    }
                }
                _ => {
                    return Err(BubbaError::InternalCompilerError {
                        message: "For loop expression is not a list".to_owned(),
                        location: location!(),
                    }
                    .into())
                }
            };

            thonk.insert_instruction_with_span(Instruction::Dup, list_span.clone(), location!());

            thonk.insert_instruction_with_span(
                Instruction::StoreLocal(list_var_idx),
                list_span.clone(),
                location!(),
            );

            // We need to create the stack with the top value being 0 and the
            // penultimate value being the length of the list.
            // The last instruction will store the starting value of the iteration
            // in the iterator.
            thonk.insert_instruction_with_span(
                Instruction::ListLength,
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::Push(new_ref!(Value, Value::Integer(0))),
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::StoreLocal(iter_index),
                span,
                location!(),
            );
        }
        ValueTypeEnum::Range(_) => {
            thonk.insert_instruction_with_span(
                Instruction::StoreLocal(iter_index),
                span,
                location!(),
            );
        }
        _ => {
            return Err(BubbaError::InternalCompilerError {
                message: "For loop expression is not a list".to_owned(),
                location: location!(),
            }
            .into())
        }
    }

    compile_expression(&body, &mut inner_thonk, context)?;
    for _ in 0..inner_thonk.get_frame_size() {
        thonk.increment_frame_size();
    }

    let top_of_loop = thonk.get_instruction_card() as isize;

    match ty.subtype {
        ValueTypeEnum::List(_) => {
            let list_var_idx = context.get_symbol(LIST_VAR).unwrap().number;
            thonk.insert_instruction_with_span(
                Instruction::FetchLocal(list_var_idx),
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::FetchLocal(iter_index),
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::ListIndex,
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::StoreLocal(iter_ident_index),
                list_span.clone(),
                location!(),
            );
        }
        ValueTypeEnum::Range(_) => {
            thonk.insert_instruction_with_span(
                Instruction::FetchLocal(iter_index),
                list_span.clone(),
                location!(),
            );
            thonk.insert_instruction_with_span(
                Instruction::StoreLocal(iter_ident_index),
                list_span.clone(),
                location!(),
            );
        }
        _ => {
            return Err(BubbaError::InternalCompilerError {
                message: "For loop expression is not a list".to_owned(),
                location: location!(),
            }
            .into())
        }
    }

    thonk.append(inner_thonk);

    // Duplicate the end so that we can compare against it.
    thonk.insert_instruction(Instruction::Dup, location!());

    // Increment the index
    thonk.insert_instruction(Instruction::FetchLocal(iter_index), location!());
    thonk.insert_instruction(
        Instruction::Push(new_ref!(Value, Value::Integer(1))),
        location!(),
    );
    thonk.insert_instruction(Instruction::Add, location!());
    thonk.insert_instruction(Instruction::Dup, location!());
    thonk.insert_instruction(Instruction::StoreLocal(iter_index), location!());

    // Test the index against the length of the list
    thonk.insert_instruction(Instruction::TestLessThanOrEqual, location!());

    // go do it again if index is < end.
    thonk.insert_instruction(
        Instruction::JumpIfFalse(top_of_loop - thonk.get_instruction_card() as isize - 1),
        location!(),
    );

    context.pop_scope();

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
    fn test_for_in_range() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       for i in 0..10 {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("test_for_in_range", ore).unwrap();
        let ctx = new_lu_dog(
            "test_for_in_range".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 21);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn nested_for_loop() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       for i in 0..10 {
                           for j in 0..10 {
                               x = x + i + j;
                           }
                       }
                       x
                   }";
        let ast = parse_dwarf("nested_for_loop", ore).unwrap();
        let ctx = new_lu_dog(
            "nested_for_loop".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 36);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(900));
    }

    #[test]
    fn for_loop_variable() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let max = 10;
                       let x = 0;
                       for i in 0..max {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("for_loop_variable", ore).unwrap();
        let ctx = new_lu_dog(
            "for_loop_variable".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 23);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn test_for_loop_iterator_param() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn add(x: int) -> int {
                       let accum = 0;
                       for i in 0..x {
                            accum = accum + i;
                       }
                       accum
                   }
                   fn main() -> int {
                    let x = 10;
                       add(x)
                   }";
        let ast = parse_dwarf("test_for_loop_iterator_param", ore).unwrap();
        let ctx = new_lu_dog(
            "test_for_loop_iterator_param".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 7);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn for_loop_iter_array_one() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 0;
                       let arr = [1, 2, 3, 4, 5];
                       for i in arr {
                           x = x + i;
                       }
                       x
                   }";
        let ast = parse_dwarf("for_loop_iter_array", ore).unwrap();
        let ctx = new_lu_dog(
            "for_loop_iter_array".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 34);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(15));
    }

    #[test]
    fn for_loop_iter_array_two() {
        setup_logging();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = r#"fn main() -> string {{
                       let x = "";
                       let arr = ["u", "b", "e", "r"];
                       for i in arr {{
                           x = x + i;
                       }}
                       x
                   }}"#;
        let ast = parse_dwarf("for_loop_iter_array_two", ore).unwrap();
        let ctx = new_lu_dog(
            "for_loop_iter_array".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 33);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("uber".to_owned())
        );
    }
}
