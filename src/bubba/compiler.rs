use std::path::PathBuf;

use crate::{
    bubba::{
        error::{Error, Result},
        instr::{Instruction, Program, Thonk},
    },
    chacha::value::ThonkInner,
    lu_dog::{
        BodyEnum, BooleanLiteralEnum, Expression, ExpressionEnum, Function, LiteralEnum, Statement,
        StatementEnum,
    },
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, NewRef, RefType, Value,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

pub fn compile(context: &ExtruderContext, sarzak: &SarzakStore) -> Result<Program> {
    let lu_dog = &context.lu_dog;

    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());

    for func in s_read!(lu_dog).iter_function() {
        program.add_thonk(compile_function(&func, context, sarzak)?);
    }

    Ok(program)
}

fn compile_function(
    func: &RefType<Function>,
    context: &ExtruderContext,
    sarzak: &SarzakStore,
) -> Result<Thonk> {
    let lu_dog = &context.lu_dog;

    let name = s_read!(func).name.clone();
    let mut thonk = Thonk::new(name.clone());

    let body = s_read!(func).r19_body(&s_read!(lu_dog))[0].clone();
    let body = s_read!(body);
    match body.subtype {
        //
        // This is a function defined in a dwarf file.
        BodyEnum::Block(ref id) => {
            let block = s_read!(lu_dog).exhume_block(id).unwrap();
            let has_stmts = !s_read!(block).r18_statement(&s_read!(lu_dog)).is_empty();

            if has_stmts {
                if let Some(ref id) = s_read!(block).statement {
                    let mut next = s_read!(lu_dog).exhume_statement(id).unwrap();

                    loop {
                        compile_statement(&next, &mut thonk, context, sarzak)?;

                        if let Some(ref id) = s_read!(next.clone()).next {
                            next = s_read!(lu_dog).exhume_statement(id).unwrap();
                        } else {
                            thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Empty)));
                            thonk.add_instruction(Instruction::Return);
                            break;
                        }
                    }
                }
            } else {
                thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Empty)));
                thonk.add_instruction(Instruction::Return);
            }
        }
        //
        // This is an externally defined function that was declared in a dwarf file.
        BodyEnum::ExternalImplementation(ref id) => {
            panic!("Somehow we found ourselves trying to compile an external implementation. This should not happen. The function name is: {name}");
        }
    };

    Ok(thonk)
}

fn compile_statement(
    statement: &RefType<Statement>,
    thonk: &mut Thonk,
    context: &ExtruderContext,
    sarzak: &SarzakStore,
) -> Result<()> {
    let lu_dog = &context.lu_dog;

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&s_read!(lu_dog))[0].clone();
            let _value = compile_expression(&expr, thonk, context, sarzak)?;
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r20_expression(&s_read!(lu_dog))[0].clone();

            let value = compile_expression(&expr, thonk, context, sarzak)?;

            let var = s_read!(stmt.r21_local_variable(&s_read!(lu_dog))[0]).clone();
            let var = s_read!(var.r12_variable(&s_read!(lu_dog))[0]).clone();

            // context.memory().insert(var.name, value);
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r41_expression(&s_read!(lu_dog))[0].clone();

            let value = compile_expression(&expr, thonk, context, sarzak)?;
        }
        StatementEnum::ItemStatement(_) => {}
    }
    Ok(())
}

fn compile_expression(
    expression: &RefType<Expression>,
    thonk: &mut Thonk,
    context: &ExtruderContext,
    sarzak: &SarzakStore,
) -> Result<()> {
    let lu_dog = &context.lu_dog;

    match &s_read!(expression).subtype {
        ExpressionEnum::Call(ref call) => {
            let call = s_read!(lu_dog).exhume_call(call).unwrap();
            let first_arg = s_read!(call).argument;
            let mut args = s_read!(call).r28_argument(&s_read!(lu_dog));

            if let Some(ref expr) = s_read!(call).expression {
                let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the underlying value/instance.
                compile_expression(&expr, thonk, context, sarzak)?;
            };
            thonk.add_instruction(Instruction::Call(0));
            thonk.add_instruction(Instruction::Pop);
        }
        ExpressionEnum::Literal(ref literal) => {
            let lu_dog = &context.lu_dog;

            let literal = s_read!(lu_dog).exhume_literal(literal).unwrap();

            let literal = match &s_read!(literal).subtype {
                //
                // BooleanLiteral
                //
                LiteralEnum::BooleanLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_boolean_literal(literal).unwrap();
                    let literal = s_read!(literal);

                    match literal.subtype {
                        BooleanLiteralEnum::FalseLiteral(_) => {
                            Ok::<RefType<Value>, Error>(new_ref!(Value, Value::Boolean(false,)))
                        }
                        BooleanLiteralEnum::TrueLiteral(_) => {
                            Ok(new_ref!(Value, Value::Boolean(true,)))
                        }
                    }
                }
                //
                // FloatLiteral
                //
                LiteralEnum::FloatLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_float_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Float(value);
                    Ok(new_ref!(Value, value))
                }
                //
                // IntegerLiteral
                //
                LiteralEnum::IntegerLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_integer_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Integer(value);
                    Ok(new_ref!(Value, value))
                }
                //
                // StringLiteral
                //
                LiteralEnum::StringLiteral(ref literal) => {
                    let literal = s_read!(lu_dog).exhume_string_literal(literal).unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value = Value::String(s_read!(literal).x_value.clone());
                    Ok(new_ref!(Value, value))
                }
            };

            thonk.add_instruction(Instruction::Push(literal?));
        }
        ExpressionEnum::VariableExpression(ref expr) => {
            let expr = s_read!(lu_dog).exhume_variable_expression(expr).unwrap();
            let expr = s_read!(expr);
            let name = expr.name.clone();

            // ATM we are here because we need to look up a function. Somehow
            // we'll need to differentiate between a function and a variable.
            thonk.add_instruction(Instruction::Push(new_ref!(
                Value,
                Value::Thonk(ThonkInner::Thonk(name))
            )));
        }
        ExpressionEnum::XPrint(ref print) => {
            let print = s_read!(lu_dog).exhume_x_print(print).unwrap();
            let expr = s_read!(print).r32_expression(&s_read!(lu_dog))[0].clone();

            compile_expression(&expr, thonk, context, sarzak)?;
            thonk.add_instruction(Instruction::Out(0));
        }
        missed => {
            panic!("Missed: {:?}", missed);
        }
    }

    Ok(())
}

mod test {
    use std::env;

    use super::*;

    use crate::{
        bubba::{CallFrame, VM},
        chacha::{error::ChaChaError, memory::Memory},
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    fn get_dwarf_home() -> PathBuf {
        env::var("DWARF_HOME")
            .unwrap_or_else(|_| {
                let mut home = env::var("HOME").unwrap();
                home.push_str("/.dwarf");
                home
            })
            .into()
    }

    // 🚧 This nastiness needs to be fixed. It's not cool that we are doing all
    // this work here.
    fn run_vm(program: &Program) -> Result<RefType<Value>, ChaChaError> {
        let mut memory = Memory::new();
        for thonk in program.iter() {
            // 🚧 This memory thing is BS. Fix it.
            let slot = memory.0.reserve_thonk_slot();
            memory.0.insert_thonk(thonk.clone(), slot);
        }
        let mut vm = VM::new(&memory.0);
        let mut frame = CallFrame::new(0, 0, &program.get_thonk("main").unwrap());
        vm.run(&mut frame, true)
    }

    #[test]
    fn empty_func() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn empty_funcs() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {}
                   fn foo() {}
                   fn bar() {}";
        let ast = parse_dwarf("empty_func", ore).unwrap();
        let ctx = new_lu_dog(
            "empty_func".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        assert_eq!(program.get_thonk_card(), 3);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 2);
        assert_eq!(program.get_thonk("bar").unwrap().get_instruction_card(), 2);

        run_vm(&program).unwrap();
    }

    #[test]
    fn print_hello_world() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       print(\"Hello, world!\");
                   }";
        let ast = parse_dwarf("print_hello_world", ore).unwrap();
        let ctx = new_lu_dog(
            "print_hello_world".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn func_call() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       foo();
                   }
                   fn foo() {
                       print(\"Hello, world!\");
                   }";
        let ast = parse_dwarf("func_call", ore).unwrap();
        let ctx = new_lu_dog(
            "func_call".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx, &sarzak).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        // assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 5);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 4);

        run_vm(&program).unwrap();
    }
}
