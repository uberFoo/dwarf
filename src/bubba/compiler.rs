use std::path::PathBuf;

use rustc_hash::FxHashMap as HashMap;

use crate::{
    bubba::{
        error::{Error, Result},
        instr::{Instruction, Program, Thonk},
    },
    chacha::value::ThonkInner,
    lu_dog::{
        BodyEnum, BooleanLiteralEnum, Expression, ExpressionEnum, Function, LiteralEnum,
        ObjectStore as LuDogStore, Statement, StatementEnum,
    },
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, NewRef, RefType, Value,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

struct Context<'a> {
    extruder_context: &'a ExtruderContext,
    symbol_tables: Vec<(usize, HashMap<String, usize>)>,
}

impl<'a> Context<'a> {
    fn new(extruder_context: &'a ExtruderContext) -> Self {
        Context {
            extruder_context,
            symbol_tables: vec![(0, HashMap::default())],
        }
    }

    fn lu_dog_heel(&self) -> RefType<LuDogStore> {
        self.extruder_context.lu_dog.clone()
    }

    fn push_symbol_table(&mut self) {
        self.symbol_tables.push((0, HashMap::default()));
    }

    fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop();
    }

    fn insert_symbol(&mut self, name: String) {
        let (next, map) = self.symbol_tables.last_mut().unwrap();
        map.insert(name, *next);
        *next += 1;
    }

    fn get_symbol(&self, name: &str) -> Option<usize> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.1.get(name) {
                return Some(*value);
            }
        }
        None
    }
}

pub fn compile(context: &ExtruderContext) -> Result<Program> {
    let lu_dog = &context.lu_dog;

    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());

    let mut context = Context::new(context);

    for func in s_read!(lu_dog).iter_function() {
        program.add_thonk(compile_function(&func, &mut context)?);
    }

    Ok(program)
}

fn compile_function(func: &RefType<Function>, context: &mut Context) -> Result<Thonk> {
    let lu_dog = context.lu_dog_heel();

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
                        compile_statement(&next, &mut thonk, context)?;

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
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&s_read!(lu_dog))[0].clone();
            let _value = compile_expression(&expr, thonk, context)?;
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r20_expression(&s_read!(lu_dog))[0].clone();

            compile_expression(&expr, thonk, context)?;

            let var = s_read!(stmt.r21_local_variable(&s_read!(lu_dog))[0]).clone();
            let var = s_read!(var.r12_variable(&s_read!(lu_dog))[0]).clone();

            let name = var.name;
            context.insert_symbol(name.clone());

            let offset = context.get_symbol(&name).unwrap();

            thonk.add_instruction(Instruction::PopLocal(offset));
            thonk.increment_frame_size();
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = s_read!(lu_dog).exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r41_expression(&s_read!(lu_dog))[0].clone();

            compile_expression(&expr, thonk, context)?;

            thonk.add_instruction(Instruction::Return);
        }
        StatementEnum::ItemStatement(_) => {}
    }
    Ok(())
}

fn compile_expression(
    expression: &RefType<Expression>,
    thonk: &mut Thonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();

    match &s_read!(expression).subtype {
        ExpressionEnum::Call(ref call) => {
            let call = s_read!(lu_dog).exhume_call(call).unwrap();
            let first_arg = s_read!(call).argument;
            let mut args = s_read!(call).r28_argument(&s_read!(lu_dog));

            if let Some(ref expr) = s_read!(call).expression {
                let expr = s_read!(lu_dog).exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the underlying value/instance.
                compile_expression(&expr, thonk, context)?;
            };
            thonk.add_instruction(Instruction::Call(0));
            thonk.add_instruction(Instruction::Pop);
        }
        ExpressionEnum::Literal(ref literal) => {
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

            if let Some(index) = context.get_symbol(&name) {
                thonk.add_instruction(Instruction::PushLocal(index));
            } else {
                // ATM we are here because we need to look up a function. Somehow
                // we'll need to differentiate between a function and a variable.
                thonk.add_instruction(Instruction::Push(new_ref!(
                    Value,
                    Value::Thonk(ThonkInner::Thonk(name))
                )));
            }
        }
        ExpressionEnum::XPrint(ref print) => {
            let print = s_read!(lu_dog).exhume_x_print(print).unwrap();
            let expr = s_read!(print).r32_expression(&s_read!(lu_dog))[0].clone();

            compile_expression(&expr, thonk, context)?;
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
        let thonk = program.get_thonk("main").unwrap();
        dbg!(thonk.get_frame_size());
        let mut frame = CallFrame::new(&thonk);

        vm.push_stack(new_ref!(
            Value,
            Value::Thonk(ThonkInner::Thonk("main".to_owned()))
        ));
        for _ in 0..thonk.get_frame_size() {
            vm.push_stack(new_ref!(Value, Value::Empty));
        }
        vm.set_fp(thonk.get_frame_size() + 1);
        vm.push_stack(new_ref!(Value, Value::Empty));

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
        let program = compile(&ctx).unwrap();

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
        let program = compile(&ctx).unwrap();

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
        let program = compile(&ctx).unwrap();

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
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 5);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn test_let_statement() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let z = 1;
                       let x = 5;
                       let y = 10;
                       x
                   }";
        let ast = parse_dwarf("test_let_statement", ore).unwrap();
        let ctx = new_lu_dog(
            "test_let_statement".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            10
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(5));
    }
}
