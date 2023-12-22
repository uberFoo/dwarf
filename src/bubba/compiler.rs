use std::path::PathBuf;

use rustc_hash::FxHashMap as HashMap;

use crate::{
    bubba::{
        error::{Error, Result},
        instr::{Instruction, Program, Thonk},
    },
    chacha::value::ThonkInner,
    lu_dog::{
        BinaryEnum, BodyEnum, BooleanLiteralEnum, CallEnum, Expression, ExpressionEnum, Function,
        LiteralEnum, ObjectStore as LuDogStore, OperatorEnum, Statement, StatementEnum,
    },
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, NewRef, RefType, Value,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

struct CThonk {
    inner: Thonk,
    returned: bool,
}

impl CThonk {
    fn new(name: String) -> Self {
        CThonk {
            inner: Thonk::new(name),
            returned: false,
        }
    }

    fn add_instruction(&mut self, instruction: Instruction) {
        self.inner.add_instruction(instruction);
    }

    fn get_instruction_card(&self) -> usize {
        self.inner.get_instruction_card()
    }

    fn increment_frame_size(&mut self) {
        self.inner.increment_frame_size();
    }

    fn get_frame_size(&self) -> usize {
        self.inner.get_frame_size()
    }

    fn append(&mut self, other: CThonk) {
        self.inner.instructions.extend(other.inner.instructions);
    }
}

impl From<CThonk> for Thonk {
    fn from(thonk: CThonk) -> Self {
        thonk.inner
    }
}

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
        let (start, _) = self.symbol_tables.last().unwrap();
        self.symbol_tables.push((*start, HashMap::default()));
    }

    fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop();
    }

    fn insert_symbol(&mut self, name: String) -> usize {
        let (next, map) = self.symbol_tables.last_mut().unwrap();
        map.insert(name, *next);
        *next += 1;
        *next - 1
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
    let lu_dog = s_read!(lu_dog);

    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());

    let mut context = Context::new(context);

    for func in lu_dog.iter_function() {
        program.add_thonk(compile_function(&func, &mut context)?.into());
    }

    Ok(program)
}

fn compile_function(func: &RefType<Function>, context: &mut Context) -> Result<CThonk> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let name = s_read!(func).name.clone();
    let mut thonk = CThonk::new(name.clone());

    let body = s_read!(func).r19_body(&lu_dog)[0].clone();
    let body = s_read!(body);
    match body.subtype {
        //
        // This is a function defined in a dwarf file.
        BodyEnum::Block(ref id) => {
            let block = lu_dog.exhume_block(id).unwrap();
            let has_stmts = !s_read!(block).r18_statement(&lu_dog).is_empty();

            if has_stmts {
                if let Some(ref id) = s_read!(block).statement {
                    let mut next = lu_dog.exhume_statement(id).unwrap();

                    loop {
                        compile_statement(&next, &mut thonk, context)?;

                        if let Some(ref id) = s_read!(next.clone()).next {
                            next = lu_dog.exhume_statement(id).unwrap();
                        } else {
                            if thonk.returned {
                                break;
                            } else {
                                thonk.add_instruction(Instruction::Push(new_ref!(
                                    Value,
                                    Value::Empty
                                )));
                                thonk.add_instruction(Instruction::Return);
                                thonk.returned = true;
                                break;
                            }
                        }
                    }
                }
            } else {
                thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Empty)));
                thonk.add_instruction(Instruction::Return);
                thonk.returned = true;
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
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog.exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&lu_dog)[0].clone();
            compile_expression(&expr, thonk, context)?;
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r20_expression(&lu_dog)[0].clone();

            compile_expression(&expr, thonk, context)?;

            let var = s_read!(stmt.r21_local_variable(&lu_dog)[0]).clone();
            let var = s_read!(var.r12_variable(&lu_dog)[0]).clone();

            let name = var.name;
            let offset = context.insert_symbol(name.clone());

            thonk.add_instruction(Instruction::StoreLocal(offset));
            thonk.increment_frame_size();
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog.exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r41_expression(&lu_dog)[0].clone();
            compile_expression(&expr, thonk, context)?;

            thonk.add_instruction(Instruction::Return);
            thonk.returned = true;
        }
        StatementEnum::ItemStatement(_) => {}
    }
    Ok(())
}

fn compile_expression(
    expression: &RefType<Expression>,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match &s_read!(expression).subtype {
        ExpressionEnum::Block(ref block) => {
            let block = lu_dog.exhume_block(block).unwrap();
            let stmts = s_read!(block).r18_statement(&lu_dog);
            if !stmts.is_empty() {
                let mut next = s_read!(block).r71_statement(&lu_dog)[0].clone();

                loop {
                    compile_statement(&next, thonk, context)?;

                    if let Some(ref id) = s_read!(next.clone()).next {
                        next = lu_dog.exhume_statement(id).unwrap();
                    } else {
                        break;
                    }
                }
            }
        }
        ExpressionEnum::Call(ref call) => {
            let call = lu_dog.exhume_call(call).unwrap();
            let call = s_read!(call);
            let first_arg = call.argument;
            let args = call.r28_argument(&lu_dog);

            let name = if let CallEnum::FunctionCall(ref call) = call.subtype {
                let call = lu_dog.exhume_function_call(call).unwrap();
                let call = s_read!(call);
                let name = call.name.clone();
                name
            } else {
                todo!("handle the other calls");
            };

            let func = lu_dog.exhume_function_id_by_name(&name).unwrap();
            let func = lu_dog.exhume_function(&func).unwrap();
            let func = s_read!(func);
            let params = func.r13_parameter(&lu_dog);

            let params = if !params.is_empty() {
                let mut params = Vec::with_capacity(params.len());
                let mut next = func
                    .r13_parameter(&lu_dog)
                    .iter()
                    .find(|p| s_read!(p).r14c_parameter(&lu_dog).is_empty())
                    .unwrap()
                    .clone();

                loop {
                    // Apparently I'm being clever. I don't typecheck against an actual
                    // type associated with the parameter. No, I am looking up the variable
                    // associated with the parameter and using it's type. I guess that's cool,
                    // but it's tricky if you aren't aware.
                    let var = s_read!(s_read!(next).r12_variable(&lu_dog)[0]).clone();
                    let value = s_read!(var.r11_x_value(&lu_dog)[0]).clone();
                    let ty = value.r24_value_type(&lu_dog)[0].clone();
                    params.push(var.name.clone());

                    let next_id = { s_read!(next).next };
                    if let Some(ref id) = next_id {
                        next = lu_dog.exhume_parameter(id).unwrap();
                    } else {
                        break;
                    }
                }

                params
            } else {
                Vec::new()
            };

            let arg_exprs = if let Some(next) = first_arg {
                let mut exprs = Vec::with_capacity(args.len());
                let mut next = lu_dog.exhume_argument(&next).unwrap();

                loop {
                    let expr = s_read!(next).r37_expression(&lu_dog)[0].clone();
                    exprs.push(expr);

                    let next_id = { s_read!(next).next };
                    if let Some(ref id) = next_id {
                        next = lu_dog.exhume_argument(id).unwrap();
                    } else {
                        break;
                    }
                }

                exprs
            } else {
                Vec::new()
            };

            if let Some(ref expr) = call.expression {
                let expr = lu_dog.exhume_expression(expr).unwrap();
                // Evaluate the LHS to get at the underlying value/instance.
                compile_expression(&expr, thonk, context)?;
            };

            for (name, expr) in params.into_iter().zip(arg_exprs) {
                compile_expression(&expr, thonk, context)?;
                context.insert_symbol(name);
            }

            thonk.add_instruction(Instruction::Call(args.len()));
        }
        ExpressionEnum::ForLoop(ref for_loop) => {
            let for_loop = lu_dog.exhume_for_loop(for_loop).unwrap();
            let for_loop = s_read!(for_loop);
            let ident = for_loop.ident.to_owned();
            let body = lu_dog.exhume_expression(&for_loop.block).unwrap();
            let list = lu_dog.exhume_expression(&for_loop.expression).unwrap();

            compile_expression(&list, thonk, context)?;

            context.push_symbol_table();
            let mut inner_thonk = CThonk::new(format!("for_{}", ident));

            inner_thonk.increment_frame_size();
            let index = context.insert_symbol(ident.clone());
            compile_expression(&body, &mut inner_thonk, context)?;
            let fp = inner_thonk.get_frame_size();
            for _ in 0..fp {
                thonk.increment_frame_size();
            }

            // Store the starting value
            thonk.add_instruction(Instruction::StoreLocal(index));

            let top_of_loop = thonk.get_instruction_card() as isize;

            thonk.append(inner_thonk);

            // Duplicate the range end so that we can compare against it.
            thonk.add_instruction(Instruction::Dup);

            // Increment the index
            thonk.add_instruction(Instruction::FetchLocal(index));
            thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Integer(1))));
            thonk.add_instruction(Instruction::Add);
            thonk.add_instruction(Instruction::Dup);
            thonk.add_instruction(Instruction::StoreLocal(index));

            // Test the index against the length of the list
            thonk.add_instruction(Instruction::TestLessThanOrEqual);

            // go do it again if index is < end.
            thonk.add_instruction(Instruction::JumpIfFalse(
                top_of_loop - thonk.get_instruction_card() as isize - 1,
            ));

            context.pop_symbol_table();
        }
        ExpressionEnum::Literal(ref literal) => {
            let literal = lu_dog.exhume_literal(literal).unwrap();

            let literal = match &s_read!(literal).subtype {
                //
                // BooleanLiteral
                //
                LiteralEnum::BooleanLiteral(ref literal) => {
                    let literal = lu_dog.exhume_boolean_literal(literal).unwrap();
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
                    let literal = lu_dog.exhume_float_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Float(value);
                    Ok(new_ref!(Value, value))
                }
                //
                // IntegerLiteral
                //
                LiteralEnum::IntegerLiteral(ref literal) => {
                    let literal = lu_dog.exhume_integer_literal(literal).unwrap();
                    let value = s_read!(literal).x_value;
                    let value = Value::Integer(value);
                    Ok(new_ref!(Value, value))
                }
                //
                // StringLiteral
                //
                LiteralEnum::StringLiteral(ref literal) => {
                    let literal = lu_dog.exhume_string_literal(literal).unwrap();
                    // 🚧 It'd be great if this were an Rc...
                    let value = Value::String(s_read!(literal).x_value.clone());
                    Ok(new_ref!(Value, value))
                }
            };

            thonk.add_instruction(Instruction::Push(literal?));
        }
        ExpressionEnum::Operator(ref op_type) => {
            let operator = lu_dog.exhume_operator(op_type).unwrap();
            let operator = s_read!(operator);
            let lhs = lu_dog.exhume_expression(&operator.lhs).unwrap();

            match operator.subtype {
                OperatorEnum::Binary(ref op_type) => {
                    let binary = lu_dog.exhume_binary(op_type).unwrap();
                    let binary = s_read!(binary);
                    let rhs = lu_dog.exhume_expression(&operator.rhs.unwrap()).unwrap();

                    match binary.subtype {
                        BinaryEnum::Addition(_) => {
                            compile_expression(&lhs, thonk, context)?;
                            compile_expression(&rhs, thonk, context)?;
                            thonk.add_instruction(Instruction::Add);
                        }
                        BinaryEnum::Assignment(_) => {
                            let offset = if let ExpressionEnum::VariableExpression(ref expr) =
                                &s_read!(lhs).subtype
                            {
                                let expr = lu_dog.exhume_variable_expression(expr).unwrap();
                                let expr = s_read!(expr);
                                context.get_symbol(&expr.name).expect(
                                    format!("symbol lookup failed for {}", expr.name).as_ref(),
                                )
                            } else {
                                panic!("In assignment and lhs is not a variable.")
                            };

                            compile_expression(&rhs, thonk, context)?;
                            thonk.add_instruction(Instruction::StoreLocal(offset));
                        }
                        BinaryEnum::BooleanOperator(_) => {
                            todo!("BooleanOperator")
                        }
                        BinaryEnum::Division(_) => {
                            compile_expression(&lhs, thonk, context)?;
                            compile_expression(&rhs, thonk, context)?;
                            thonk.add_instruction(Instruction::Divide);
                        }
                        BinaryEnum::Subtraction(_) => {
                            compile_expression(&lhs, thonk, context)?;
                            compile_expression(&rhs, thonk, context)?;
                            thonk.add_instruction(Instruction::Subtract);
                        }
                        BinaryEnum::Multiplication(_) => {
                            compile_expression(&lhs, thonk, context)?;
                            compile_expression(&rhs, thonk, context)?;
                            thonk.add_instruction(Instruction::Multiply);
                        }
                    }
                }
                OperatorEnum::Comparison(ref op_type) => {
                    let op_type = lu_dog.exhume_comparison(op_type).unwrap();
                    let op_type = s_read!(op_type);

                    match &op_type.subtype {
                        _ => todo!(),
                    }
                }
                OperatorEnum::Unary(ref id) => {
                    todo!("unary");
                }
            }
        }
        ExpressionEnum::RangeExpression(ref range) => {
            let range = lu_dog.exhume_range_expression(range).unwrap();
            let range = s_read!(range);

            let start = lu_dog.exhume_expression(&range.lhs.unwrap()).unwrap();
            let end = lu_dog.exhume_expression(&range.rhs.unwrap()).unwrap();

            // We push first the end, then the start onto the stack.
            compile_expression(&end, thonk, context)?;
            compile_expression(&start, thonk, context)?;
        }
        ExpressionEnum::VariableExpression(ref expr) => {
            let expr = lu_dog.exhume_variable_expression(expr).unwrap();
            let expr = s_read!(expr);
            let name = expr.name.clone();

            if let Some(index) = context.get_symbol(&name) {
                thonk.add_instruction(Instruction::FetchLocal(index));
            } else {
                // We are here because we need to look up a function.
                thonk.add_instruction(Instruction::Push(new_ref!(
                    Value,
                    Value::Thonk(ThonkInner::Thonk(name))
                )));
            }
        }
        ExpressionEnum::XPrint(ref print) => {
            let print = lu_dog.exhume_x_print(print).unwrap();
            let expr = s_read!(print).r32_expression(&lu_dog)[0].clone();

            compile_expression(&expr, thonk, context)?;
            thonk.add_instruction(Instruction::Out(0));
        }
        missed => {
            panic!("Implement: {:?}", missed);
        }
    }

    Ok(())
}

mod test {
    use std::env;

    use super::*;

    #[allow(unused_imports)]
    use crate::{
        bubba::{vm::Error, CallFrame, VM},
        chacha::{error::ChaChaError, memory::Memory},
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
    };

    #[allow(dead_code)]
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
    #[allow(dead_code)]
    fn run_vm(program: &Program) -> Result<RefType<Value>, Error> {
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

        vm.run(0, &mut frame, true)
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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 4);

        run_vm(&program).unwrap();
    }

    #[test]
    fn test_let_statements() {
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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 8);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(5));
    }

    #[test]
    fn test_func_args() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) -> int {
                       x + y + z
                   }";
        let ast = parse_dwarf("test_func_args", ore).unwrap();
        let ctx = new_lu_dog(
            "test_func_args".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 6);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(6));
    }

    #[test]
    fn test_func_args_and_locals() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) -> int {
                       let a = 1;
                       let b = 2;
                       let c = 3;
                       x + y + z + a + b + c
                   }";
        let ast = parse_dwarf("test_func_args_and_locals", ore).unwrap();
        let ctx = new_lu_dog(
            "test_func_args_and_locals".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 6);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 18);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(12));
    }

    // #[test]
    fn test_argument_ordering() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() {
                       foo(1, 2, 3)
                   }
                   fn foo(x: int, y: int, z: int) {
                       chacha::assert_eq(x, 1);
                       chacha::assert_eq(y, 2);
                       chacha::assert_eq(z, 3);
                   }";
        let ast = parse_dwarf("test_argument_ordering", ore).unwrap();
        let ctx = new_lu_dog(
            "test_argument_ordering".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let mut program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 6);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 18);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(12));
    }

    #[test]
    fn test_add_strings() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> string {
                       \"Hello, \" + \"world!\"
                   }";
        let ast = parse_dwarf("test_add_strings", ore).unwrap();
        let ctx = new_lu_dog(
            "test_add_strings".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("Hello, world!".to_owned())
        );
    }

    #[test]
    fn test_subtraction() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 - 2
                   }";
        let ast = parse_dwarf("test_subtraction", ore).unwrap();
        let ctx = new_lu_dog(
            "test_subtraction".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(3));
    }

    #[test]
    fn test_multiplication() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 * 2
                   }";
        let ast = parse_dwarf("test_multiplication", ore).unwrap();
        let ctx = new_lu_dog(
            "test_multiplication".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn test_division() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       5 / 2
                   }";
        let ast = parse_dwarf("test_division", ore).unwrap();
        let ctx = new_lu_dog(
            "test_division".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(2));
    }

    #[test]
    fn test_boolean_true() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> bool {
                       true
                   }";
        let ast = parse_dwarf("test_boolean_true", ore).unwrap();
        let ctx = new_lu_dog(
            "test_boolean_true".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Boolean(true));
    }

    #[test]
    fn test_boolean_false() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> bool {
                       false
                   }";
        let ast = parse_dwarf("test_boolean_false", ore).unwrap();
        let ctx = new_lu_dog(
            "test_boolean_false".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 2);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Boolean(false));
    }

    #[test]
    fn test_assignment() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "fn main() -> int {
                       let x = 5;
                       x = 10;
                       x
                   }";
        let ast = parse_dwarf("test_assignment", ore).unwrap();
        let ctx = new_lu_dog(
            "test_assignment".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();

        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(10));
    }

    #[test]
    fn test_for_in_range() {
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
        // assert_eq!(
        // program.get_thonk("main").unwrap().get_instruction_card(),
        // 16
        // );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }
}
