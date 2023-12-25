use std::path::PathBuf;

use ansi_term::Colour;
use rustc_hash::FxHashMap as HashMap;
use snafu::prelude::*;

use crate::{
    bubba::instr::{Instruction, Program, Thonk},
    chacha::value::ThonkInner,
    lu_dog::{
        BodyEnum, Expression, ExpressionEnum, Function, ObjectStore as LuDogStore, Statement,
        StatementEnum,
    },
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, NewRef, RefType, Value,
};

mod expression;

use expression::{
    block, call, for_loop, if_expr, literal, operator, print, range, variable, xmatch,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

const _ERR_CLR: Colour = Colour::Red;
const _OK_CLR: Colour = Colour::Green;
const _POP_CLR: Colour = Colour::Yellow;
const _OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {}

type Result<T, E = Error> = std::result::Result<T, E>;

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

#[derive(Debug)]
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
        match self.get_symbol(name.as_str()) {
            Some(value) => value,
            None => {
                let (next, map) = self.symbol_tables.last_mut().unwrap();
                map.insert(name, *next);
                let value = *next;
                *next += 1;
                value
            }
        }
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
                        } else if thonk.returned {
                            break;
                        } else {
                            thonk.add_instruction(Instruction::Push(new_ref!(Value, Value::Empty)));
                            thonk.add_instruction(Instruction::Return);
                            thonk.returned = true;
                            break;
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
        BodyEnum::ExternalImplementation(ref _id) => {
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
    match &s_read!(expression).subtype {
        ExpressionEnum::Block(ref block) => block::compile(block, thonk, context)?,
        ExpressionEnum::Call(ref call) => call::compile(call, thonk, context)?,
        ExpressionEnum::ForLoop(ref for_loop) => for_loop::compile(for_loop, thonk, context)?,
        ExpressionEnum::Literal(ref literal) => literal::compile(literal, thonk, context)?,
        ExpressionEnum::Operator(ref op_type) => operator::compile(op_type, thonk, context)?,
        ExpressionEnum::RangeExpression(ref range) => range::compile(range, thonk, context)?,
        ExpressionEnum::VariableExpression(ref expr) => variable::compile(expr, thonk, context)?,
        ExpressionEnum::XIf(ref expr) => if_expr::compile(expr, thonk, context)?,
        ExpressionEnum::XMatch(ref expr) => xmatch::compile(expr, thonk, context)?,
        ExpressionEnum::XPrint(ref print) => print::compile(print, thonk, context)?,
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
        let mut frame = CallFrame::new(thonk);

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
        let program = compile(&ctx).unwrap();

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
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            19
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(45));
    }

    #[test]
    fn nested_for_loop() {
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
        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            32
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(900));
    }

    #[test]
    fn if_expression_true_arm() {
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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            10
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn if_expression_false_arm() {
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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            10
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(2));
    }

    #[test]
    fn if_expression_complex() {
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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            65
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn fibonacci() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       fib(10)
                   }
                   fn fib(n: int) -> int {
                       if n == 0 {
                           0
                       } else if n == 1 {
                           1
                       } else {
                           fib(n - 1) + fib(n - 2)
                       }
                   }";
        let ast = parse_dwarf("fibonacci", ore).unwrap();
        let ctx = new_lu_dog(
            "fibonacci".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 2);

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 4);

        assert_eq!(program.get_thonk("fib").unwrap().get_instruction_card(), 31);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(55));
    }

    // #[test]
    fn match_expression() {
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 1 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           _ => 4,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
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

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }
}