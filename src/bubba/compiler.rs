use log::{self, log_enabled, Level::Trace};
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, prelude::*, Location};

use crate::{
    bubba::instr::{Instruction, Program, Thonk},
    lu_dog::{
        BodyEnum, Expression, ExpressionEnum, Function, ObjectStore as LuDogStore, Statement,
        StatementEnum, ValueType,
    },
    new_ref, s_read,
    sarzak::ObjectStore as SarzakStore,
    Context as ExtruderContext, NewRef, RefType, Span, Value, ERR_CLR, POP_CLR,
};

mod expression;

use expression::{
    block, call, field, for_loop, if_expr, index, list, literal, operator, print, range, ret,
    struct_expr, typecast, variable, xmatch,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {
    #[snafu(display("\n{}: `{message}`\n  --> {}::{}::{}", ERR_CLR.bold().paint("error"), location.file, location.line, location.column))]
    InternalCompilerError { location: Location, message: String },
}

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

    fn add_instruction(&mut self, instruction: Instruction, location: Location) {
        log::debug!(target: "instr", "{}: {}:{}:{}\n{instruction}", POP_CLR.paint("add_instruction"), location.file, location.line, location.column);

        if log_enabled!(target: "instr", Trace) {
            self.inner.add_instruction(
                Instruction::Comment(new_ref!(
                    String,
                    format!("{}:{}:{}", location.file, location.line, location.column)
                )),
                None,
            );
        }
        self.inner.add_instruction(instruction, None);
    }

    fn add_instruction_with_span(
        &mut self,
        instruction: Instruction,
        span: Span,
        location: Location,
    ) {
        log::debug!(target: "instr", "{}: {}:{}:{}\n{instruction}", POP_CLR.paint("add_instruction"), location.file, location.line, location.column);

        if log_enabled!(target: "instr", Trace) {
            self.inner.add_instruction(
                Instruction::Comment(new_ref!(
                    String,
                    format!("{}:{}:{}", location.file, location.line, location.column)
                )),
                None,
            );
        }
        self.inner.add_instruction(instruction, Some(span));
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
struct Symbol {
    number: usize,
    ty: ValueType,
}

#[derive(Debug)]
struct SymbolTable {
    start: usize,
    map: HashMap<String, Symbol>,
}

impl SymbolTable {
    fn new(start: usize) -> Self {
        SymbolTable {
            start,
            map: HashMap::default(),
        }
    }

    fn insert(&mut self, name: String, ty: ValueType) -> usize {
        let number = self.count();
        self.map.insert(
            name,
            Symbol {
                number,
                ty: ty.clone(),
            },
        );
        number
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        self.map.get(name)
    }

    fn count(&self) -> usize {
        self.map.len() + self.start
    }
}

#[derive(Debug)]
struct Context<'a> {
    extruder_context: &'a ExtruderContext,
    symbol_tables: Vec<SymbolTable>,
    method_name: Option<String>,
}

impl<'a> Context<'a> {
    fn new(extruder_context: &'a ExtruderContext) -> Self {
        Context {
            extruder_context,
            symbol_tables: vec![SymbolTable::new(0)],
            method_name: None,
        }
    }

    fn lu_dog_heel(&self) -> RefType<LuDogStore> {
        self.extruder_context.lu_dog.clone()
    }

    fn sarzak_heel(&self) -> RefType<SarzakStore> {
        self.extruder_context.sarzak.clone()
    }

    fn push_symbol_table(&mut self) {
        let start = self.symbol_tables.last().unwrap().count();
        self.symbol_tables.push(SymbolTable::new(start));
    }

    fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop();
    }

    fn insert_symbol(&mut self, name: String, ty: ValueType) -> usize {
        match self.get_symbol(name.as_str()) {
            Some(value) => value.number,
            None => {
                let table = self.symbol_tables.last_mut().unwrap();
                table.insert(name, ty)
            }
        }
    }

    fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.get(name) {
                return Some(value);
            }
        }
        None
    }
}

pub fn compile(context: &ExtruderContext) -> Result<Program> {
    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());

    let mut context = Context::new(context);

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    for func in lu_dog.iter_function() {
        program.add_thonk(compile_function(&func, &mut context)?.into());
    }

    Ok(program)
}

fn compile_function(func: &RefType<Function>, context: &mut Context) -> Result<CThonk> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_function"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let func = s_read!(func);
    let ty_name = if let Some(i_block) = func.r9_implementation_block(&lu_dog).first() {
        let i_block = s_read!(i_block);
        if let Some(woog_struct) = i_block.r8_woog_struct(&lu_dog).first() {
            s_read!(woog_struct).name.clone()
        } else if let Some(woog_enum) = i_block.r84c_enumeration(&lu_dog).first() {
            s_read!(woog_enum).name.clone()
        } else {
            "".to_owned()
        }
    } else {
        "".to_owned()
    };

    // context.push_symbol_table();

    let (name, incr_fs) = if ty_name.is_empty() {
        (func.name.clone(), false)
    } else {
        let ty = if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(&ty_name) {
            // Here is where we look for actual user defined types, as
            // in types that are defined in dwarf source.
            let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
            let woog_struct = s_read!(woog_struct);
            woog_struct.r1_value_type(&lu_dog)[0].clone()
        } else if let Some(ref id) = lu_dog.exhume_enumeration_id_by_name(&ty_name) {
            let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
            let woog_enum = s_read!(woog_enum);
            woog_enum.r1_value_type(&lu_dog)[0].clone()
        } else {
            return Err(BubbaError::InternalCompilerError {
                location: location!(),
                message: format!("Could not find type: {ty_name}"),
            }
            .into());
        };

        context.insert_symbol("self".to_owned(), s_read!(ty).clone());
        (format!("{ty_name}::{}", func.name), true)
    };

    let mut thonk = CThonk::new(name.clone());

    if incr_fs {
        thonk.increment_frame_size();
    }

    let body = func.r19_body(&lu_dog)[0].clone();
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
                            thonk.add_instruction(
                                Instruction::Push(new_ref!(Value, Value::Empty)),
                                location!(),
                            );
                            thonk.add_instruction(Instruction::Return, location!());
                            thonk.returned = true;
                            break;
                        }
                    }
                }
            } else {
                thonk.add_instruction(
                    Instruction::Push(new_ref!(Value, Value::Empty)),
                    location!(),
                );
                thonk.add_instruction(Instruction::Return, location!());
                thonk.returned = true;
            }
        }
        //
        // This is an externally defined function that was declared in a dwarf file.
        BodyEnum::ExternalImplementation(ref _id) => {
            panic!("Somehow we found ourselves trying to compile an external implementation. This should not happen. The function name is: {name}");
        }
    };

    // context.pop_symbol_table();

    Ok(thonk)
}

fn compile_statement(
    statement: &RefType<Statement>,
    thonk: &mut CThonk,
    context: &mut Context,
) -> Result<()> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_statement"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog.exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)?;
        }
        StatementEnum::LetStatement(ref stmt) => {
            let stmt = lu_dog.exhume_let_statement(stmt).unwrap();
            let stmt = s_read!(stmt);

            let expr = stmt.r20_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)?;

            let var = s_read!(stmt.r21_local_variable(&lu_dog)[0]).clone();
            let var = s_read!(var.r12_variable(&lu_dog)[0]).clone();
            let value = s_read!(var.r11_x_value(&lu_dog)[0]).clone();
            let ty = s_read!(value.r24_value_type(&lu_dog)[0]).clone();

            let name = var.name;
            let offset = context.insert_symbol(name.clone(), ty);
            thonk.increment_frame_size();

            thonk.add_instruction(Instruction::StoreLocal(offset), location!());
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog.exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r41_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)?;

            thonk.add_instruction(Instruction::Return, location!());
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
    span: Span,
) -> Result<()> {
    log::debug!(target: "instr", "{}: {}:{}:{}", POP_CLR.paint("compile_expression"), file!(), line!(), column!());

    match &s_read!(expression).subtype {
        ExpressionEnum::Block(ref block) => block::compile(block, thonk, context)?,
        ExpressionEnum::Call(ref call) => call::compile(call, thonk, context, span)?,
        ExpressionEnum::FieldAccess(ref field) => {
            field::compile_field_access(field, thonk, context, span)?
        }
        ExpressionEnum::FieldExpression(ref field) => {
            field::compile_field_expression(field, thonk, context)?
        }

        ExpressionEnum::ForLoop(ref for_loop) => for_loop::compile(for_loop, thonk, context, span)?,
        ExpressionEnum::Index(ref index) => index::compile(index, thonk, context, span)?,
        ExpressionEnum::ListElement(ref list) => list::compile_list_element(list, thonk, context)?,
        ExpressionEnum::ListExpression(ref list) => {
            list::compile_list_expression(list, thonk, context, span)?
        }
        ExpressionEnum::Literal(ref literal) => literal::compile(literal, thonk, context, span)?,
        ExpressionEnum::Operator(ref op_type) => operator::compile(op_type, thonk, context, span)?,
        ExpressionEnum::RangeExpression(ref range) => range::compile(range, thonk, context)?,
        ExpressionEnum::StructExpression(ref expr) => {
            struct_expr::compile(expr, thonk, context, span)?
        }
        ExpressionEnum::TypeCast(ref expr) => typecast::compile(expr, thonk, context, span)?,
        ExpressionEnum::VariableExpression(ref expr) => {
            variable::compile(expr, thonk, context, span)?
        }
        ExpressionEnum::XIf(ref expr) => if_expr::compile(expr, thonk, context)?,
        ExpressionEnum::XMatch(ref expr) => xmatch::compile(expr, thonk, context, span)?,
        ExpressionEnum::XPrint(ref print) => print::compile(print, thonk, context)?,
        ExpressionEnum::XReturn(ref expr) => ret::compile(expr, thonk, context, span)?,
        missed => {
            panic!("Implement: {:?}", missed);
        }
    }

    Ok(())
}

fn get_span(expression: &RefType<Expression>, lu_dog: &LuDogStore) -> Span {
    let value = &s_read!(expression).r11_x_value(lu_dog)[0];
    let span = &s_read!(value).r63_span(lu_dog)[0];
    let read = s_read!(span);
    read.start as usize..read.end as usize
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use std::env;

    use super::*;

    use crate::{
        bubba::{vm::Error, VM},
        chacha::value::{EnumVariant, TupleEnum},
        dwarf::{new_lu_dog, parse_dwarf},
        lu_dog::ValueType,
        s_write,
        sarzak::MODEL as SARZAK_MODEL,
        NewRef, RefType,
    };

    pub(super) fn get_dwarf_home() -> PathBuf {
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
    pub(super) fn run_vm(program: &Program) -> Result<RefType<Value>, Error> {
        let mut vm = VM::new(program);
        vm.invoke("main", &[], true)
    }

    #[test]
    fn empty_func() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
    fn func_call() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
    fn test_let_statements() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 6);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(6));
    }

    #[test]
    fn test_func_args_and_locals() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 18);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(12));
    }

    #[test]
    fn test_argument_ordering() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 7);
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 29);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Empty);
    }

    #[test]
    fn test_boolean_true() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
    fn test_for_in_range() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(program.get_thonk("main").unwrap().get_instruction_card(), 5);

        assert_eq!(program.get_thonk("fib").unwrap().get_instruction_card(), 33);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(55));
    }

    #[test]
    fn match_literal_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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
            30
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(1));
    }

    #[test]
    fn match_literal_catchall() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 6 {
                           1 => 1,
                           2 => 2,
                           3 => 3,
                           a => {
                                print(a);
                                4
                           }
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
            33
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &4.into());
    }

    #[test]
    fn match_literal_exp_middle() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> int {
                       match 3 {
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
            30
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &3.into());
    }

    #[test]
    fn match_string_literal_expression() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
                       match \"foo\" {
                           \"foo\" => \"foo\",
                           \"bar\" => \"bar\",
                           \"baz\" => \"baz\",
                           _ => \"qux\",
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
            30
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("foo".to_owned())
        );
    }

    #[test]
    fn match_enum() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar,
                       Baz,
                       Qux,
                   }
                   fn main() -> Foo {
                       match Foo::Bar {
                           Foo::Bar => Foo::Bar,
                           Foo::Baz => Foo::Baz,
                           Foo::Qux => Foo::Qux,
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

        let ty = {
            let mut lu_dog = s_write!(ctx.lu_dog);

            let id = lu_dog.exhume_enumeration_id_by_name("Foo").unwrap();
            let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
            ValueType::new_enumeration(&woog_enum, &mut lu_dog)
        };

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            32
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Unit(ty, "Foo".to_owned(), "Bar".to_owned()))
        );
    }

    #[test]
    fn match_tuple_enum() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> Foo {
                       match Foo::Bar(40 + 2) {
                           Foo::Baz(1) => Foo::Baz(1),
                           Foo::Bar(42) => Foo::Bar(42),
                           Foo::Qux(1) => Foo::Qux(1),
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

        let lu_dog = &ctx.lu_dog;

        let id = s_read!(lu_dog)
            .exhume_enumeration_id_by_name("Foo")
            .unwrap();
        let woog_enum = s_read!(lu_dog).exhume_enumeration(&id).unwrap();
        let ty = ValueType::new_enumeration(&woog_enum, &mut s_write!(lu_dog));
        let user_enum = TupleEnum::new("Bar", new_ref!(Value, Value::Integer(42)));
        let user_enum = new_ref!(TupleEnum, user_enum);

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            53
        );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::Enumeration(EnumVariant::Tuple((ty, "Foo".to_owned()), user_enum))
        );
    }

    #[test]
    fn match_pattern_variable() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(42);
                       match x {
                           Foo::Bar(u) => u,
                           Foo::Baz(v) => v,
                           Foo::Qux(w) => w,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            50
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }

    #[test]
    fn something_interesting_in_match() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak_store = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   enum Foo {
                       Bar(int),
                       Baz(int),
                       Qux(int),
                   }
                   fn main() -> int {
                       let x = Foo::Baz(40);
                       match x {
                           Foo::Bar(u) => u + 2,
                           Foo::Baz(v) => v + 2,
                           Foo::Qux(w) => w + 2,
                       }
                   }";
        let ast = parse_dwarf("match_expression", ore).unwrap();
        let ctx = new_lu_dog(
            "match_expression".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak_store,
        )
        .unwrap();

        let program = compile(&ctx).unwrap();
        println!("{program}");

        assert_eq!(program.get_thonk_card(), 1);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            56
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(42));
    }

    #[test]
    fn index_into_list() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            13
        );

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &2.into());
    }

    #[test]
    fn index_out_of_bounds() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

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

    // #[test]
    fn index_into_string() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   fn main() -> string {
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

        // assert_eq!(
        //     program.get_thonk("main").unwrap().get_instruction_card(),
        //     8
        // );

        assert_eq!(
            &*s_read!(run_vm(&program).unwrap()),
            &Value::String("o".to_owned())
        );
    }

    #[test]
    fn use_std_option() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                   use std::Option;
                   fn main() -> bool {
                       let foo = Option::Some(1);
                       chacha::assert(foo.is_some());
                       let bar = Option::None;
                       chacha::assert(bar.is_none());

                       match foo {
                           Option::Some(x) => true,
                           Option::None => false,
                    }
                   }";
        let ast = parse_dwarf("use_std_option", ore).unwrap();
        let ctx = new_lu_dog(
            "use_std_option".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 3);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            59
        );
        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }
}
