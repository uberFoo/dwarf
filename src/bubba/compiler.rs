use heck::ToUpperCamelCase;
use log::{self, log_enabled, Level::Trace};
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, prelude::*, Location};

use crate::{
    bubba::instr::{Instruction, Program, Thonk},
    lu_dog::{
        BodyEnum, Expression, ExpressionEnum, Function, ObjectStore as LuDogStore, Statement,
        StatementEnum, ValueType,
    },
    new_ref, s_read, s_write,
    sarzak::{ObjectStore as SarzakStore, Ty},
    Context as ExtruderContext, NewRef, RefType, Span, Value, ERR_CLR, MERLIN, POP_CLR, SARZAK,
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

#[derive(Debug)]
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
        log::debug!(target: "instr", "{}: {name}: {number}", ERR_CLR.paint("symbol insert"));

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
    plugins: HashMap<String, String>,
}

impl<'a> Context<'a> {
    fn new(extruder_context: &'a ExtruderContext) -> Self {
        Context {
            extruder_context,
            symbol_tables: vec![SymbolTable::new(0)],
            method_name: None,
            plugins: HashMap::default(),
        }
    }

    fn lu_dog_heel(&self) -> RefType<LuDogStore> {
        self.extruder_context.lu_dog.clone()
    }

    fn sarzak_heel(&self) -> RefType<SarzakStore> {
        self.extruder_context.sarzak.clone()
    }

    fn insert_plugin(&mut self, name: String, path: String) {
        self.plugins.insert(name, path);
    }

    fn push_symbol_table(&mut self) {
        self.symbol_tables.push(SymbolTable::new(0));
    }

    fn push_child_symbol_table(&mut self) {
        let start = self.symbol_tables.last().unwrap().count();
        self.symbol_tables.push(SymbolTable::new(start));
    }

    fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop();
    }

    fn insert_symbol(&mut self, name: String, ty: ValueType) -> (bool, usize) {
        match self.get_symbol(name.as_str()) {
            Some(value) => (false, value.number),
            None => {
                let table = self.symbol_tables.last_mut().unwrap();
                (true, table.insert(name, ty))
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

    // We need to grab this specific instance's value of the string type.
    let ty = Ty::new_z_string(&s_read!(context.sarzak_heel()));
    let ty = ValueType::new_ty(true, &ty, &mut s_write!(lu_dog));
    let ty = Value::ValueType((*s_read!(ty)).clone());
    program.add_symbol("STRING".to_owned(), ty);

    let lu_dog = s_read!(lu_dog);

    for func in lu_dog.iter_function() {
        program.add_thonk(compile_function(&func, &mut context)?.into());
    }

    for (name, path) in context.plugins.iter() {
        program.add_symbol(format!("PLUGIN_{}", name), path.clone().into());
    }

    Ok(program)
}

fn compile_function(func: &RefType<Function>, context: &mut Context) -> Result<CThonk> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    let func = s_read!(func);
    let body = func.r19_body(&lu_dog)[0].clone();
    let body = s_read!(body);

    log::debug!(target: "instr", "{}: {}\n\t-->{}:{}:{}", POP_CLR.paint("compile_function"), func.name, file!(), line!(), column!());

    context.push_symbol_table();

    let params = func.r13_parameter(&lu_dog);

    // I need to iterate over the parameters to get the name of the parameter.
    if !params.is_empty() {
        let mut next = func
            .r13_parameter(&lu_dog)
            .iter()
            .find(|p| s_read!(p).r14c_parameter(&lu_dog).is_empty())
            .unwrap()
            .clone();

        loop {
            let param = next.clone();
            let param = s_read!(param);
            let var = s_read!(param.r12_variable(&lu_dog)[0]).clone();
            let ty = s_read!(param.r79_value_type(&lu_dog)[0]).clone();

            context.insert_symbol(var.name.clone(), ty);

            let next_id = { param.next };
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_parameter(id).unwrap();
            } else {
                break;
            }
        }
    }

    let (ty_name, ty_path) = if let Some(i_block) = func.r9_implementation_block(&lu_dog).first() {
        let i_block = s_read!(i_block);
        if let Some(woog_struct) = i_block.r8_woog_struct(&lu_dog).first() {
            let woog_struct = s_read!(woog_struct);
            let path = woog_struct.x_path.clone();
            let name = woog_struct.name.clone();
            (name, path)
        } else if let Some(woog_enum) = i_block.r84c_enumeration(&lu_dog).first() {
            let woog_enum = s_read!(woog_enum);
            let path = woog_enum.x_path.clone();
            let name = woog_enum.name.clone();
            (name, path)
        } else {
            ("".to_owned(), "".to_owned())
        }
    } else {
        ("".to_owned(), "".to_owned())
    };

    let (name, incr_fs) = if ty_name.is_empty() {
        (func.name.clone(), false)
    } else {
        // Here is where we look for actual user defined types, as
        // in types that are defined in dwarf source.
        let ty = if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(&ty_name) {
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
        BodyEnum::ExternalImplementation(ref block_id) => {
            let external = lu_dog.exhume_external_implementation(block_id).unwrap();
            let external = s_read!(external);
            let model_name = external.x_model.clone();
            let model_name = if model_name == MERLIN {
                SARZAK.to_owned()
            } else {
                model_name
            };
            let models = &context.extruder_context.models;
            let model = models.get(&model_name).unwrap();
            let func_name = external.function.clone();

            let object_name = &external.object;
            let object_name = object_name.to_upper_camel_case();

            dbg!(model, func_name, object_name);
        }
    };

    context.pop_symbol_table();

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
            let offset = match context.insert_symbol(name.clone(), ty) {
                (true, index) => {
                    thonk.increment_frame_size();
                    index
                }
                (false, index) => index,
            };

            thonk.add_instruction(Instruction::StoreLocal(offset), location!());
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog.exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r41_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)?;

            // 🚧 This is incorrect. We should only return if we are in an outer scope.
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
        ExpressionEnum::XDebugger(_) => {}
        ExpressionEnum::FieldAccess(ref field) => {
            field::compile_field_access(field, thonk, context, span)?
        }
        ExpressionEnum::FieldExpression(ref field) => {
            field::compile_field_expression(field, thonk, context)?
        }

        ExpressionEnum::ForLoop(ref for_loop) => for_loop::compile(for_loop, thonk, context, span)?,
        ExpressionEnum::Index(ref index) => index::compile(index, thonk, context, span)?,
        ExpressionEnum::Lambda(ref λ) => {}
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

    pub(super) fn run_vm(program: &Program) -> Result<RefType<Value>, Error> {
        let mut vm = VM::new(program, &[], &get_dwarf_home());
        vm.invoke("main", &[])
    }

    pub(super) fn run_vm_with_args(
        program: &Program,
        args: &[RefType<Value>],
    ) -> Result<RefType<Value>, Error> {
        let mut vm = VM::new(program, args, &get_dwarf_home());
        vm.invoke("main", &[])
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
                   use std::option::Option;
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
        assert_eq!(program.get_thonk_card(), 5);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            59
        );
        let run = run_vm(&program);
        eprintln!("{:?}", run);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    #[test]
    fn use_plugin() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = "
                use http::client::HttpClient;
                fn main() -> bool {
                    let client = HttpClient::new();
                    true
                }";
        let ast = parse_dwarf("use_plugin", ore).unwrap();
        let ctx = new_lu_dog(
            "use_plugin".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 11);

        // assert_eq!(
        //     program.get_thonk("main").unwrap().get_instruction_card(),
        //     59
        // );
        let run = run_vm(&program);
        println!("{:?}", run);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    // #[test]
    fn use_async() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ore = r#"
use http::client::HttpClient;
use http::client::HttpError;
use http::client::Response;
use std::result::Result;

async fn async_get(urls: [String]) -> Future<[Result<string, HttpError>]> {
    let client = HttpClient::new();

    let tasks: [Future<Result<string, HttpError>>] = [];
    // Start a task for each url and push them into the tasks array.
    for url in urls {
        let task = chacha::spawn(async || -> Result<string, HttpError> {
            // This creates a request and sends it.
            let get = client.get(url).await.send().await;
            match get {
                Result::<Response, HttpError>::Ok(response) => {
                    let text = response.text().await;
                    match text {
                        Result::Ok(text) => Result::<string, HttpError>::Ok(text),
                        // Return an error if there was a problem getting the page's text.
                        Result::Err(e) => Result::<string, HttpError>::Err(e),
                    }
                }
                // Return an Error if there was a problem creating or sending the request.
                Result::<Response, HttpError>::Err(e) => Result::<string, HttpError>::Err(e),
            }
        });
        tasks.push(task);
    }

    let results: [Result<string, HttpError>] = [];
    for task in tasks {
        // Await each task that was spawned above.
        let result = task.await;
        results.push(result);
    }

    results
}

async fn main() -> Future<()> {
    let requests = [
        "https://en.wikipedia.org/wiki/Main_Page",
        "https://en.wikipedi.org/wiki/Main_Page",
        "https://www.rust-lang.org/",
        "https://www.github.com/",
    ];
    let results = async_get(requests).await;

    let i = 0;
    for result in results {
        match result {
            Result::Ok(response) => {
                print("{1}: {0} bytes\n".format(response.len(), requests[i]));
            }
            Result::Err(e) => {
                print("{1}: {0}\n".format(e.to_string(), requests[i]));
            }
        }
        i = i + 1;
    }
}                "#;
        let ast = parse_dwarf("use_plugin", ore).unwrap();
        let ctx = new_lu_dog(
            "use_plugin".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 11);

        // assert_eq!(
        //     program.get_thonk("main").unwrap().get_instruction_card(),
        //     59
        // );
        let run = run_vm(&program);
        println!("{:?}", run);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    #[test]
    fn test_locals_and_params() {
        let _ = env_logger::builder().is_test(true).try_init();
        color_backtrace::install();

        let ore = "
                   fn main() -> int {
                       let x = 1;
                       let y = 2;
                       let z = 3;
                       foo(x, y, z)
                   }
                   fn foo(a: int, b: int, c: int) -> int {
                       let z = 42;
                       let x = a + b;
                       let y = x + c;
                       y
                   }";
        let ast = parse_dwarf("test_locals_and_params", ore).unwrap();
        let sarzak = SarzakStore::from_bincode(SARZAK_MODEL).unwrap();
        let ctx = new_lu_dog(
            "test_locals_and_params".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 2);

        assert_eq!(
            program.get_thonk("main").unwrap().get_instruction_card(),
            13
        );
        assert_eq!(program.get_thonk("foo").unwrap().get_instruction_card(), 12);
        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Integer(6));
    }
}
