use heck::ToUpperCamelCase;
use log::{self, log_enabled, Level::Trace};
use rustc_hash::FxHashMap as HashMap;
use snafu::{location, prelude::*, Location};

use crate::{
    bubba::{
        instr::{Instruction, Program, Thonk},
        BOOL, EMPTY, INT, RANGE, RESULT, STRING, STRING_ARRAY, UNKNOWN,
    },
    lu_dog::{
        BodyEnum, Expression, ExpressionEnum, Function, ObjectStore as LuDogStore, Statement,
        StatementEnum, ValueType, ValueTypeEnum,
    },
    new_ref, s_read, s_write,
    sarzak::{ObjectStore as SarzakStore, Ty},
    Context as ExtruderContext, NewRef, RefType, Span, Value, ERR_CLR, MERLIN, POP_CLR, SARZAK,
};

mod expression;

use expression::{
    a_weight, block, call, field, for_loop, if_expr, index, list, literal, operator, print, range,
    ret, struct_expr, typecast, variable, xmatch,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const BUILD_TIME: &str = include!(concat!(env!("OUT_DIR"), "/timestamp.txt"));

#[derive(Clone, Debug, Snafu)]
pub struct Error(BubbaError);

#[derive(Clone, Debug, Snafu)]
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

    fn insert_instruction(&mut self, instruction: Instruction, location: Location) {
        log::debug!(target: "instr", "{}:\t\t{instruction}\n  --> {}:{}:{}", POP_CLR.paint("add_instruction"), location.file, location.line, location.column);

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

    fn prefix_instruction(&mut self, instruction: Instruction, location: Location) {
        log::debug!(target: "instr", "{}:\t\t{instruction}\n  --> {}:{}:{}", POP_CLR.paint("prefix_instruction"), location.file, location.line, location.column);

        if log_enabled!(target: "instr", Trace) {
            self.inner.add_instruction(
                Instruction::Comment(new_ref!(
                    String,
                    format!("{}:{}:{}", location.file, location.line, location.column)
                )),
                None,
            );
        }
        self.inner.prefix_instruction(instruction, None);
    }

    fn insert_instruction_with_span(
        &mut self,
        instruction: Instruction,
        span: Span,
        location: Location,
    ) {
        log::debug!(target: "instr", "{}:\t\t{instruction}\n  --> {}:{}:{}", POP_CLR.paint("add_instruction"), location.file, location.line, location.column);

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
        self.inner.instruction_card()
    }

    fn increment_frame_size(&mut self) {
        self.inner.increment_frame_size();
    }

    fn get_frame_size(&self) -> usize {
        self.inner.frame_size()
    }

    fn append(&mut self, other: CThonk) {
        self.inner.append_thonk(&other.inner);
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
        log::debug!(target: "instr", "{}: {start}", ERR_CLR.paint("new symbol table"));
        SymbolTable {
            start,
            map: HashMap::default(),
        }
    }

    fn insert(&mut self, name: String, ty: ValueType) -> usize {
        let number = self.count();
        log::debug!(target: "instr", "{}: {name} ({number})", ERR_CLR.paint("symbol insert"));

        self.map.insert(name, Symbol { number, ty });
        number
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        self.map.get(name)
    }

    fn count(&self) -> usize {
        self.map.len() + self.start
    }

    fn start(&self) -> usize {
        self.start
    }
}

impl Drop for SymbolTable {
    fn drop(&mut self) {
        log::debug!(target: "instr", "{}", ERR_CLR.paint("drop symbol table"));
    }
}

#[derive(Debug)]
struct Context<'a, 'b> {
    extruder_context: &'a ExtruderContext,
    symbol_tables: Vec<SymbolTable>,
    program: &'b mut Program,
    st_depth: usize,
    funcs: HashMap<String, ValueType>,
    pub(crate) captures: Option<HashMap<String, usize>>,
    types: HashMap<String, ValueType>,
}

impl<'a, 'b> Context<'a, 'b> {
    fn new(extruder_context: &'a ExtruderContext, program: &'b mut Program) -> Self {
        Context {
            extruder_context,
            symbol_tables: vec![],
            program,
            st_depth: 0,
            funcs: HashMap::default(),
            captures: None,
            types: HashMap::default(),
        }
    }

    fn insert_type(&mut self, name: String, ty: ValueType) {
        self.types.insert(name, ty);
    }

    fn get_type(&self, name: &str) -> Option<&ValueType> {
        self.types.get(name)
    }

    fn insert_function(&mut self, name: String, ty: ValueType) {
        self.funcs.insert(name, ty);
    }

    fn check_function(&self, name: &str) -> Option<&ValueType> {
        self.funcs.get(name)
    }

    fn get_program(&mut self) -> &mut Program {
        self.program
    }

    fn lu_dog_heel(&self) -> RefType<LuDogStore> {
        self.extruder_context.lu_dog.clone()
    }

    fn sarzak_heel(&self) -> RefType<SarzakStore> {
        self.extruder_context.sarzak.clone()
    }

    fn push_symbol_table(&mut self) {
        self.st_depth += 1;
        self.symbol_tables.push(SymbolTable::new(0));
    }

    fn push_scope(&mut self) {
        self.st_depth += 1;
        let start = self.symbol_tables.last().unwrap().count();
        self.symbol_tables.push(SymbolTable::new(start));
    }

    fn pop_symbol_table(&mut self) {
        self.st_depth -= 1;
        self.symbol_tables.pop();
    }

    fn pop_scope(&mut self) {
        self.st_depth -= 1;
        self.symbol_tables.pop();
    }

    fn is_root_symbol_table(&self) -> bool {
        self.st_depth == 1
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
        // This is a bit goofy, and I think I can work around it.
        // The goofy bit is that our symbol tables aren't bound to just lexical
        // scope. The extend across function calls. This shouldn't be a problem
        // in general because the extruder takes care of checking that functions
        // aren't referencing anything outside of their scope.
        //
        // The exception is lambdas. In their case, we'll get symbol offset
        // duplicates. Think about it for a second -- you'll get there.
        // I think this is ok.
        //
        // When we compile the lambda, we know which symbols are captured from
        // the outer scope. We introduce them into the symbol table of the lambda.
        // This will cause the symbol lookups to use the offset for the lambda.
        //
        // Yeah, so I'm wrong. The extruder does not check for variable validity:
        // the interpreter does that. So I either need to change the extruder,
        // or change the compiler. Presumably, it was hard checking in the
        // extruder. I'll need to cogitate.
        //
        // Something interesting happens with missing variables. When they
        // aren't found in the symbol table the variable code emits instructions
        // for setting up a function call stack. When the VM get's hold of this
        // they result in missing symbols.
        //
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.get(name) {
                return Some(value);
            }
            if table.start() == 0 {
                break;
            }
        }

        None
    }
}

pub fn compile(context: &ExtruderContext) -> Result<Program> {
    let mut program = Program::new(VERSION.to_owned(), BUILD_TIME.to_owned());
    program.set_source(context.source());

    let mut context = Context::new(context, &mut program);

    let lu_dog = context.lu_dog_heel();
    let sarzak = context.sarzak_heel();

    // We need to grab this specific instance's value of the string type.
    let string = Ty::new_z_string(&s_read!(sarzak));
    let string = ValueType::new_ty(true, &string, &mut s_write!(lu_dog));
    let string = (*s_read!(string)).clone();
    let string_value = Value::ValueType(string.clone());
    context
        .get_program()
        .add_symbol(STRING.to_owned(), string_value);

    context.insert_type(STRING.to_owned(), string.clone());

    let boolean = Ty::new_boolean(&s_read!(context.sarzak_heel()));
    let boolean = (*s_read!(ValueType::new_ty(true, &boolean, &mut s_write!(lu_dog)))).clone();
    context.insert_type(BOOL.to_owned(), boolean);

    let empty = ValueType::new_empty(true, &mut s_write!(lu_dog));
    let empty = (*s_read!(empty)).clone();
    context.insert_type("Empty".to_owned(), empty);

    let range_ty = ValueType::new_range(true, &mut s_write!(lu_dog));
    let range_ty = (*s_read!(range_ty)).clone();
    context.insert_type(RANGE.to_owned(), range_ty);

    let unknown = ValueType::new_unknown(true, &mut s_write!(lu_dog));
    let unknown = (*s_read!(unknown)).clone();
    context.insert_type(UNKNOWN.to_owned(), unknown);

    let int = Ty::new_integer(&s_read!(sarzak));
    let int = ValueType::new_ty(true, &int, &mut s_write!(lu_dog));
    let int = (*s_read!(int)).clone();
    context.insert_type(INT.to_owned(), int);

    let empty = ValueType::new_empty(true, &mut s_write!(lu_dog));
    let empty = (*s_read!(empty)).clone();
    context.insert_type(EMPTY.to_owned(), empty);

    let mut string_array = ValueType::new_empty(true, &mut s_write!(lu_dog));
    for vt in s_read!(lu_dog).iter_value_type() {
        if let ValueTypeEnum::List(id) = s_read!(vt).subtype {
            let list = s_read!(lu_dog).exhume_list(&id).unwrap();
            let list_ty = s_read!(list).r36_value_type(&s_read!(lu_dog))[0].clone();
            if string == *s_read!(list_ty) {
                string_array = vt.clone();
            }
        }
    }
    let string_array = (*s_read!(string_array)).clone();
    context.insert_type(STRING_ARRAY.to_owned(), string_array);

    // And Result
    if let Some(ref ty) = s_read!(lu_dog).exhume_enumeration_id_by_name("::std::result::Result") {
        let ty = s_read!(lu_dog).exhume_enumeration(&ty).unwrap();
        let Some(ty) = s_read!(lu_dog).iter_value_type().find(|vt| {
            if let ValueTypeEnum::Enumeration(id) = s_read!(vt).subtype {
                let id = s_read!(lu_dog).exhume_enumeration(&id).unwrap();
                if s_read!(id).id == s_read!(ty).id {
                    return true;
                }
            }
            false
        }) else {
            unreachable!()
        };
        context
            .get_program()
            .add_symbol(RESULT.to_owned(), Value::ValueType((*s_read!(ty)).clone()));
    };

    let lu_dog = s_read!(lu_dog);

    for func in lu_dog.iter_function() {
        context.insert_function(
            get_function_name(&func, &lu_dog),
            get_function_type(&func, &lu_dog),
        );
    }

    for func in lu_dog.iter_function() {
        let thonk = compile_function(&func, &mut context)?.into();
        context.get_program().add_thonk(thonk);
    }

    Ok(program)
}

fn get_function_type(func: &RefType<Function>, lu_dog: &LuDogStore) -> ValueType {
    let func = s_read!(func);
    let ty = func.r1_value_type(lu_dog)[0].clone();
    let ty = (*s_read!(ty)).clone();

    ty
}

fn get_function_name(func: &RefType<Function>, lu_dog: &LuDogStore) -> String {
    let func = s_read!(func);
    let name = func.name.clone();
    let ty_name = if let Some(i_block) = func.r9_implementation_block(&lu_dog).first() {
        let i_block = s_read!(i_block);
        if let Some(woog_struct) = i_block.r8_woog_struct(&lu_dog).first() {
            let woog_struct = s_read!(woog_struct);
            let name = woog_struct.name.clone();
            name
        } else if let Some(woog_enum) = i_block.r84c_enumeration(&lu_dog).first() {
            let woog_enum = s_read!(woog_enum);
            let name = woog_enum.name.clone();
            name
        } else {
            "".to_owned()
        }
    } else {
        "".to_owned()
    };

    if ty_name.is_empty() {
        name
    } else {
        format!("{ty_name}::{name}")
    }
}

fn compile_function(func: &RefType<Function>, context: &mut Context) -> Result<CThonk> {
    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    context.push_symbol_table();

    let func = s_read!(func);
    let body = func.r19_body(&lu_dog)[0].clone();
    let body = s_read!(body);
    let params = func.r13_parameter(&lu_dog);

    log::debug!(target: "instr", "{}: {}\n  --> {}:{}:{}", ERR_CLR.paint("compile_function"), func.name, file!(), line!(), column!());

    // I need to iterate over the parameters to get the name of the parameter.
    if !params.is_empty() {
        let mut next = func.r82_parameter(&lu_dog)[0].clone();
        loop {
            let param = next.clone();
            let param = s_read!(param);
            let var = s_read!(param.r12_variable(&lu_dog)[0]).clone();
            let ty = s_read!(param.r79_value_type(&lu_dog)[0]).clone();

            context.insert_symbol(var.name.clone(), ty);

            let next_id = param.next;
            if let Some(ref id) = next_id {
                next = lu_dog.exhume_parameter(id).unwrap();
            } else {
                break;
            }
        }
    }

    let ty_name = if let Some(i_block) = func.r9_implementation_block(&lu_dog).first() {
        let i_block = s_read!(i_block);
        if let Some(woog_struct) = i_block.r8_woog_struct(&lu_dog).first() {
            let woog_struct = s_read!(woog_struct);
            let name = woog_struct.name.clone();
            name
        } else if let Some(woog_enum) = i_block.r84c_enumeration(&lu_dog).first() {
            let woog_enum = s_read!(woog_enum);
            let name = woog_enum.name.clone();
            name
        } else {
            "".to_owned()
        }
    } else {
        "".to_owned()
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

    // This is making room for the self parameter.
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
                            thonk.insert_instruction(
                                Instruction::Push(new_ref!(Value, Value::Empty)),
                                location!(),
                            );
                            thonk.insert_instruction(Instruction::Return, location!());
                            thonk.returned = true;
                            break;
                        }
                    }
                }
            } else {
                thonk.insert_instruction(
                    Instruction::Push(new_ref!(Value, Value::Empty)),
                    location!(),
                );
                thonk.insert_instruction(Instruction::Return, location!());
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
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}:\n --> {}:{}:{}", POP_CLR.paint("compile_statement"), file!(), line!(), column!());

    let lu_dog = context.lu_dog_heel();
    let lu_dog = s_read!(lu_dog);

    match s_read!(statement).subtype {
        StatementEnum::ExpressionStatement(ref stmt) => {
            let stmt = lu_dog.exhume_expression_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r31_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            compile_expression(&expr, thonk, context, span)
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

            thonk.insert_instruction(Instruction::StoreLocal(offset), location!());

            let empty = context.get_type(EMPTY).unwrap().clone();
            Ok(Some(empty))
        }
        StatementEnum::ResultStatement(ref stmt) => {
            let stmt = lu_dog.exhume_result_statement(stmt).unwrap();
            let stmt = s_read!(stmt);
            let expr = stmt.r41_expression(&lu_dog)[0].clone();
            let span = get_span(&expr, &lu_dog);
            let result = compile_expression(&expr, thonk, context, span);

            if context.is_root_symbol_table() {
                thonk.insert_instruction(Instruction::Return, location!());
                thonk.returned = true;
            }

            result
        }
        StatementEnum::ItemStatement(_) => unimplemented!(),
    }
}

fn compile_expression(
    expression: &RefType<Expression>,
    thonk: &mut CThonk,
    context: &mut Context,
    span: Span,
) -> Result<Option<ValueType>> {
    log::debug!(target: "instr", "{}:\n  -> {}:{}:{}", POP_CLR.paint("compile_expression"), file!(), line!(), column!());

    match &s_read!(expression).subtype {
        ExpressionEnum::AWait(ref expr) => a_weight::compile(expr, thonk, context, span),
        ExpressionEnum::Block(ref block) => block::compile(block, thonk, context),
        ExpressionEnum::Call(ref call) => call::compile(call, thonk, context, span),
        ExpressionEnum::XDebugger(_) => Ok(None),
        ExpressionEnum::FieldAccess(ref field) => {
            field::compile_field_access(field, thonk, context, span)
        }
        ExpressionEnum::FieldExpression(ref field) => {
            field::compile_field_expression(field, thonk, context)
        }

        ExpressionEnum::ForLoop(ref for_loop) => for_loop::compile(for_loop, thonk, context, span),
        ExpressionEnum::Index(ref index) => index::compile(index, thonk, context, span),
        ExpressionEnum::Lambda(ref λ) => call::compile_lambda(λ, thonk, context, span),
        ExpressionEnum::ListElement(ref list) => list::compile_list_element(list, thonk, context),
        ExpressionEnum::ListExpression(ref list) => {
            list::compile_list_expression(list, thonk, context, span)
        }
        ExpressionEnum::Literal(ref literal) => literal::compile(literal, thonk, context, span),
        ExpressionEnum::Operator(ref op_type) => operator::compile(op_type, thonk, context, span),
        ExpressionEnum::RangeExpression(ref range) => range::compile(range, thonk, context),
        ExpressionEnum::StructExpression(ref expr) => {
            struct_expr::compile(expr, thonk, context, span)
        }
        ExpressionEnum::TypeCast(ref expr) => typecast::compile(expr, thonk, context, span),
        ExpressionEnum::VariableExpression(ref expr) => {
            variable::compile(expr, thonk, context, span)
        }
        ExpressionEnum::XIf(ref expr) => if_expr::compile(expr, thonk, context),
        ExpressionEnum::XMatch(ref expr) => xmatch::compile(expr, thonk, context, span),
        ExpressionEnum::XPrint(ref print) => print::compile(print, thonk, context),
        ExpressionEnum::XReturn(ref expr) => ret::compile(expr, thonk, context, span),
        missed => {
            panic!("Implement: {:?}", missed);
        }
    }
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

    use tracing_test::traced_test;

    use crate::{
        bubba::{vm::Error, VM},
        dwarf::{new_lu_dog, parse_dwarf},
        sarzak::MODEL as SARZAK_MODEL,
        RefType,
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

    // Nothing special about this number.
    const THREADS: usize = 5;
    pub(super) fn run_vm(program: &Program) -> Result<RefType<Value>, Error> {
        let mut vm = VM::new(program, &[], &get_dwarf_home(), THREADS);
        vm.invoke("main", &[])
    }

    pub(super) fn run_vm_with_args(
        program: &Program,
        args: &[RefType<Value>],
    ) -> Result<RefType<Value>, Error> {
        let mut vm = VM::new(program, args, &get_dwarf_home(), THREADS);
        vm.invoke("main", &[])
    }

    pub(super) fn setup_logging() {
        color_backtrace::install();
    }

    #[traced_test]
    #[test]
    fn test_let_statements() {
        setup_logging();

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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 8);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(5));
    }

    #[traced_test]
    #[test]
    fn test_boolean_true() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 2);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Boolean(true));
    }

    #[traced_test]
    #[test]
    fn test_boolean_false() {
        setup_logging();
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
        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 2);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Boolean(false));
    }

    #[traced_test]
    #[test]
    fn fibonacci() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 5);

        assert_eq!(program.get_thonk("fib").unwrap().instruction_card(), 30);

        assert_eq!(&*s_read!(run_vm(&program).unwrap()), &Value::Integer(55));
    }

    #[traced_test]
    #[test]
    fn use_std_option() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 59);
        let run = run_vm(&program);
        eprintln!("{:?}", run);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    #[traced_test]
    #[test]
    fn use_plugin() {
        setup_logging();
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

        assert_eq!(program.get_instruction_count(), 316);
        let run = run_vm(&program);
        println!("{:?}", run);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    #[traced_test]
    #[test]
    fn use_async() {
        setup_logging();
        // let _ = env_logger::builder().is_test(true).try_init();

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
        print(url);
        print("\n");
        let task = chacha::spawn(async || -> Result<string, HttpError> {
            // This creates a request and sends it.
            let get = client.get(url).await;
            let get = get.send().await;
            match get {
                Result::<Response, HttpError>::Ok(response) => {
                    let text = response.text().await;
                    match text {
                        Result::<string, HttpError>::Ok(text) => Result::<string, HttpError>::Ok(text),
                        // Return an error if there was a problem getting the page's text.
                        Result::<string, HttpError>::Err(e) => Result::<string, HttpError>::Err(e),
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
        "https://10.0.1.1",
        "https://www.rust-lang.org/",
        "https://en.wikipedia.org/wiki/Main_Page",
        "https://en.wikipedi.org/wiki/Main_Page",
        "https://www.github.com/",
    ];
    let results = async_get(requests).await;

    print("Results length: ");
    print(results.len());
    print("\n");

    let i = 0;
    for result in results {
        match result {
            Result::<string, HttpError>::Ok(req) => {
                // print("{1}: {0} bytes\n".format(req.len(), requests[i]));
                print(requests[i]);
                print(": ");
                print(req.len());
                print(" bytes\n");
            }
            Result::<string, HttpError>::Err(e) => {
                // print("{1}: {0}\n".format(e.to_string(), requests[i]));
                print(requests[i]);
                print(": ");
                print(e.to_string());
                print("\n");
            }
        };
        i = i + 1;
    }
}                "#;
        let ast = parse_dwarf("use_async", ore).unwrap();
        let ctx = new_lu_dog(
            "use_async".to_owned(),
            Some((ore.to_owned(), &ast)),
            &get_dwarf_home(),
            &sarzak,
        )
        .unwrap();
        let program = compile(&ctx).unwrap();
        println!("{program}");
        assert_eq!(program.get_thonk_card(), 13);

        // assert_eq!(
        //     program..get_instruction_count(),
        //     59
        // );
        let run = run_vm(&program);
        println!("{:?}", run);
        assert!(run.is_ok());
        // assert_eq!(&*s_read!(run.unwrap()), &Value::Boolean(true));
    }

    #[traced_test]
    #[test]
    fn test_locals_and_params() {
        setup_logging();
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

        assert_eq!(program.get_thonk("main").unwrap().instruction_card(), 13);
        assert_eq!(program.get_thonk("foo").unwrap().instruction_card(), 12);
        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Integer(6));
    }

    #[traced_test]
    #[test]
    fn test_scopes() {
        setup_logging();
        let ore = "
                   fn main() -> int {
                    let a = 0;
                    {
                        let b = 1;
                        {
                            let c = 2;
                        };
                    };
                    {
                        let b = 3;
                    };
                    a
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
        assert_eq!(program.get_thonk_card(), 1);

        let run = run_vm(&program);
        assert!(run.is_ok());
        assert_eq!(&*s_read!(run.unwrap()), &Value::Integer(0));
    }
}
