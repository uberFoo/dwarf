use core::fmt;
use std::{fs::File, io::prelude::*, ops::Range, path::PathBuf};

use ansi_term::Colour;
use heck::{ToShoutySnakeCase, ToUpperCamelCase};
use log;
use sarzak::sarzak::{store::ObjectStore as SarzakStore, types::Ty, Object};
use snafu::{location, Location};
use tracy_client::Client;
use uuid::Uuid;

use crate::{
    dwarf::{
        DwarfError, Expression as ParserExpression, Item, Result, Spanned,
        Statement as ParserStatement, Type,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{
            Block, BooleanOperator, Call, ErrorExpression, Expression, ExpressionEnum,
            ExpressionStatement, Field, FieldExpression, ForLoop, Function, Implementation, Index,
            IntegerLiteral, Item as WoogItem, ItemStatement, Lambda, LambdaParameter, LetStatement,
            Literal, LocalVariable, Parameter, Print, RangeExpression, Span as LuDogSpan,
            Statement, StaticMethodCall, StringLiteral, StructExpression, ValueType, ValueTypeEnum,
            Variable, VariableExpression, WoogOption, WoogStruct, XIf, XValue, XValueEnum, ZSome,
        },
        Argument, Binary, BooleanLiteral, Comparison, DwarfSourceFile, FieldAccess,
        FieldAccessTarget, FloatLiteral, List, ListElement, ListExpression, MethodCall, Operator,
        Reference, ResultStatement, TypeCast, Unary, VariableEnum, WoogOptionEnum, XReturn,
    },
    new_ref, s_read, s_write, NewRef, RefType,
};

macro_rules! link_parameter {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_parameter(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_argument {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_argument(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_statement {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_statement(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_list_element {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_list_element(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! function {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}

macro_rules! debug {
    ($($arg:tt)*) => {
        log::debug!(
            target: "extruder",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        )
    };
}

macro_rules! warn {
    ($($arg:tt)*) => {
        log::warn!(
            target: "extruder",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Cyan.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        )
    };
}

macro_rules! error {
    ($($arg:tt)*) => {
        log::error!(
            target: "extruder",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Red.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        )
    };
}

type Span = Range<usize>;
type ExprSpan = (RefType<Expression>, RefType<LuDogSpan>);

// These below are just to avoid cloning things.
struct ConveyFunc<'a> {
    name: &'a str,
    span: &'a Span,
    params: &'a [(Spanned<String>, Spanned<Type>)],
    return_type: &'a Spanned<Type>,
    statements: &'a Spanned<ParserExpression>,
}

impl<'a> ConveyFunc<'a> {
    fn new(
        name: &'a str,
        span: &'a Span,
        params: &'a [(Spanned<String>, Spanned<Type>)],
        return_type: &'a Spanned<Type>,
        statements: &'a Spanned<ParserExpression>,
    ) -> Self {
        Self {
            name,
            span,
            params,
            return_type,
            statements,
        }
    }
}

struct ConveyStruct<'a> {
    name: &'a str,
    span: &'a Span,
    fields: &'a [(Spanned<String>, Spanned<Type>)],
}

impl<'a> ConveyStruct<'a> {
    fn new(name: &'a str, span: &'a Span, fields: &'a [(Spanned<String>, Spanned<Type>)]) -> Self {
        Self { name, span, fields }
    }
}

struct ConveyImpl<'a> {
    name: &'a str,
    span: &'a Span,
    funcs: &'a [Spanned<Item>],
}

impl<'a> ConveyImpl<'a> {
    fn new(name: &'a str, span: &'a Span, funcs: &'a [Spanned<Item>]) -> Self {
        Self { name, span, funcs }
    }
}

/// The main entry point
///
/// This is where we go to populate the model from the parsed AST.
///
/// So this thing is going to get reworked. Currently `out_dir` is used to write
/// `woog_structs.rs`, which contains the UUIDs of the woog structs in the LuDog
/// domain.
///
/// `ast` is of course the parsed AST. It's a list of items, as found in the
/// source code. The AST is itself sort of an intermediate representation. It's
/// this program that turns it into instance is the model. I'm writing this,
/// wondering why I did this. Why am I not just populating a LuDog model in
/// the parser? Okay, so I'm actually doing some type analysis here. So this is
/// sort of a compiler... I need to figure out what to call it. I'm not in love
/// with "populator", but I'm not sure that it lives up to "compiler".
///
/// `models`. An array of models. I guess it's a reference to a slice of models
/// really. These are used by the compiler(?) to resolve imported object references.
///
/// `sarzak`. This one really needs a close looking at. It's just an empty metamodel.
/// It's used in two or three places, and it's baggage on every function call. I think
/// that it may only be used to look up the id of the UUID type.
pub fn new_lu_dog(
    out_dir: Option<&PathBuf>,
    source: Option<(String, &[Spanned<Item>])>,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<LuDogStore> {
    let mut lu_dog = LuDogStore::new();

    // We need to stuff all of the sarzak types into the store.
    ValueType::new_ty(&Ty::new_boolean(), &mut lu_dog);
    ValueType::new_ty(&Ty::new_float(), &mut lu_dog);
    ValueType::new_ty(&Ty::new_integer(), &mut lu_dog);
    ValueType::new_ty(&Ty::new_s_string(), &mut lu_dog);
    ValueType::new_ty(&Ty::new_s_uuid(), &mut lu_dog);

    if let Some((source, ast)) = source {
        let _client = Client::start();

        let source = DwarfSourceFile::new(source, &mut lu_dog);

        // I'm going to try promoting the string type to a struct and adding method
        // definitions so that we can type check better.
        // let string = WoogStruct::new("string".to_owned(), None, &mut lu_dog);
        // let implementation = Implementation::new(&string, &mut lu_dog);
        // let _ = WoogItem::new_implementation(&source, &implementation, &mut lu_dog);
        // let block = Block::new(Uuid::new_v4(), None, &mut lu_dog);
        // let ret_ty = Ty::new_s_string();
        // let ret_ty = ValueType::new_ty(&ret_ty, &mut lu_dog);
        // let func = Function::new(
        //     "len".to_owned(),
        //     &block,
        //     Some(&implementation),
        //     &ret_ty,
        //     &mut lu_dog,
        // );
        // let _ = WoogItem::new_function(&source, &func, &mut lu_dog);
        // Create a type for our function
        // let _ = ValueType::new_function(&func, &mut lu_dog);

        walk_tree(out_dir, ast, &source, &mut lu_dog, models, sarzak)?;
    }

    Ok(lu_dog)
}

fn walk_tree(
    out_dir: Option<&PathBuf>,
    ast: &[Spanned<Item>],
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    let mut funcs = Vec::new();
    let mut implementations = Vec::new();
    let mut structs = Vec::new();

    // We need the structs before the impls, so we do this.
    for item in ast {
        match item {
            (Item::Function((name, _name_span), params, return_type, stmts), span) => {
                funcs.push(ConveyFunc::new(name, span, params, return_type, stmts))
            }
            (Item::Implementation((name, _name_span), funcs), span) => {
                implementations.push(ConveyImpl::new(name, span, funcs))
            }
            // Imports can happen any time, I think.
            (Item::Import((path, _path_span), alias), span) => {
                inter_import(path, alias, &s_read!(source).source, span, lu_dog)?
            }
            (Item::Struct((name, span), fields), _span) => {
                structs.push(ConveyStruct::new(name, span, fields))
            }
        }
    }

    let mut errors = Vec::new();
    // Put the type information in first.
    for ConveyStruct { name, span, fields } in structs {
        debug!("Interning struct {}", name);
        let _ =
            inter_struct(name, fields, span, source, lu_dog, models, sarzak).map_err(|mut e| {
                errors.append(&mut e);
            });
    }

    // Using the type information, and the input, inter the implementation blocks.
    for ConveyImpl { name, span, funcs } in implementations {
        debug!("Interning implementation {}", name);
        let _ = inter_implementation(name, funcs, span, source, lu_dog, models, sarzak).map_err(
            |mut e| {
                errors.append(&mut e);
            },
        );
    }

    // Finally, inter the loose functions.
    for ConveyFunc {
        name,
        span,
        params,
        return_type,
        statements,
    } in funcs
    {
        debug!("Interning function {}", name);
        let _ = inter_func(
            name,
            params,
            return_type,
            statements,
            None,
            None,
            span,
            source,
            true,
            lu_dog,
            models,
            sarzak,
        )
        .map_err(|mut e| errors.append(&mut e));
    }

    // 🚧 Is this a good place for this?
    if let Some(out_dir) = out_dir {
        // Now write a file containing the WoogStruct id's.
        let mut path = PathBuf::from(out_dir);
        // 🚧 This needs to be changed.
        path.push("woog_structs.rs");

        let mut file = File::create(&path).unwrap();
        writeln!(file, "use uuid::{{uuid, Uuid}};\n").unwrap();

        for ws in lu_dog.iter_woog_struct() {
            let ws = s_read!(ws);
            writeln!(
                file,
                "pub(crate) const {}_TYPE_UUID: Uuid = uuid!(\"{}\");",
                ws.name.to_shouty_snake_case(),
                ws.id
            )
            .unwrap();
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[allow(clippy::too_many_arguments)]
fn inter_func(
    name: &str,
    params: &[(Spanned<String>, Spanned<Type>)],
    return_type: &Spanned<Type>,
    stmts_p: &Spanned<ParserExpression>,
    impl_block: Option<&RefType<Implementation>>,
    impl_ty: Option<&RefType<ValueType>>,
    span: &Span,
    source: &RefType<DwarfSourceFile>,
    check_types: bool,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_func {}", name);

    let block = Block::new(Uuid::new_v4(), None, lu_dog);

    let name = name.de_sanitize();
    let stmts = if let ParserExpression::Block(stmts) = &stmts_p.0 {
        stmts
    } else {
        // I don't even think that this is possible given the parser.
        unreachable!()
    };

    let ret_ty = get_value_type(
        &return_type.0,
        &return_type.1,
        impl_ty,
        lu_dog,
        models,
        sarzak,
    )?;

    let func = Function::new(name.to_owned(), &block, impl_block, &ret_ty, lu_dog);
    let _ = WoogItem::new_function(source, &func, lu_dog);
    // Create a type for our function
    let ty = ValueType::new_function(&func, lu_dog);
    LuDogSpan::new(
        span.end as i64,
        span.start as i64,
        source,
        Some(&ty),
        None,
        lu_dog,
    );

    let mut errors = Vec::new();
    let mut last_param_uuid: Option<usize> = None;
    for ((param_name, name_span), (param_ty, param_span)) in params {
        debug!("param name {}", param_name);
        debug!("param ty {}", param_ty);

        let param = Parameter::new(&func, None, lu_dog);

        debug!("param {:?}", param);

        let var = Variable::new_parameter(param_name.to_owned(), &param, lu_dog);
        debug!("var {:?}", var);
        // We need to introduce the values into the block, so that we don't
        // error out when parsing the statements.
        //
        let param_ty = match get_value_type(param_ty, param_span, impl_ty, lu_dog, models, sarzak) {
            Ok(ty) => ty,
            Err(mut e) => {
                errors.append(&mut e);
                continue;
            }
        };
        debug!("param_ty {:?}", param_ty);
        let value = XValue::new_variable(&block, &param_ty, &var, lu_dog);
        LuDogSpan::new(
            name_span.end as i64,
            name_span.start as i64,
            source,
            None,
            Some(&value),
            lu_dog,
        );
        last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
    }

    let stmts: Vec<RefType<ParserStatement>> = stmts
        .iter()
        .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
        .collect();

    let (block_ty, block_span) = inter_statements(
        &stmts,
        &stmts_p.1,
        &block,
        check_types,
        source,
        lu_dog,
        models,
        sarzak,
    )?;

    if check_types {
        typecheck(
            (&ret_ty, span),
            (&block_ty, &block_span),
            location!(),
            lu_dog,
            sarzak,
            models,
        )?;
    }

    debug!("func {name} saved");

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn inter_statement(
    stmt: &RefType<ParserStatement>,
    source: &RefType<DwarfSourceFile>,
    block: &RefType<Block>,
    check_types: bool,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(Spanned<RefType<Statement>>, RefType<ValueType>)> {
    debug!("inter_statement {:?}", stmt);

    match &*s_read!(stmt) {
        //
        // Expression
        //
        ParserStatement::Expression((expr, span)) => {
            let (expr, _) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                check_types,
                span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ExpressionStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_expression_statement(block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Item
        //
        ParserStatement::Item((item, span)) => {
            match item {
                // Item::Function((name, _name_span), params, return_type, stmts) => {
                Item::Function(ref name, ref params, ref return_type, ref stmts) => inter_func(
                    &name.0,
                    params,
                    return_type,
                    stmts,
                    None,
                    None,
                    span,
                    source,
                    check_types,
                    lu_dog,
                    models,
                    sarzak,
                )?,
                Item::Implementation((name, _name_span), funcs) => {
                    inter_implementation(name, funcs, span, source, lu_dog, models, sarzak)?
                }
                // Item::Import((path, _path_span), alias) => {
                //     inter_import(path, alias, &s_read!(source).source, span, lu_dog)?
                // }
                Item::Struct((name, span), fields) => {
                    inter_struct(name, fields, span, source, lu_dog, models, sarzak)?
                }
                _ => unimplemented!(),
            };
            let _stmt = ItemStatement::new();
            let stmt = Statement::new_item_statement(block, None, lu_dog);
            Ok(((stmt, span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Let
        //
        ParserStatement::Let((var_name, _var_span), type_, (expr, expr_span)) => {
            // Setup the local variable that is the LHS of the statement.
            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            let var = Variable::new_local_variable(var_name.to_owned(), &local, lu_dog);

            // Now parse the RHS, which is an expression.
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                check_types,
                expr_span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let ty = if let Some((type_, span)) = type_ {
                if check_types {
                    let lhs_ty = type_.into_value_type(span, lu_dog, models, sarzak)?;
                    typecheck(
                        (&lhs_ty, span),
                        (&ty, expr_span),
                        location!(),
                        lu_dog,
                        sarzak,
                        models,
                    )?;
                    lhs_ty
                } else {
                    ty
                }
            } else {
                ty
            };

            // 🚧
            // Let's keep an eye on this. I've had the notion of having a separate
            // entry point for the REPL, and conditionally needing to generate an
            // error would support the idea.
            if let ValueTypeEnum::Unknown(_) = s_read!(ty).subtype {
                warn!("Unknown type for variable {}", var_name);
            }

            // Create a variable, now that we have a type from the expression.
            let _value = XValue::new_variable(block, &ty, &var, lu_dog);
            // 🚧 Was this causing a crash?
            // LuDogSpan::new(
            //     var_span.end as i64,
            //     var_span.start as i64,
            //     source,
            //     Some(&value),
            //     None,
            //     lu_dog,
            // );

            // Setup the let statement itself.
            let stmt = LetStatement::new(&expr.0, &local, lu_dog);
            let stmt = Statement::new_let_statement(block, None, &stmt, lu_dog);

            Ok(((stmt, expr_span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Result
        //
        ParserStatement::Result((ref expr, span)) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                check_types,
                span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ResultStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_result_statement(block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ty))
        }
        道 => todo!("{:?}", 道),
    }
}

#[allow(clippy::too_many_arguments)]
fn inter_statements(
    statements: &[RefType<ParserStatement>],
    span: &Span,
    block: &RefType<Block>,
    check_types: bool,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<Spanned<RefType<ValueType>>> {
    let mut value_type = ValueType::new_empty(lu_dog);
    let mut span = span.to_owned();
    let mut errors = Vec::new();

    let mut last_stmt_uuid: Option<usize> = None;
    for stmt in statements {
        let (stmt, ty) =
            match inter_statement(stmt, source, block, check_types, lu_dog, models, sarzak) {
                Ok((stmt, ty)) => (stmt, ty),
                Err(err) => {
                    errors.extend(err);
                    continue;
                }
            };
        if last_stmt_uuid.is_none() {
            s_write!(block).statement = Some(s_read!(stmt.0).id);
        }
        if s_read!(block).statement.is_none() {
            s_write!(block).statement = Some(s_read!(stmt.0).id);
        }
        last_stmt_uuid = link_statement!(last_stmt_uuid, stmt.0, lu_dog);
        value_type = ty;
        span = stmt.1;
    }

    if errors.is_empty() {
        Ok((value_type, span))
    } else {
        Err(errors)
    }
}

/// I have a feeling that this one is going to be intense...
/// Actually, maybe not. There's not much happening just when we are populating.
/// It should get intense when we are evaluating for the SVM.
/// I may have spoken too soon. We need to be populating the store, in addition
/// to returning the type. Duh. And we should return the expression so that we
/// can create a value from it.
#[allow(clippy::too_many_arguments)]
fn inter_expression(
    expr: &RefType<ParserExpression>,
    check_types: bool,
    span: &Span,
    source: &RefType<DwarfSourceFile>,
    block: &RefType<Block>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("expr {:?}", expr);
    debug!("span {:?}", span);
    debug!("check_types {check_types}");

    let span = LuDogSpan::new(
        span.end as i64,
        span.start as i64,
        source,
        None,
        None,
        lu_dog,
    );

    match &*s_read!(expr) {
        //
        // Addition
        //
        ParserExpression::Addition(ref lhs_p, ref rhs_p) => {
            debug!("Addition");
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same.
            // 🚧 We also need to check that the type supports addition.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Binary::new_addition(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // And
        //
        ParserExpression::And(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if let ValueTypeEnum::Ty(ref id) = s_read!(lhs_ty).subtype {
                let ty = sarzak.exhume_ty(id).unwrap();
                matches!(ty, Ty::Boolean(_));
            } else {
                let lhs = PrintableValueType(&lhs_ty, lu_dog, sarzak, models);
                return Err(vec![DwarfError::TypeMismatch {
                    found: lhs.to_string(),
                    expected: "bool".to_string(),
                    found_span: lhs_p.1.to_owned(),
                    expected_span: rhs_p.1.to_owned(),
                    location: location!(),
                }]);
            }

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = BooleanOperator::new_and(lu_dog);
            let expr = Binary::new_boolean_operator(&expr, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // As
        //
        ParserExpression::As(ref expr, ref ty) => {
            let (expr, _expr_ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let as_type = get_value_type(&ty.0, &ty.1, None, lu_dog, models, sarzak)?;
            let as_op = TypeCast::new(&expr.0, &as_type, lu_dog);
            let expr = Expression::new_type_cast(&as_op, lu_dog);
            let value = XValue::new_expression(block, &as_type, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), as_type))
        }
        //
        // Asm
        //
        // ParserExpression::Asm(ref exprs) => {
        //     let mut exprs = exprs
        //         .iter()
        //         .map(|expr| {
        //             inter_expression(
        //                 &new_ref!(ParserExpression, expr.0.to_owned()),
        //                 &expr.1,
        //                 source,
        //                 block,
        //                 lu_dog,
        //                 models,
        //                 sarzak,
        //             )
        //         })
        //         .collect::<Result<Vec<_>, _>>()?;

        //     let static_method_call =
        //         StaticMethodCall::new("execute_asm".to_owned(), "chacha".to_owned(), lu_dog);
        //     //     &exprs
        //     //         .iter()
        //     //         .map(|expr| &expr.0)
        //     //         .collect::<Vec<_>>()
        //     //         .as_slice(),
        //     //     lu_dog,
        //     // );
        // }
        //
        // Assignment
        //
        ParserExpression::Assignment(ref lhs_p, ref rhs_p) => {
            // dbg!("raw", &lhs_p, &rhs_p);
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Binary::new_assignment(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // Bang
        //
        ParserExpression::Bang(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let not = Unary::new_not(lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &not, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Block
        //
        ParserExpression::Block(ref stmts) => {
            let block = Block::new(Uuid::new_v4(), None, lu_dog);
            debug!("block {block:?}");
            let stmts_vec: Vec<RefType<ParserStatement>> = stmts
                .iter()
                .map(|stmt| new_ref!(ParserStatement, stmt.0.to_owned()))
                .collect();
            // 🚧 The one that's commented out is correct -- assuming the block isn't `{}`.
            // The one that isn't commented out _should_ be right, but I'm not sure that it is.
            // let stmts_span = stmts.iter().map(|stmt| stmt.1.start).min().unwrap()
            //     ..stmts.iter().map(|stmt| stmt.1.end).max().unwrap();
            let stmts_span = s_read!(span).start as usize..s_read!(span).end as usize;

            let expr = Expression::new_block(&block, lu_dog);
            let ty = inter_statements(
                &stmts_vec,
                &stmts_span,
                &block,
                check_types,
                source,
                lu_dog,
                models,
                sarzak,
            )?;
            let value = XValue::new_expression(&block, &ty.0, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            debug!("block {expr:?}");
            Ok(((expr, span), ty.0))
        }
        //
        // BooleanLiteral
        //
        ParserExpression::BooleanLiteral(literal) => {
            let literal = if *literal {
                BooleanLiteral::new_true_literal(lu_dog)
            } else {
                BooleanLiteral::new_false_literal(lu_dog)
            };
            let expr =
                Expression::new_literal(&Literal::new_boolean_literal(&literal, lu_dog), lu_dog);
            let ty = ValueType::new_ty(&Ty::new_boolean(), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Debug
        //
        ParserExpression::Debug => {
            let expr = Expression::new_debugger(lu_dog);
            let ty = ValueType::new_empty(lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Division
        //
        ParserExpression::Division(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We also need to check that the type supports division.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Binary::new_division(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        // 🚧 This doesn't exist. And I don't want to create a BS type for it in
        // lu_dog. Because really the "empty" type is just an empty tuple. I
        // think I want tuples, so I'm just going to assert that the user can't
        // construct the empty type.
        // //
        // // Empty
        // //
        // ParserExpression::Empty => {
        //     let expr = Expression::new_empty(lu_dog);
        //     let ty = ValueType::new_empty(lu_dog);
        //     let value = XValue::new_expression(block, &ty, &expr, lu_dog);
        //     s_write!(span).x_value = Some(s_read!(value).id);

        //     Ok(((expr, span), ty))
        // }
        //
        // Error
        //
        ParserExpression::Error => {
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            //
            // This error stuff really needs fleshing out:
            //
            //  💥 The parser needs to capture the span of the error, and we need to use it
            //    to look up the text in the source code. That, or we need to capture the
            //    source in the error.
            //  💥 If we look it up in the source, then we need to capture that. I'm part-
            //    way to having that done, as there is a place to do that in the model now.
            //  💥 We need to return an `ErrorExpression`. Right now it's got an attribute
            //     called `span` that's a String. Probably rename that, but leave it otherwise.
            //  💥 We need to figure out what's up with `ValueType::Error`. I plugged some
            //     shit in a while back when stubbing something out, but I didn't put much
            //     thought into it.
            //
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            let error = ErrorExpression::new(
                "🚧 Under Construction 🚧\n💥 Input is fucked up for some reason 💥\n".to_owned(),
                lu_dog,
            );
            let expr = Expression::new_error_expression(&error, lu_dog);
            // 🚧
            // Returning an empty, because the error stuff in ValueType is fucked.
            let ty = ValueType::new_empty(lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Equals
        //
        ParserExpression::Equals(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // FieldAccess
        //
        ParserExpression::FieldAccess(lhs, rhs) => {
            debug!("ParserExpression::FieldAccess lhs {:?}", lhs);
            debug!("ParserExpression::FieldAccess rhs {:?}", rhs);

            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.clone()),
                check_types,
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let id = s_read!(lhs_ty).id;
            let ty = lu_dog.exhume_value_type(&id).unwrap();
            let ty_read = s_read!(ty);

            match &ty_read.subtype {
                // We matched on the lhs type.
                ValueTypeEnum::Function(ref _id) => {
                    // let func = lu_dog.exhume_function(id).unwrap();
                    // let impl_ = &s_read!(func).r9_implementation(lu_dog)[0];
                    // let woog_struct = &s_read!(impl_).r8_woog_struct(lu_dog)[0];
                    // let fat = FieldAccessTarget::new_function(&func, lu_dog);
                    // let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    // let expr = Expression::new_field_access(&expr, lu_dog);
                    // let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                    // let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    // s_write!(span).x_value = Some(s_read!(value).id);

                    Ok((lhs, ty.clone()))
                }
                ValueTypeEnum::WoogStruct(ref id) => {
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();

                    if let Some(field) =
                        lu_dog.exhume_field_id_by_name(&rhs.0.to_upper_camel_case())
                    {
                        let field = lu_dog.exhume_field(&field);
                        // let field = woog_struct.r7_field(lu_dog);
                        // let field = field.iter().find(|f| s_read!(f).name == rhs);
                        let func = if let Some(impl_) =
                            s_read!(woog_struct).r8c_implementation(lu_dog).pop()
                        {
                            let impl_ = s_read!(impl_);

                            let funcs = impl_.r9_function(lu_dog);
                            funcs.iter().find(|f| s_read!(f).name == rhs.0).cloned()
                        } else {
                            None
                        };

                        // We need to grab the type from the field: what we have above is the type
                        // of the struct.
                        if let Some(field) = field {
                            let fat = FieldAccessTarget::new_field(&field, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(&expr, lu_dog);
                            let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            s_write!(span).x_value = Some(s_read!(value).id);

                            Ok(((expr, span), ty))
                        } else if let Some(func) = func {
                            let fat = FieldAccessTarget::new_function(&func, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(&expr, lu_dog);
                            let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            s_write!(span).x_value = Some(s_read!(value).id);

                            Ok(((expr, span), ty))
                        } else {
                            let span = s_read!(span);
                            let span = span.start as usize..span.end as usize;
                            Err(vec![DwarfError::StructFieldNotFound {
                                field: rhs.0.clone(),
                                span,
                            }])
                        }
                    } else {
                        Err(vec![DwarfError::StructFieldNotFound {
                            field: rhs.0.clone(),
                            span: rhs.1.to_owned(),
                        }])
                    }
                }
                ty => {
                    // 🚧 This should be a DwarfError.
                    let error =
                        ErrorExpression::new(format!("💥 {:?} is not a struct\n", ty), lu_dog);
                    let expr = Expression::new_error_expression(&error, lu_dog);
                    // 🚧 Returning an empty, because the error stuff in ValueType is fucked.
                    // I wonder what we mean?
                    let ty = ValueType::new_empty(lu_dog);
                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    s_write!(span).x_value = Some(s_read!(value).id);

                    Ok(((expr, span), ty))
                }
            }
        }
        //
        // FloatLiteral
        //
        ParserExpression::FloatLiteral(literal) => {
            let expr = Expression::new_literal(
                &Literal::new_float_literal(&FloatLiteral::new(*literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(&Ty::new_float(), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // For Loop
        //
        ParserExpression::For(iter, collection, body) => {
            debug!("For");

            let cspan = &collection.1;
            let collection = new_ref!(ParserExpression, collection.0.clone());

            // 🚧 Should we be checking this to ensure that it's an iterable?
            // Yes, duh. It won't be too much work...
            let (collection, _collection_ty) = inter_expression(
                &collection,
                check_types,
                cspan,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let bspan = &body.1;
            let body = new_ref!(ParserExpression, (body.0).to_owned());

            // Note to future self!
            //
            // The commented out code is what I'd like to insert into the block
            // body that I'm creating just below. The problem is that I need the
            // body before it's been interred. So instead I turn off type checking.
            // Thats the best I could come up with. I'd like a better resolution.
            //
            // One thing I could do to make it less sucky would be to disable
            // checking for _just_ the iterator var.
            //
            // In general a better solution would involve being able to pass
            // the `XValue` with a `None` in place of a body. And then _somehow
            // use_ it _inside_ the boy.
            //
            // let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            // let var = Variable::new_local_variable(iter.0.to_owned(), &local, lu_dog);
            // let _value = XValue::new_variable(&body, &collection_ty, &var, lu_dog);

            let (body, _body_ty) =
                inter_expression(&body, false, bspan, source, block, lu_dog, models, sarzak)?;

            let body = s_read!(body.0);
            let body = if let ExpressionEnum::Block(body) = &body.subtype {
                body
            } else {
                unreachable!()
            };
            let body = lu_dog.exhume_block(body).unwrap();

            let for_loop = ForLoop::new(iter.0.to_owned(), &body, &collection.0, lu_dog);
            let expr = Expression::new_for_loop(&for_loop, lu_dog);
            let ty = ValueType::new_empty(lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // FunctionCall
        //
        ParserExpression::FunctionCall(func, params) => {
            debug!("func {:?}", func);
            let fspan = &func.1;
            let func = &func.0;
            debug!("params {:?}", params);

            // I think that we need to see if we have the function definition
            // in memory. This is actually tricky, because theoretically we will
            // eventually have either the function definition or a pointer to
            // some external function. Either way we should be able to look up the
            // return type. We only need to resort to interring the expression
            // if we con't find it, which means it's a local variable, and we
            // can go that route.
            //
            // The problem, currently, is that not only do we not really resolve
            // external references, but we also don't parse all the function
            // signatures into memory before we parse their bodies.
            //
            // 🚧 Deal with the above situation.
            let (func_expr, ret_ty) = inter_expression(
                &new_ref!(ParserExpression, func.to_owned()),
                check_types,
                fspan,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if let ValueTypeEnum::Unknown(_) = s_read!(ret_ty).subtype {
                // Here's where we need to lookup the function definition.
                // panic!("🚧 we need a function definition");
            }

            let func_call = Call::new_function_call(false, Some(&func_expr.0), lu_dog);
            let func = Expression::new_call(&func_call, lu_dog);
            let value = XValue::new_expression(block, &ret_ty, &func, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            let mut last_arg_uuid: Option<usize> = None;
            for param in params {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, param.0.to_owned()),
                    check_types,
                    &param.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let arg = Argument::new(&arg_expr.0, &func_call, None, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "ParserExpression::FunctionCall exit {:?}",
                (&func_call, s_read!(func_call).r28_argument(lu_dog))
            );

            debug!(
                "return type {}",
                PrintableValueType(&ret_ty, lu_dog, sarzak, models).to_string()
            );

            Ok(((func, span), ret_ty))
        }
        //
        // GreaterThan: >
        //
        ParserExpression::GreaterThan(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_greater_than(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // GreaterThanOrEqual: >=
        //
        ParserExpression::GreaterThanOrEqual(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_greater_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Group
        //
        ParserExpression::Group(ref expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            Ok((expr, ty))
        }
        //
        // If
        //
        ParserExpression::If(conditional, true_block, false_block) => {
            debug!("conditional {:?}", conditional);
            let cspan = &conditional.1;
            let conditional = new_ref!(ParserExpression, conditional.0.to_owned());
            let (conditional, conditional_ty) = inter_expression(
                &conditional,
                check_types,
                cspan,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            debug!("ParserExpression::If {:?}", conditional_ty);

            // Check that the conditional expression evaluates to a boolean.
            // Note that this first check is necessary to unwrap the sarzak type
            // from the lu_dog type.
            if let ValueTypeEnum::Ty(ref ty) = s_read!(conditional_ty).subtype {
                let s_ty = sarzak.exhume_ty(ty).unwrap();
                if let Ty::Boolean(_) = s_ty {
                    // Good Times.
                } else {
                    let ty = PrintableValueType(&conditional_ty, lu_dog, sarzak, models);
                    return Err(vec![DwarfError::TypeMismatch {
                        expected: "boolean".to_owned(),
                        found: ty.to_string(),
                        expected_span: cspan.to_owned(),
                        found_span: cspan.to_owned(),
                        location: location!(),
                    }]);
                }
            } else {
                let ty = PrintableValueType(&conditional_ty, lu_dog, sarzak, models);
                return Err(vec![DwarfError::TypeMismatch {
                    expected: "boolean".to_owned(),
                    found: ty.to_string(),
                    expected_span: cspan.to_owned(),
                    found_span: cspan.to_owned(),
                    location: location!(),
                }]);
            }

            let tspan = &true_block.1;
            let true_block = new_ref!(ParserExpression, true_block.0.to_owned());
            let (true_block, true_ty) = inter_expression(
                &true_block,
                check_types,
                tspan,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let true_block =
                if let ExpressionEnum::Block(true_block) = s_read!(true_block.0).subtype {
                    true_block
                } else {
                    panic!("Expected a block expression");
                };
            let true_block = lu_dog.exhume_block(&true_block).unwrap();

            let false_block = if let Some(false_block) = false_block {
                let fspan = &false_block.1;
                let false_block = new_ref!(ParserExpression, false_block.0.to_owned());
                let (false_block, _false_ty) = inter_expression(
                    &false_block,
                    check_types,
                    fspan,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let false_block =
                    if let ExpressionEnum::Block(false_block) = s_read!(false_block.0).subtype {
                        false_block
                    } else {
                        panic!("Expected a block expression");
                    };
                let false_block = lu_dog.exhume_block(&false_block).unwrap();
                Some(false_block)
            } else {
                None
            };

            let if_expr = XIf::new(false_block.as_ref(), &conditional.0, &true_block, lu_dog);
            let expr = Expression::new_x_if(&if_expr, lu_dog);

            let ty = true_ty;

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Index
        //
        ParserExpression::Index(target, index_p) => {
            let (target, _target_ty) = inter_expression(
                &new_ref!(ParserExpression, target.0.to_owned()),
                check_types,
                &target.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (index, index_ty) = inter_expression(
                &new_ref!(ParserExpression, index_p.0.to_owned()),
                check_types,
                &index_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let int_ty = ValueType::new_ty(&Ty::new_integer(), lu_dog);
            if check_types {
                // let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
                let index_span = s_read!(index.1).start as usize..s_read!(index.1).end as usize;
                typecheck(
                    (&int_ty, &index_span),
                    (&index_ty, &index_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let index = Index::new(&index.0, &target.0, lu_dog);

            let expr = Expression::new_index(&index, lu_dog);
            let value = XValue::new_expression(block, &int_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);
            // 🚧 We should really check that the target type is some sort of list.

            Ok(((expr, span), int_ty))
        }
        //
        // IntegerLiteral
        //
        ParserExpression::IntegerLiteral(literal) => {
            let expr = Expression::new_literal(
                &Literal::new_integer_literal(&IntegerLiteral::new(*literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(&Ty::new_integer(), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Lambda
        //
        ParserExpression::Lambda(params, return_type, body) => {
            let block = Block::new(Uuid::new_v4(), None, lu_dog);
            let stmts = if let ParserExpression::Block(body) = &body.0 {
                body
            } else {
                unreachable!();
            };

            let ret_ty =
                get_value_type(&return_type.0, &return_type.1, None, lu_dog, models, sarzak)?;

            let lambda = Lambda::new(&block, &ret_ty, lu_dog);
            let _ = ValueType::new_lambda(&lambda, lu_dog);

            let mut errors = Vec::new();
            let mut last_param_uuid: Option<usize> = None;
            for ((param_name, name_span), (param_ty, param_span)) in params {
                debug!("param name {}", param_name);
                debug!("param ty {}", param_ty);

                let param = LambdaParameter::new(&lambda, None, lu_dog);

                debug!("param {:?}", param);

                let var = Variable::new_lambda_parameter(param_name.to_owned(), &param, lu_dog);
                debug!("var {:?}", var);
                // We need to introduce the values into the block, so that we don't
                // error out when parsing the statements.
                //
                let param_ty =
                    match get_value_type(param_ty, param_span, None, lu_dog, models, sarzak) {
                        Ok(ty) => ty,
                        Err(mut e) => {
                            errors.append(&mut e);
                            continue;
                        }
                    };
                debug!("param_ty {:?}", param_ty);
                let value = XValue::new_variable(&block, &param_ty, &var, lu_dog);
                LuDogSpan::new(
                    name_span.end as i64,
                    name_span.start as i64,
                    source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
            }

            let stmts: Vec<RefType<ParserStatement>> = stmts
                .iter()
                .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
                .collect();

            let (block_ty, block_span) = inter_statements(
                &stmts,
                &body.1,
                &block,
                check_types,
                source,
                lu_dog,
                models,
                sarzak,
            )?;

            if check_types {
                typecheck(
                    (&ret_ty, &return_type.1),
                    (&block_ty, &block_span),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Expression::new_lambda(&lambda, lu_dog);
            let value = XValue::new_expression(&block, &ret_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ret_ty))
        }
        //
        // LessThan: <
        //
        ParserExpression::LessThan(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_less_than(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // LessThanOrEqual
        //
        ParserExpression::LessThanOrEqual(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_less_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // List
        //
        ParserExpression::List(ref elements) => {
            if elements.is_empty() {
                let list = List::new(&ValueType::new_empty(lu_dog), lu_dog);
                let expr =
                    Expression::new_list_expression(&ListExpression::new(None, lu_dog), lu_dog);
                let ty = ValueType::new_list(&list, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);

                Ok(((expr, span), ty))
            } else {
                let mut elements = elements.iter();
                // I'm going to get the type of the first element, and then check
                // that each subsequent element is the same type.
                let element = elements.next().unwrap();
                let span1 = &element.1;
                let ((first, first_span), first_ty) = inter_expression(
                    &new_ref!(ParserExpression, element.0.to_owned()),
                    check_types,
                    &element.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;

                let list = List::new(&first_ty, lu_dog);
                let element = ListElement::new(&first, None, lu_dog);
                let expr = Expression::new_list_element(&element, lu_dog);
                let value = XValue::new_expression(block, &first_ty, &expr, lu_dog);
                s_write!(first_span).x_value = Some(s_read!(value).id);
                let list_expr = ListExpression::new(Some(&element), lu_dog);

                let mut last_element_uuid: Option<usize> = Some(s_read!(element).id);
                for element in elements {
                    let ((elt, elt_span), elt_ty) = inter_expression(
                        &new_ref!(ParserExpression, element.0.to_owned()),
                        check_types,
                        &element.1,
                        source,
                        block,
                        lu_dog,
                        models,
                        sarzak,
                    )?;

                    if check_types {
                        typecheck(
                            (&first_ty, span1),
                            (&elt_ty, &element.1),
                            location!(),
                            lu_dog,
                            sarzak,
                            models,
                        )?;
                    }

                    let element = ListElement::new(&elt, None, lu_dog);
                    last_element_uuid = link_list_element!(last_element_uuid, element, lu_dog);
                    let expr = Expression::new_list_element(&element, lu_dog);
                    let value = XValue::new_expression(block, &elt_ty, &expr, lu_dog);
                    s_write!(elt_span).x_value = Some(s_read!(value).id);
                }

                let expr = Expression::new_list_expression(&list_expr, lu_dog);
                let ty = ValueType::new_list(&list, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                s_write!(first_span).x_value = Some(s_read!(value).id);

                Ok(((expr, first_span), ty))
            }
        }
        //
        // LocalVariable
        //
        ParserExpression::LocalVariable(name) => {
            // We need to return an expression and a type.
            debug!("local variable name {}", name);
            // We look for a value in the current block. We need to clone them
            // to be able to modify lu_dog below.
            //
            // So, multiple let statements will result in multiple values. We only
            // need one -- and it needs to be the right one...
            // To expound, there are likely to be multiple values in this block,
            // and we need to find the one that matches the variable name.
            //
            // ⚡️ Oh shit -- I'm in the compiler!!!
            //
            // So what's happening? We hit a local variable node in the ast. We need
            // to create
            //
            let values = lu_dog
                .iter_x_value()
                .filter(|value| s_read!(value).block == s_read!(block).id)
                .collect::<Vec<RefType<XValue>>>();

            // debug!("values", values);

            let types = lu_dog.iter_value_type().collect::<Vec<_>>();
            // Now search for a value that's a Variable, and see if the access matches
            // the variable.
            let mut expr_type_tuples = values
                .iter()
                .inspect(|v| {
                    let ty = s_read!(v).ty;
                    debug!("value: {v:#?}, type: {:#?}", types[ty]);
                })
                .filter_map(|value| {
                    let value = s_read!(value);
                    match value.subtype {
                        XValueEnum::Expression(ref _expr) => {
                            // let expr = lu_dog.exhume_expression(expr).unwrap();
                            // error!("we don't expect to be here", expr);
                            // So we get here after all.
                            // Must. Remember. In. Compiler.
                            // So we need to create some nodes here. And return an expression
                            // and a type.
                            //
                            // Still wondering how we get here. Debugging is showing that we've
                            // got a Literal expression. But why's it showing up as a LocalVariable?
                            // I got here by entering `Point::new(5, a)` in the interpreter. `a` is
                            // an Inflection instance. I need to turn on logging and sort this out.
                            //
                            // Fuck me. I've been debugging something that's completely normal. What's
                            // going on is that there are a bunch of values in the block --
                            // especially when running the interpreter. So we are iterating over
                            // them all, and we are bound to find some that aren't variable expressions
                            // even though we are parsing a LocalVariable. Remember these are all of
                            // the values -- not just the ones that have something to do with finding
                            // ourselves here.
                            //
                            // Hopefully this is concluded.
                            //

                            None
                        }
                        XValueEnum::Variable(ref var) => {
                            let var = s_read!(lu_dog.exhume_variable(var).unwrap()).clone();
                            debug!("value var {:?}", var);
                            // Check the name
                            if var.name == *name {
                                match var.subtype {
                                    VariableEnum::LocalVariable(_) |
                                    VariableEnum::Parameter(_) |
                                    VariableEnum::LambdaParameter(_)=> {
                                        // let value =
                                            // s_read!(var.r11_x_value(lu_dog)[0]).clone();
                                        let ty = value.r24_value_type(lu_dog)[0].clone();

                                        let lhs_ty =
                                            PrintableValueType(&ty, lu_dog, sarzak, models);

                                        debug!("{}, {:?}, {}", name, &value, lhs_ty.to_string());

                                        let expr = lu_dog
                                            .iter_variable_expression()
                                            .find(|expr| s_read!(expr).name == *name);

                                        let expr = if let Some(expr) = expr {
                                            s_read!(expr).r15_expression(lu_dog)[0].clone()
                                        } else {
                                            let expr =
                                                VariableExpression::new(name.to_owned(), lu_dog);
                                            debug!("created a new variable expression {:?}", expr);
                                            Expression::new_variable_expression(&expr, lu_dog)
                                        };

                                        let value =
                                            XValue::new_expression(block, &ty, &expr, lu_dog);
                                        s_write!(span).x_value =
                                            Some(s_read!(value).id);


                                        debug!("expr {expr:?}, value {value:?}, type {ty:?}, span {span:?}");

                                        Some(((expr, span.clone()), ty))
                                    }
                                }
                            } else {
                                None
                            }
                        }
                    }
                })
                .collect::<Vec<(
                    (RefType<Expression>, RefType<LuDogSpan>),
                    RefType<ValueType>,
                )>>();
            // There should be zero or 1 results.
            // Actually there are `n`, where `n` is the number of values in the block,
            // which is equivalent to the number of `let` statements.
            //
            // -- or not (the let statement thing)--
            //
            // I'm figuring out assignment right now, and doing that I'm looking at
            // what this returns, and how it works. Here's the setup:
            //
            // ```
            // let b = "yes";
            // let c = b;
            // b = "no";
            // ```
            //
            // `b` is being evaluated in the assignment handler. It's getting back
            // "yes", which I guess is correct, but they type is `()`. So that's weird.
            // Also, there are two different places that b shows up. I'm taking the
            // last one, which maybe corresponds to `b` being in rhs of the previous
            // storage allocation. I'm figuring that out now. In that case, it sort
            // of makes sense that the type is `()`.
            //
            // My sieve isn't sufficient. I'm right, there should be one per `let`.
            // WHats going on here is that we have been returned values that aren't
            // necessarily storage locations. So up above we need to filter values
            // that are only local variables.
            //
            // So, yeah, we always want to grab the last one.
            // debug_assert!(expr_type_tuples.len() <= 1);

            debug!("expr_ty {:?}", expr_type_tuples);

            // Why are we taking the last one? -- Oh, read above.
            if let Some(expr_ty_tuple) = expr_type_tuples.pop() {
                debug!("returning {:?}", expr_ty_tuple);
                Ok(expr_ty_tuple)
            } else if let Some(ref id) =
                lu_dog.exhume_function_id_by_name(&name.to_upper_camel_case())
            {
                // 🚧 The exhumation above is sort of messed up. I don't like that
                // I have to turn it into upper-camel-case.
                // We get here because there was no local variable info, so we are
                // going to check if it's a function.
                // 🚧 NB: We'll only find it if it's been processed. We really need to
                // load function definitions before we start processing the block.
                // 🚧 NB: If it's imported we're screwed until that's implemented.
                //
                // Dang. We need to return an expression, and what I'd really like
                // to do is just return the function's return type. I wonder if I
                // can cheat and return a variable expression?
                debug!("found a function named {name}");
                let func = lu_dog.exhume_function(id).unwrap();
                let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(&expr, lu_dog);

                Ok(((expr, span), ty))
            } else {
                debug!("variable not found");
                // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
                // As neat as it is that I'm compiling this into the binary, we should actually
                // bail here, and make the user do something about it. The issue is that this
                // may get run from the repl, and in that case we want to return the expression.
                // So, we need a flag...
                // let expr = ErrorExpression::new(
                //     format!(
                //         "\n  ──➤  variable: `{}` not found\n",
                //         Colour::Red.paint(name.to_owned())
                //     ),
                //     lu_dog,
                // );
                // Expression::new_error_expression(&expr, lu_dog);
                // ValueType::new_error(&Error::new_unknown_variable(lu_dog), lu_dog);
                // 🚧 WTF are we doing here? Above it's an error, and here we have
                // a variable expression that we're returning?
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(&expr, lu_dog);
                let ty = ValueType::new_unknown(lu_dog);

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);

                debug!("LocalVariable result ({expr:#?}, {ty:#?})");
                Ok(((expr, span), ty))
            }
        }
        //
        // MethodCall
        //
        ParserExpression::MethodCall(instance, (method, meth_span), args) => {
            debug!("MethodCall Enter: {:?}.{method}", instance);

            let (instance, instance_ty) = inter_expression(
                &new_ref!(ParserExpression, instance.0.to_owned()),
                check_types,
                &instance.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            debug!("MethodCall inter method: expr: ty {instance:?}: {instance_ty:?}");

            let ret_ty = if let ValueTypeEnum::WoogStruct(id) = s_read!(instance_ty).subtype {
                let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
                let x = lookup_woog_struct_method_type(&s_read!(woog_struct).name, method, lu_dog);

                x
            } else if let ValueTypeEnum::Ty(id) = s_read!(instance_ty).subtype {
                let ty = sarzak.exhume_ty(&id).unwrap();
                if let Ty::SString(_) = ty {
                    match method.as_str() {
                        "len" => {
                            let ty = Ty::new_integer();
                            ValueType::new_ty(&ty, lu_dog)
                        }
                        "format" => {
                            let ty = Ty::new_s_string();
                            ValueType::new_ty(&ty, lu_dog)
                        }
                        _ => {
                            return Err(vec![DwarfError::NoSuchMethod {
                                method: method.to_owned(),
                                span: meth_span.to_owned(),
                            }])
                        }
                    }
                } else {
                    ValueType::new_unknown(lu_dog)
                }
            } else {
                ValueType::new_unknown(lu_dog)
            };

            let meth = MethodCall::new(method.to_owned(), lu_dog);
            let call = Call::new_method_call(false, Some(&instance.0), &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            let value = XValue::new_expression(block, &instance_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            let mut last_arg_uuid: Option<usize> = None;
            // This is the self parameter
            let this = Argument::new(&instance.0, &call, None, lu_dog);
            last_arg_uuid = link_argument!(last_arg_uuid, this, lu_dog);

            for arg in args {
                let (arg_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, arg.0.to_owned()),
                    check_types,
                    &arg.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let value = XValue::new_expression(block, &ty, &arg_expr.0, lu_dog);
                let _span = LuDogSpan::new(
                    arg.1.end as i64,
                    arg.1.start as i64,
                    source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                let arg = Argument::new(&arg_expr.0, &call, None, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "{} return type {}",
                Colour::Red.dimmed().italic().paint("MethodCall"),
                PrintableValueType(&ret_ty, lu_dog, sarzak, models).to_string()
            );

            // If we return unknown then things like a[a.len() - 1] work because
            // a.len() is unknown, and we let that pass knowing the interpreter
            // will catch a type mismatch. This breaks field access because the
            // extruder doesn't know that the instance is really an instance.
            // If we return the type of the instance, then the field access
            // stuff works, and the first example does not.
            //
            // I could maybe fix the field code to be ok with an unknown type.
            //
            // I think the right thing to do, and it's affecting other things,
            // like type resolution, is to look up the method in the model and
            // return it's type. We just need to rip though the method
            // signatures first, and then take care of the deets.
            //
            let _ty = ValueType::new_unknown(lu_dog);
            Ok(((expr, span), ret_ty))
        }
        //
        // Negation
        //
        ParserExpression::Negation(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let negation = Unary::new_negation(lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &negation, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // NotEquals
        //
        ParserExpression::NotEquals(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Comparison::new_not_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Print
        //
        ParserExpression::Print(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let print = Print::new(&expr.0, lu_dog);
            let expr = Expression::new_print(&print, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Multiplication
        //
        ParserExpression::Multiplication(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We also need to check that the type supports multiplication.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = Binary::new_multiplication(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // None
        //
        ParserExpression::None => {
            let ty = ValueType::new_unknown(lu_dog);
            let expr = Expression::new_z_none(lu_dog);
            let option = WoogOption::new_z_none(&ty, lu_dog);
            let ty = ValueType::new_woog_option(&option, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);

            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Or
        //
        ParserExpression::Or(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                check_types,
                &lhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                check_types,
                &rhs_p.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            if let ValueTypeEnum::Ty(ref id) = s_read!(lhs_ty).subtype {
                let ty = sarzak.exhume_ty(id).unwrap();
                matches!(ty, Ty::Boolean(_));
            } else {
                let lhs = PrintableValueType(&lhs_ty, lu_dog, sarzak, models);
                return Err(vec![DwarfError::TypeMismatch {
                    found: lhs.to_string(),
                    expected: "bool".to_string(),
                    found_span: lhs_p.1.to_owned(),
                    expected_span: rhs_p.1.to_owned(),
                    location: location!(),
                }]);
            }

            if check_types {
                typecheck(
                    (&lhs_ty, &lhs_p.1),
                    (&rhs_ty, &rhs_p.1),
                    location!(),
                    lu_dog,
                    sarzak,
                    models,
                )?;
            }

            let expr = BooleanOperator::new_or(lu_dog);
            let expr = Binary::new_boolean_operator(&expr, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // Range
        //
        ParserExpression::Range(start, end) => {
            let (start, start_ty) = inter_expression(
                &new_ref!(ParserExpression, start.0.to_owned()),
                check_types,
                &start.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (end, _end_ty) = inter_expression(
                &new_ref!(ParserExpression, end.0.to_owned()),
                check_types,
                &end.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧 Typecheck the start and end types to make sure that they are
            // both ints.

            let range = RangeExpression::new_full(Some(&start.0), Some(&end.0), lu_dog);

            let expr = Expression::new_range_expression(&range, lu_dog);
            let value = XValue::new_expression(block, &start_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), start_ty))
        }
        //
        // Return
        //
        ParserExpression::Return(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let ret = XReturn::new(&expr.0, lu_dog);
            let expr = Expression::new_x_return(&ret, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Some
        //
        ParserExpression::Some(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                check_types,
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧 Missing span...
            let value = XValue::new_expression(block, &ty, &expr.0, lu_dog);
            let some = ZSome::new(&value, lu_dog);
            let expr = Expression::new_z_some(&some, lu_dog);
            let option = WoogOption::new_z_some(&ty, &some, lu_dog);
            let ty = ValueType::new_woog_option(&option, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);

            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // StaticMethodCall
        //
        ParserExpression::StaticMethodCall(ty, (method, _), params) => {
            let type_name = if let Type::UserType((obj, _)) = ty {
                obj.de_sanitize().to_owned()
            } else {
                panic!("I don't think that we should ever see anything other than a user type here: {:?}", ty);
            };

            debug!("type_name {:?}", type_name);

            let meth = StaticMethodCall::new(method.to_owned(), type_name.to_owned(), lu_dog);
            let call = Call::new_static_method_call(false, None, &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            debug!("name {}", type_name);
            debug!("method {}", method);

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // So we are down to this. I suppose that we can check the obj against
            // what's been entered thus far. Really this should be a second pass
            // then. For now, I'm going to hack something in...
            // We could do something with the imports...
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            let ty = if type_name == "Uuid" && method == "new" {
                ValueType::new_ty(&Ty::new_s_uuid(), lu_dog)
            } else {
                debug!("ParserExpression::StaticMethodCall: looking up type {type_name}");

                lookup_woog_struct_method_type(&type_name, method, lu_dog)

                // Look up the struct in the imported models.
                // I'll revisit this model business after I get the basics working.
                // for model in models {
                //     if let Some(obj) = model.exhume_object_id_by_name(&type_name) {
                //         let id = if let Some(s) = lu_dog
                //             .iter_woog_struct()
                //             .find(|s| s_read!(s).object == Some(obj))
                //         {
                //             s_read!(s).id
                //         } else {
                //             model.exhume_ty(&obj).unwrap().id()
                //         };

                //         ty = lu_dog.exhume_value_type(&id).unwrap().clone();
                //         break;
                //     }
                // }
                // ty
            };

            let mut last_arg_uuid: Option<usize> = None;
            for param in params {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, param.0.to_owned()),
                    check_types,
                    &param.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let arg = Argument::new(&arg_expr.0, &call, None, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // StringLiteral
        //
        ParserExpression::StringLiteral(literal) => {
            debug!("literal {:?}", literal);
            let expr = Expression::new_literal(
                &Literal::new_string_literal(
                    &StringLiteral::new(literal.to_owned(), lu_dog),
                    lu_dog,
                ),
                lu_dog,
            );
            let ty = ValueType::new_ty(&Ty::new_s_string(), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Struct
        //
        ParserExpression::Struct(name, fields) => {
            let name_span = &name.1;
            let name = if let ParserExpression::LocalVariable(obj) = &name.0 {
                obj
            } else {
                return Err(vec![DwarfError::Internal {
                    description: "Expected a local variable in struct expression".to_owned(),
                    location: location!(),
                }]);
            };

            debug!("ParserExpression::Struct {}", name);

            // Here we don't de_sanitize the name, and we are looking it up in the
            // dwarf model.
            let id = match lu_dog.exhume_woog_struct_id_by_name(name) {
                Some(id) => id,
                None => {
                    return Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        span: name_span.to_owned(),
                    }])
                }
            };
            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();

            let expr = StructExpression::new(Uuid::new_v4(), &woog_struct, lu_dog);

            for (name, field_expr) in fields {
                // 🚧 Do type checking here? I don't think that I have what I need.
                let (field_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, field_expr.0.to_owned()),
                    check_types,
                    &field_expr.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                debug!("field `{name:?}` is of type `{ty:?}`, expr: {field_expr:?}");
                let field = FieldExpression::new(name.0.to_owned(), &field_expr.0, &expr, lu_dog);

                let expr = Expression::new_field_expression(&field, lu_dog);
                // Adding the following actually breaks shit.
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                s_write!(field_expr.1).x_value = Some(s_read!(value).id);
            }

            // Same name, de_sanitized, in a different model. Oh, right, this is
            // the source model. What's going on above?
            // Here we are looking up the object in one of the models. Above we
            // are pulling the struct and it's fields from the lu_dog. lol
            for model in models {
                if let Some(obj) = model.exhume_object_id_by_name(name.de_sanitize()) {
                    let ty = *model.exhume_ty(&obj).unwrap();

                    // let obj = model.exhume_object_id_by_name(name.de_sanitize()).unwrap();
                    // let ty = model.exhume_ty(&obj).unwrap();

                    let expr = Expression::new_struct_expression(&expr, lu_dog);
                    let ty = ValueType::new_ty(&ty, lu_dog);

                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    s_write!(span).x_value = Some(s_read!(value).id);

                    return Ok(((expr, span), ty));
                }
            }

            // I love that the type of the thing is the same as the thing itself.
            let expr = Expression::new_struct_expression(&expr, lu_dog);
            let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Subtraction
        //
        ParserExpression::Subtraction(ref lhs, ref rhs) => {
            debug!("Subtraction");
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                check_types,
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, _rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                check_types,
                &rhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same.
            // 🚧 We also need to check that the type supports subtraction.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let expr = Binary::new_subtraction(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        道 => {
            let source = &s_read!(source).source;
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            Err(vec![DwarfError::NoImplementation {
                missing: format!("inter_expression: {:?}", 道),
                code: source[span.clone()].to_owned(),
                span,
            }])
        }
    }
}

fn inter_import(
    _path: &[Spanned<String>],
    _alias: &Option<(String, Range<usize>)>,
    source: &str,
    span: &Range<usize>,
    _lu_dog: &mut LuDogStore,
) -> Result<()> {
    error!("Do something with the use statement");

    // let mut path_root = path;
    // path_root.pop().expect("Path root not found");
    // let path_root = path_root.join("::");
    // let obj_name = path.split("::").last().unwrap();
    // let (has_alias, alias) = if let Some((alias, _)) = alias {
    //     (true, alias.to_owned())
    // } else {
    //     (false, "".to_owned())
    // };

    // let import = Import::new(
    //     alias,
    //     has_alias,
    //     obj_name.to_owned(),
    //     path_root,
    //     None,
    //     lu_dog,
    // );
    // debug!("import", import);

    Err(vec![DwarfError::NoImplementation {
        missing: "Use statement not implemented yet".to_owned(),
        // span: path[0].1.start..path[path.len() - 1].1.end,
        span: span.to_owned(),
        code: source[span.to_owned()].to_owned(),
    }])
}

fn inter_implementation(
    name: &str,
    funcs: &[Spanned<Item>],
    span: &Span,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    let name = name.de_sanitize();

    let impl_ty = get_value_type(
        &Type::UserType((name.to_owned(), 0..0)),
        span,
        None,
        lu_dog,
        models,
        sarzak,
    )?;

    let mut errors = Vec::new();

    // 🚧 I think that this is completely wrong. We don't want to do this for
    // every model, but rather, we want to find the model that contains the
    // type.
    // for model in models {
    // let obj_id = model
    //     .exhume_object_id_by_name(name)
    //     .expect(&format!("Object {} not found", name));

    // let obj = model
    //     .exhume_object(&obj_id)
    //     .expect(&format!("Object {} not found", name));
    let id = match lu_dog.exhume_woog_struct_id_by_name(name) {
        Some(id) => id,
        None => {
            return Err(vec![DwarfError::ObjectNameNotFound {
                name: name.to_owned(),
            }]);
        }
    };

    let mt = lu_dog.exhume_woog_struct(&id).unwrap();

    let implementation = Implementation::new(&mt, lu_dog);
    let _ = WoogItem::new_implementation(source, &implementation, lu_dog);

    debug!("inter_implementation {}", name);

    for (func, span) in funcs {
        match func {
            Item::Function(ref name, ref params, ref return_type, ref stmts) => match inter_func(
                &name.0,
                params,
                return_type,
                stmts,
                Some(&implementation),
                Some(&impl_ty),
                span,
                source,
                true,
                lu_dog,
                models,
                sarzak,
            ) {
                Ok(_) => (),
                Err(mut err) => errors.append(&mut err),
            },
            _ => return Err(vec![DwarfError::ImplementationBlock { span: span.clone() }]),
        }
    }
    // }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn inter_struct(
    name: &str,
    fields: &[(Spanned<String>, Spanned<Type>)],
    _span: &Span,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_struct {}", name);
    let s_name = name.de_sanitize();

    let mut errors = Vec::new();
    if let Some((model, ref id)) = models
        .iter()
        .find_map(|model| model.exhume_object_id_by_name(s_name).map(|id| (model, id)))
        .map(|id| id.to_owned())
    {
        let obj = match model.exhume_object(id) {
            Some(obj) => obj,
            None => return Err(vec![DwarfError::ObjectIdNotFound { id: *id }]),
        };

        let mt = WoogStruct::new(
            name.to_owned(),
            Some(&new_ref!(Object, obj.to_owned())),
            lu_dog,
        );
        let _ = WoogItem::new_woog_struct(source, &mt, lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, span)) in fields {
            let name = name.de_sanitize();

            let ty = match get_value_type(type_, span, None, lu_dog, models, sarzak) {
                Ok(ty) => ty,
                Err(mut err) => {
                    errors.append(&mut err);
                    continue;
                }
            };
            let _field = Field::new(name.to_owned(), &mt, &ty, lu_dog);
        }
    } else {
        let mt = WoogStruct::new(name.to_owned(), None, lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, span)) in fields {
            let name = name.de_sanitize();

            let ty = match get_value_type(type_, span, None, lu_dog, models, sarzak) {
                Ok(ty) => ty,
                Err(mut err) => {
                    errors.append(&mut err);
                    continue;
                }
            };
            let _field = Field::new(name.to_owned(), &mt, &ty, lu_dog);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Get a Lu-Dog ValueType from a Dwarf Type
///
/// Note that the `new_*` methods on `Ty` just return `const`s. Also, the
/// `ValueType::new_ty` method takes on the id of it's subtype, so neither do
/// those take much space.
fn get_value_type(
    type_: &Type,
    span: &Span,
    enclosing_type: Option<&RefType<ValueType>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<RefType<ValueType>> {
    match type_ {
        Type::Boolean => {
            let ty = Ty::new_boolean();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::Empty => Ok(ValueType::new_empty(lu_dog)),
        Type::Float => {
            let ty = Ty::new_float();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::Integer => {
            let ty = Ty::new_integer();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::List(ref type_) => {
            let inner_type =
                get_value_type(&type_.0, &type_.1, enclosing_type, lu_dog, models, sarzak)?;
            let list = List::new(&inner_type, lu_dog);
            Ok(ValueType::new_list(&list, lu_dog))
        }
        Type::Option(ref type_) => {
            let inner_type =
                get_value_type(&type_.0, &type_.1, enclosing_type, lu_dog, models, sarzak)?;
            let option = WoogOption::new_z_none(&inner_type, lu_dog);
            Ok(ValueType::new_woog_option(&option, lu_dog))
        }
        Type::Reference(ref type_) => {
            let inner_type =
                get_value_type(&type_.0, &type_.1, enclosing_type, lu_dog, models, sarzak)?;
            // We don't know the address yet -- we'll fix it in the interpreter.
            let reference = Reference::new(Uuid::new_v4(), false, &inner_type, lu_dog);
            Ok(ValueType::new_reference(&reference, lu_dog))
        }
        Type::Self_ => match enclosing_type {
            Some(ty) => Ok(ty.clone()),
            None => Err(vec![DwarfError::BadSelf { span: span.clone() }]),
        },
        Type::String => {
            let ty = Ty::new_s_string();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::UserType(tok) => {
            let name = tok.0.de_sanitize();

            // Deal with imports
            let import = lu_dog.iter_import().find(|import| {
                let import = s_read!(import);
                import.name == name || (import.has_alias && import.alias == name)
            });

            if let Some(import) = import {
                // 🚧 Holy cow, it's been a while since I've plumbed these depths.
                // I seem to be having an interesting conversation with myself below.
                // I wonder what will come of it? TBH, I'm not sure how I'm getting
                // away with this code. I guess it's just not being used?
                //
                // Now what do we do with it? If it's something that we generated,
                // then we could load up the store, if we knew where it was, and
                // but we don't.
                //
                // I don't think that we can actually connect the dots until we've
                // generated the imports themselves. So the best we can do is leave
                // behind a breadcrumb.
                //
                // It might be sort of cool to make `Import` a `ValueType`, and then
                // just return this. `Function` and `Struct`, two out of the other
                // three `Item`s are already `ValueType`s, so it's not a stretch.
                Ok(ValueType::new_import(&import, lu_dog))
            } else if name == "Self" {
                match enclosing_type {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(vec![DwarfError::BadSelf {
                        span: tok.1.clone(),
                    }]),
                }
            } else if name == "String" {
                Ok(ValueType::new_ty(&Ty::new_s_string(), lu_dog))
            } else if name == "Uuid" {
                Ok(ValueType::new_ty(&Ty::new_s_uuid(), lu_dog))
            } else {
                for model in models {
                    // Look for the Object in the model domains first.
                    if let Some(ty) = model.iter_ty().find(|ty| match ty {
                        Ty::Object(ref obj) => {
                            let obj = model.exhume_object(obj).unwrap();
                            // We are going to cheat a little bit here. Say we have an
                            // object called `Point`. We want to be able to also handle
                            // proxy objects for `Point`. Those are suffixed with "Proxy".
                            let obj = obj.name.to_upper_camel_case();
                            obj == *name || name == format!("{}Proxy", obj)
                        }
                        _ => false,
                    }) {
                        return Ok(ValueType::new_ty(ty, lu_dog));
                    }
                }

                // Unlikely to have to reach back this far, except of course for
                // the Uuid. So, it's not unlikely; it's the least likely.
                if let Some(ty) = sarzak.iter_ty().find(|ty| match ty {
                    Ty::Object(ref obj) => {
                        let obj = sarzak.exhume_object(obj).unwrap();
                        let obj = obj.name.to_upper_camel_case();
                        obj == *name || name == format!("{}Proxy", obj)
                    }
                    _ => false,
                }) {
                    Ok(ValueType::new_ty(ty, lu_dog))
                } else if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(name) {
                    let ws = lu_dog.exhume_woog_struct(id).unwrap();
                    Ok(ValueType::new_woog_struct(&ws, lu_dog))
                } else {
                    Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        span: span.to_owned(),
                    }])
                }
            }
        }
        Type::Uuid => {
            let ty = Ty::new_s_uuid();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        道 => todo!("{:?}", 道),
    }
}

fn lookup_woog_struct_method_type(
    type_name: &str,
    method: &str,
    lu_dog: &mut LuDogStore,
) -> RefType<ValueType> {
    // Look up the type in lu_dog structs.
    if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(type_name) {
        let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
        let ty = if let Some(impl_) = s_read!(woog_struct).r8c_implementation(lu_dog).pop() {
            let impl_ = s_read!(impl_);

            let funcs = impl_.r9_function(lu_dog);
            funcs.iter().find(|f| s_read!(f).name == *method).map(|f| {
                let ret_ty = s_read!(f).return_type;
                lu_dog.exhume_value_type(&ret_ty).unwrap()
            })
        } else {
            debug!("ParserExpression type not found");
            Some(ValueType::new_unknown(lu_dog))
        };
        debug!("ParserExpression found type: {ty:?}");
        if let Some(ty) = ty {
            ty
        } else {
            ValueType::new_unknown(lu_dog)
        }
    } else {
        debug!("ParserExpression type not found");
        ValueType::new_unknown(lu_dog)
    }
}

trait DeSanitize {
    fn de_sanitize(&self) -> &str;
}

impl DeSanitize for String {
    fn de_sanitize(&self) -> &str {
        if let Some(str) = de_sanitize(self) {
            str
        } else {
            self
        }
    }
}

impl DeSanitize for &str {
    fn de_sanitize(&self) -> &str {
        if let Some(str) = de_sanitize(self) {
            str
        } else {
            self
        }
    }
}

fn de_sanitize(string: &str) -> Option<&str> {
    match string {
        "Ty" => Some("Type"),
        "WoogOption" => Some("Option"),
        "WoogStruct" => Some("Struct"),
        "False Literal" => Some("False"),
        "FalseLiteral" => Some("False"),
        "True Literal" => Some("True"),
        "TrueLiteral" => Some("True"),
        "XSuper" => Some("Super"),
        "XSuperProxy" => Some("SuperProxy"),
        "XBox" => Some("Box"),
        "XBoxProxy" => Some("BoxProxy"),
        "ZObjectStore" => Some("ObjectStore"),
        "ZSome" => Some("Some"),
        "ZNone" => Some("None"),
        "XIf" => Some("If"),
        "XReturn" => Some("Return"),
        "XValue" => Some("Value"),
        _ => None,
    }
}

fn typecheck(
    lhs: (&RefType<ValueType>, &Span),
    rhs: (&RefType<ValueType>, &Span),
    location: Location,
    lu_dog: &LuDogStore,
    sarzak: &SarzakStore,
    models: &[SarzakStore],
) -> Result<()> {
    let lhs_span = lhs.1;
    let lhs = lhs.0;
    let rhs_span = rhs.1;
    let rhs = rhs.0;

    cfg_if::cfg_if! {
        if #[cfg(any(feature = "single", feature = "single-vec"))] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    match (&s_read!(lhs).subtype, &s_read!(rhs).subtype) {
        (ValueTypeEnum::Ty(a), ValueTypeEnum::Ty(b)) => {
            let a = sarzak.exhume_ty(a).unwrap();
            let b = sarzak.exhume_ty(b).unwrap();
            match (a, b) {
                (Ty::Integer(_), Ty::Integer(_)) => Ok(()),
                (Ty::Float(_), Ty::Float(_)) => Ok(()),
                (Ty::Boolean(_), Ty::Boolean(_)) => Ok(()),
                (Ty::SString(_), Ty::SString(_)) => Ok(()),
                (Ty::SUuid(_), Ty::SUuid(_)) => Ok(()),
                (a, b) => {
                    // dbg!(PrintableValueType(lhs, lu_dog, sarzak, models).to_string());
                    // dbg!(PrintableValueType(rhs, lu_dog, sarzak, models).to_string());
                    if a == b {
                        Ok(())
                    } else {
                        let a = PrintableValueType(lhs, lu_dog, sarzak, models);
                        let b = PrintableValueType(rhs, lu_dog, sarzak, models);

                        Err(vec![DwarfError::TypeMismatch {
                            expected: a.to_string(),
                            found: b.to_string(),
                            expected_span: lhs_span.to_owned(),
                            found_span: rhs_span.to_owned(),
                            location,
                        }])
                    }
                }
            }
        }
        // (ValueTypeEnum::Unknown(_), _) => Ok(()),
        // (_, ValueTypeEnum::Unknown(_)) => Ok(()),
        (lhs_t, rhs_t) => {
            // dbg!(PrintableValueType(lhs, lu_dog, sarzak, models).to_string());
            // dbg!(PrintableValueType(rhs, lu_dog, sarzak, models).to_string());
            if lhs_t == rhs_t {
                Ok(())
            } else {
                let lhs = PrintableValueType(lhs, lu_dog, sarzak, models);
                let rhs = PrintableValueType(rhs, lu_dog, sarzak, models);

                Err(vec![DwarfError::TypeMismatch {
                    expected: lhs.to_string(),
                    found: rhs.to_string(),
                    expected_span: lhs_span.to_owned(),
                    found_span: rhs_span.to_owned(),
                    location,
                }])
            }
        }
    }
}

pub(crate) struct PrintableValueType<'d, 'a, 'b, 'c>(
    pub &'d RefType<ValueType>,
    pub &'a LuDogStore,
    pub &'b SarzakStore,
    pub &'c [SarzakStore],
);

impl<'d, 'a, 'b, 'c> fmt::Display for PrintableValueType<'d, 'a, 'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        const TY_CLR: Colour = Colour::Purple;
        const TY_WARN_CLR: Colour = Colour::Yellow;
        const TY_ERR_CLR: Colour = Colour::Red;

        let value = s_read!(self.0);
        let lu_dog = self.1;
        let sarzak = self.2;
        let models = self.3;

        match value.subtype {
            ValueTypeEnum::Char(c) => write!(f, "'{}'", c),
            ValueTypeEnum::Empty(_) => write!(f, "()"),
            ValueTypeEnum::Error(_) => write!(f, "<error>"),
            ValueTypeEnum::Function(_) => write!(f, "<function>"),
            ValueTypeEnum::Import(ref import) => {
                let import = lu_dog.exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueTypeEnum::Lambda(_) => write!(f, "{}", TY_CLR.italic().paint("<lambda>")),
            ValueTypeEnum::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(lu_dog)[0].clone();
                write!(f, "[{}]", PrintableValueType(&ty, lu_dog, sarzak, models))
            }
            ValueTypeEnum::Range(_) => write!(f, "<range>"),
            ValueTypeEnum::Reference(ref reference) => {
                let reference = lu_dog.exhume_reference(reference).unwrap();
                let reference = s_read!(reference);
                let ty = reference.r35_value_type(lu_dog)[0].clone();
                write!(f, "&{}", PrintableValueType(&ty, lu_dog, sarzak, models))
            }
            ValueTypeEnum::Ty(ref ty) => {
                // So, sometimes these show up in the model domain. It'll get really
                // interesting when there are multiples of those in memory at once...
                if let Some(ty) = sarzak.exhume_ty(ty) {
                    match ty {
                        Ty::Boolean(_) => write!(f, "bool"),
                        Ty::Float(_) => write!(f, "float"),
                        Ty::Integer(_) => write!(f, "int"),
                        Ty::Object(ref object) => {
                            error!("um, check this out?");
                            // This should probably just be an unwrap().
                            if let Some(object) = sarzak.exhume_object(object) {
                                write!(f, "{}", object.name)
                            } else {
                                write!(f, "<unknown object>")
                            }
                        }
                        Ty::SString(_) => write!(f, "string"),
                        Ty::SUuid(_) => write!(f, "Uuid"),
                        gamma => {
                            error!("deal with sarzak type {:?}", gamma);
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    for model in models {
                        if let Some(Ty::Object(ref object)) = model.exhume_ty(ty) {
                            if let Some(object) = model.exhume_object(object) {
                                return write!(f, "{}Proxy", object.name);
                            }
                        }
                    }
                    write!(f, "<unknown object>")
                }
            }
            ValueTypeEnum::Unknown(_) => write!(f, "<unknown>"),
            ValueTypeEnum::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                let option = s_read!(option);
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap();
                        let some = s_read!(some);
                        let value = s_read!(some.r23_x_value(lu_dog)[0]).clone();
                        let ty = value.r24_value_type(lu_dog)[0].clone();
                        write!(
                            f,
                            "Some({})",
                            PrintableValueType(&ty, lu_dog, sarzak, models)
                        )
                    }
                }
            }
            ValueTypeEnum::WoogStruct(ref woog_struct) => {
                debug!("woog_struct {:?}", woog_struct);
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", woog_struct.name)
            }
            ValueTypeEnum::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}
