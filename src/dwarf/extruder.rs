use core::fmt;
use std::{fs::File, io::prelude::*, ops::Range, path::PathBuf};

use ansi_term::Colour;
use heck::{ToShoutySnakeCase, ToUpperCamelCase};
use log;
use sarzak::{
    lu_dog::ItemStatement,
    sarzak::{store::ObjectStore as SarzakStore, types::Ty, Object},
};
use snafu::{location, prelude::*, Location};
use tracy_client::Client;
use uuid::Uuid;

use crate::{
    dwarf::{
        DwarfError, Expression as ParserExpression, Item, ObjectIdNotFoundSnafu, Result, Spanned,
        Statement as ParserStatement, Type, TypeMismatchSnafu,
    },
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{
            Block, BooleanOperator, Call, Error, ErrorExpression, Expression, ExpressionStatement,
            Field, FieldExpression, ForLoop, Function, Implementation, Index, IntegerLiteral,
            Item as WoogItem, LetStatement, Literal, LocalVariable, Parameter, Print,
            RangeExpression, Span as LuDogSpan, Statement, StaticMethodCall, StringLiteral,
            StructExpression, ValueType, Variable, VariableExpression, WoogOption, WoogStruct, XIf,
            XValue, XValueEnum, ZSome,
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
        );
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
        );
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
        );
    };
}

type Span = Range<usize>;

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
/// source code. The AST is itself sort of an intermetiate representation. It's
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
    ValueType::new_ty(&new_ref!(Ty, Ty::new_boolean()), &mut lu_dog);
    ValueType::new_ty(&new_ref!(Ty, Ty::new_float()), &mut lu_dog);
    ValueType::new_ty(&new_ref!(Ty, Ty::new_integer()), &mut lu_dog);
    ValueType::new_ty(&new_ref!(Ty, Ty::new_s_string()), &mut lu_dog);
    ValueType::new_ty(&new_ref!(Ty, Ty::new_s_uuid()), &mut lu_dog);

    if let Some((source, ast)) = source {
        let _client = Client::start();

        let source = DwarfSourceFile::new(source, &mut lu_dog);

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

    // Put the type information in first.
    for ConveyStruct { name, span, fields } in structs {
        debug!("Intering struct {}", name);
        inter_struct(&name, &fields, span, source, lu_dog, models, sarzak)?;
    }

    // Using the type information, and the input, inter the implementation blocks.
    for ConveyImpl { name, span, funcs } in implementations {
        debug!("Intering implementation {}", name);
        inter_implementation(&name, &funcs, span, source, lu_dog, models, sarzak)?;
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
        debug!("Intering function {}", name);
        inter_func(
            &name,
            &params,
            &return_type,
            &statements,
            None,
            None,
            span,
            source,
            lu_dog,
            models,
            sarzak,
        )?;
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

    Ok(())
}

fn inter_func(
    name: &str,
    params: &[(Spanned<String>, Spanned<Type>)],
    return_type: &Spanned<Type>,
    stmts: &Spanned<ParserExpression>,
    impl_block: Option<&RefType<Implementation>>,
    impl_ty: Option<&RefType<ValueType>>,
    _span: &Span,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_func {}", name);

    let block = Block::new(Uuid::new_v4(), None, lu_dog);

    let name = name.de_sanitize();
    let stmts = if let ParserExpression::Block(stmts) = &stmts.0 {
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
    ValueType::new_function(&func, lu_dog);

    let mut last_param_uuid: Option<Uuid> = None;
    for ((param_name, name_span), (param_ty, param_span)) in params {
        debug!("param name {}", param_name);
        debug!("param ty {}", param_ty);

        let span = LuDogSpan::new(
            name_span.end as i64,
            name_span.start as i64,
            source,
            None,
            None,
            lu_dog,
        );
        let param = Parameter::new(&func, None, lu_dog);

        debug!("param {:?}", param);

        let var = Variable::new_parameter(param_name.to_owned(), &param, lu_dog);
        debug!("var {:?}", var);
        // 🚧 We'll need to do something about this soon. Actually, it never belonged
        // here. It only makes sense that you can only have values in a block. Now the
        // model enforces that.
        //
        // That said, we need to introduce the values into the block, so that we don't
        // error out when parsing the statements.
        //
        let param_ty = get_value_type(
            &param_ty,
            param_span,
            impl_ty.clone(),
            lu_dog,
            models,
            sarzak,
        )?;
        debug!("param_ty {:?}", param_ty);
        let value = XValue::new_variable(&block, &param_ty, &var, lu_dog);
        // 🚧 Was this causing a crash?
        s_write!(span).x_value = Some(s_read!(value).id);
        last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
    }

    let stmts: Vec<RefType<ParserStatement>> = stmts
        .iter()
        .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
        .collect();

    inter_statements(&stmts, &block, source, lu_dog, models, sarzak)?;

    Ok(())
}

pub fn inter_statement(
    stmt: &RefType<ParserStatement>,
    source: &RefType<DwarfSourceFile>,
    block: &RefType<Block>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(RefType<Statement>, RefType<ValueType>)> {
    debug!("inter_statement {:?}", stmt);

    match &*s_read!(stmt) {
        //
        // Expression
        //
        ParserStatement::Expression((expr, span)) => {
            let (expr, _) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ExpressionStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_expression_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ValueType::new_empty(lu_dog)))
        }
        //
        // Item
        //
        ParserStatement::Item((item, span)) => {
            match item {
                // Item::Function((name, _name_span), params, return_type, stmts) => {
                Item::Function(ref name, ref params, ref return_type, ref stmts) => inter_func(
                    &name.0,
                    &params,
                    &return_type,
                    &stmts,
                    None,
                    None,
                    span,
                    source,
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
                    inter_struct(&name, &fields, span, source, lu_dog, models, sarzak)?
                }
                _ => unimplemented!(),
            };
            let stmt = ItemStatement::new();
            let stmt = Statement::new_item_statement(&block, None, lu_dog);
            Ok((stmt, ValueType::new_empty(lu_dog)))
        }
        //
        // Let
        //
        ParserStatement::Let((var_name, _var_span), type_, (expr, expr_span)) => {
            // We only want one storage location per name per block, so we look
            // for, and remove an existing one -- all the way up to the value.
            // 🚧 There's a let problem in mandelbrot, and I wonder if this might
            // have something to do with it? I'worry that we are eliding the locals
            // in parent stack frames.
            let values = lu_dog
                .iter_x_value()
                .filter(|value| s_read!(value).block == s_read!(block).id)
                .collect::<Vec<_>>();
            for value in values {
                let value = s_read!(value);
                match value.subtype {
                    XValueEnum::Variable(ref var) => {
                        let var = lu_dog.exhume_variable(var).unwrap();
                        let var = s_read!(var);
                        if var.name == *var_name {
                            match var.subtype {
                                VariableEnum::LocalVariable(ref local) => {
                                    lu_dog.exorcise_local_variable(local);
                                    lu_dog.exorcise_variable(&var.id);
                                    lu_dog.exorcise_x_value(&value.id);
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Setup the local variable that is the LHS of the statement.
            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            let var = Variable::new_local_variable(var_name.to_owned(), &local, lu_dog);

            // dbg!(&var, &block);

            // Now parse the RHS, which is an expression.
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                expr_span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let ty = if let Some((type_, _)) = type_ {
                type_.into_value_type(lu_dog, models, sarzak)
            } else {
                ty
            };

            // 🚧
            // Let's keep an eye on this. I've had the notion of having a separate
            // entry point for the REPL, and conditionally needing to generate an
            // error would support the idea.
            if let ValueType::Unknown(_) = &*s_read!(ty) {
                warn!("Unknown type for variable {}", var_name);
            }

            // Create a variable, now that we (hopefully) have a type from the expression.
            let _value = XValue::new_variable(&block, &ty, &var, lu_dog);
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
            let stmt = Statement::new_let_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ValueType::new_empty(lu_dog)))
        }
        //
        // Result
        //
        ParserStatement::Result((ref expr, span)) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                span,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ResultStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_result_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ty))
        }
        道 => todo!("{:?}", 道),
    }
}

fn inter_statements(
    statements: &[RefType<ParserStatement>],
    block: &RefType<Block>,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<RefType<ValueType>> {
    let mut value_type = ValueType::new_empty(lu_dog);

    let mut last_stmt_uuid: Option<Uuid> = None;
    for stmt in statements {
        let (stmt, ty) = inter_statement(stmt, source, block, lu_dog, models, sarzak)?;
        if last_stmt_uuid == None {
            s_write!(block).statement = Some(s_read!(stmt).id);
        }
        if s_read!(block).statement.is_none() {
            s_write!(block).statement = Some(s_read!(stmt).id);
        }
        last_stmt_uuid = link_statement!(last_stmt_uuid, stmt, lu_dog);
        value_type = ty;
    }

    Ok(value_type)
}

/// I have a feeling that this one is going to be intense...
/// Actually, maybe not. There's not much happening just when we are populating.
/// It should get intense when we are evaluating for the SVM.
/// I may have spoken too soon. We need to be populating the store, in addition
/// to returning the type. Duh. And we should return the expression so that we
/// can create a value from it.
fn inter_expression(
    expr: &RefType<ParserExpression>,
    span: &Span,
    source: &RefType<DwarfSourceFile>,
    block: &RefType<Block>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(
    (RefType<Expression>, RefType<LuDogSpan>),
    RefType<ValueType>,
)> {
    debug!("expr {:?}", expr);
    debug!("span {:?}", span);

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
        ParserExpression::Addition(ref lhs, ref rhs) => {
            debug!("Addition");
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            // 🚧 We also need to check that the type supports addition.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Binary::new_addition(lu_dog);
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // And
        //
        ParserExpression::And(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                &rhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            ensure!(
                if let ValueType::Ty(ref id) = &*s_read!(lhs_ty) {
                    let ty = sarzak.exhume_ty(id).unwrap();
                    if let Ty::Boolean(_) = ty {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                },
                {
                    let span = s_read!(lhs.1).start as usize..s_read!(lhs.1).end as usize;
                    let lhs = PrintableValueType(lhs_ty, lu_dog, sarzak, models);
                    TypeMismatchSnafu {
                        found: lhs.to_string(),
                        expected: "bool".to_string(),
                        span,
                    }
                }
            );

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = BooleanOperator::new_and(lu_dog);
            let expr = Binary::new_boolean_operator(&expr, lu_dog);
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
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
        //         StaticMethodCall::new("execute_asm".to_owend(), "chacha".to_owned(), lu_dog);
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
        ParserExpression::Assignment(ref lhs, ref rhs) => {
            // dbg!("raw", &lhs, &rhs);
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                &rhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            // dbg!("expr", &lhs, &rhs);
            // dbg!(&lhs_ty, &rhs_ty);

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Binary::new_assignment(lu_dog);
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(&block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        //
        // Bang
        //
        ParserExpression::Bang(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, (*expr).0.to_owned()),
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let not = Unary::new_not(lu_dog);
            let operator = Operator::new_unary(None, &expr.0, &not, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Block
        //
        ParserExpression::Block(ref stmts) => {
            let block = Block::new(Uuid::new_v4(), None, lu_dog);
            debug!("block {:?}", block);
            let stmts: Vec<RefType<ParserStatement>> = stmts
                .iter()
                .map(|stmt| new_ref!(ParserStatement, stmt.0.to_owned()))
                .collect();
            let expr = Expression::new_block(&block, lu_dog);
            let ty = inter_statements(&stmts, &block, source, lu_dog, models, sarzak)?;
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
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
            let ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_boolean()), lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Debug
        //
        ParserExpression::Debug => {
            let expr = Expression::new_debugger(lu_dog);
            let ty = ValueType::new_empty(lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Division
        //
        ParserExpression::Division(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            // 🚧 We also need to check that the type supports division.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Binary::new_division(lu_dog);
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        // //
        // // Empty
        // //
        // ParserExpression::Empty => {
        //     let expr = Expression::new_empty(lu_dog);
        //     let ty = ValueType::new_empty(lu_dog);
        //     let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Equals
        //
        ParserExpression::Equals(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                &rhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Comparison::new_equal(lu_dog);
            let expr = Operator::new_comparison(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&new_ref!(Ty, ty), lu_dog);

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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
                &new_ref!(ParserExpression, (*lhs).0.clone()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let id = s_read!(lhs_ty).id();
            let ty = lu_dog.exhume_value_type(&id).unwrap();
            let ty_read = s_read!(ty);

            match &*ty_read {
                // So the lhs is is a sarzak type, which (probably?) means that
                // it's an object. So we look it up in the models. What if it's
                // a UDT without a store? That's what I'm working on now, and I
                // haven't hit any problems with this. So, I'm wondering if it's
                // becasue it's getting caught as a woog struct below. In fact,
                // I wonder if this code is necessary at all.
                // ValueType::Ty(ref id) => {
                //     for model in models {
                //         if let Some(Ty::Object(ref _object)) = model.exhume_ty(id) {
                //             // let object = model.exhume_object(object).unwrap();
                //             let expr = FieldAccess::new(rhs, &lhs.0, lu_dog);
                //             let expr = Expression::new_field_access(&expr, lu_dog);

                //             // 🚧 Can we not do better?
                //             let ty = ValueType::new_unknown(lu_dog);

                //             let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                //             s_write!(span).x_value = Some(s_read!(value).id);

                //             return Ok(((expr, span), ty));
                //         }
                //     }

                //     // Return an error (really need to get result in here) if it wasn't
                //     // in one of the models.
                //     let error = ErrorExpression::new(
                //         format!("💥 {:?} is not a proxy object\n", ty),
                //         lu_dog,
                //     );
                //     let expr = Expression::new_error_expression(&error, lu_dog);
                //     // 🚧
                //     // Returning an empty, because the error stuff in ValueType is fucked.
                //     let ty = ValueType::new_empty(lu_dog);

                //     let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                //     s_write!(span).x_value = Some(s_read!(value).id);

                //     return Ok(((expr, span), ty));
                // }
                /// We matched on the lhs type.
                ValueType::Function(ref _id) => {
                    // let func = lu_dog.exhume_function(id).unwrap();
                    // let impl_ = &s_read!(func).r9_implementation(lu_dog)[0];
                    // let woog_struct = &s_read!(impl_).r8_woog_struct(lu_dog)[0];
                    // let fat = FieldAccessTarget::new_function(&func, lu_dog);
                    // let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                    // let expr = Expression::new_field_access(&expr, lu_dog);
                    // let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                    // let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                    // s_write!(span).x_value = Some(s_read!(value).id);

                    Ok((lhs, ty.clone()))
                }
                ValueType::WoogStruct(ref id) => {
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();

                    let field = lu_dog
                        .exhume_field_id_by_name(&rhs.0.to_upper_camel_case())
                        .unwrap();
                    let field = lu_dog.exhume_field(&field);
                    // let field = woog_struct.r7_field(lu_dog);
                    // let field = field.iter().find(|f| s_read!(f).name == rhs);
                    let func = if let Some(impl_) =
                        s_read!(woog_struct).r8c_implementation(lu_dog).pop()
                    {
                        let impl_ = s_read!(impl_);

                        let funcs = impl_.r9_function(lu_dog);
                        funcs
                            .iter()
                            .find(|f| s_read!(f).name == rhs.0)
                            .and_then(|f| Some(f.clone()))
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
                        let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                        s_write!(span).x_value = Some(s_read!(value).id);

                        Ok(((expr, span), ty))
                    } else if let Some(func) = func {
                        let fat = FieldAccessTarget::new_function(&func, lu_dog);
                        let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                        let expr = Expression::new_field_access(&expr, lu_dog);
                        let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                        let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                        s_write!(span).x_value = Some(s_read!(value).id);

                        Ok(((expr, span), ty))
                    } else {
                        let span = s_read!(span);
                        // let span: Range<usize> = *span.into();
                        let span = span.start as usize..span.end as usize;
                        Err(DwarfError::StructFieldNotFound {
                            field: rhs.0.clone(),
                            span: span,
                        })
                    }
                }
                ty => {
                    let error =
                        ErrorExpression::new(format!("💥 {:?} is not a struct\n", ty), lu_dog);
                    let expr = Expression::new_error_expression(&error, lu_dog);
                    // 🚧 Returning an empty, because the error stuff in ValueType is fucked.
                    // I wonder what we mean?
                    let ty = ValueType::new_empty(lu_dog);
                    let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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
            let ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_float()), lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // For Loop
        //
        ParserExpression::For(iter, collection, body) => {
            debug!("ParserExpresssion::For");
            let iter = iter.0.clone();

            let cspan = &collection.1;
            let collection = new_ref!(ParserExpression, collection.0.clone());

            // 🚧 Should we be checking this to ensure that it's an iterable?
            let (collection, _collection_ty) =
                inter_expression(&collection, cspan, source, block, lu_dog, models, sarzak)?;

            let bspan = &body.1;
            let body = new_ref!(ParserExpression, (&body.0).to_owned());
            let (body, _body_ty) =
                inter_expression(&body, bspan, source, block, lu_dog, models, sarzak)?;

            let body = s_read!(body.0);
            let body = if let Expression::Block(body) = &*body {
                body
            } else {
                panic!("Expected a block expression");
            };
            let body = lu_dog.exhume_block(&body).unwrap();

            let for_loop = ForLoop::new(iter, &body, &collection.0, lu_dog);
            let expr = Expression::new_for_loop(&for_loop, lu_dog);
            let ty = ValueType::new_empty(lu_dog);

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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
            // For now, this will help the REPL work correctly...
            //
            // 🚧 Deal with the above situation.

            // Look up the function. Shit. Maybe this function lookup thing
            // belongs in the LocalVariable interring code...
            let (func_expr, ret_ty) = inter_expression(
                &new_ref!(ParserExpression, func.to_owned()),
                fspan,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let func_call = Call::new_function_call(false, Some(&func_expr.0), lu_dog);
            let func = Expression::new_call(&func_call, lu_dog);
            let value = XValue::new_expression(&block, &ret_ty, &func, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            let mut last_arg_uuid: Option<Uuid> = None;
            for param in params {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, param.0.to_owned()),
                    &param.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let arg = Argument::new(None, &func_call, &arg_expr.0, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "ParserExpression::FunctionCall exit {:?}",
                (&func_call, &s_read!(func_call).r28_argument(lu_dog))
            );

            let ty = PrintableValueType(ret_ty.clone(), lu_dog, sarzak, models);
            debug!("return type{}", ty.to_string());

            Ok(((func, span), ret_ty))
        }
        //
        // GreaterThan
        //
        ParserExpression::GreaterThan(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Comparison::new_greater_than(lu_dog);
            let expr = Operator::new_comparison(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&new_ref!(Ty, ty), lu_dog);

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // If
        //
        ParserExpression::If(conditional, true_block, false_block) => {
            debug!("conditional {:?}", conditional);
            let cspan = &conditional.1;
            let conditional = new_ref!(ParserExpression, conditional.0.to_owned());
            let (conditional, conditional_ty) =
                inter_expression(&conditional, cspan, source, block, lu_dog, models, sarzak)?;
            debug!("ParserExpression::If {:?}", conditional_ty);

            // We really need to get some error handling in here.
            if let ValueType::Ty(ref ty) = &*s_read!(conditional_ty) {
                let s_ty = sarzak.exhume_ty(ty).unwrap();
                if let Ty::Boolean(_) = s_ty {
                    // We're good.
                } else {
                    let ty = PrintableValueType(conditional_ty.clone(), lu_dog, sarzak, models);
                    return Err(DwarfError::TypeMismatch {
                        expected: "boolean".to_owned(),
                        found: ty.to_string(),
                        span: cspan.to_owned(),
                    });
                }
            } else {
                let ty = PrintableValueType(conditional_ty.clone(), lu_dog, sarzak, models);
                return Err(DwarfError::TypeMismatch {
                    expected: "boolean".to_owned(),
                    found: ty.to_string(),
                    span: cspan.to_owned(),
                });
            }

            let tspan = &true_block.1;
            let true_block = new_ref!(ParserExpression, true_block.0.to_owned());
            let (true_block, true_ty) =
                inter_expression(&true_block, tspan, source, block, lu_dog, models, sarzak)?;
            let true_block = if let Expression::Block(true_block) = s_read!(true_block.0).clone() {
                true_block
            } else {
                panic!("Expected a block expression");
            };
            let true_block = lu_dog.exhume_block(&true_block).unwrap();

            let false_block = if let Some(false_block) = false_block {
                let fspan = &false_block.1;
                let false_block = new_ref!(ParserExpression, false_block.0.to_owned());
                let (false_block, _false_ty) =
                    inter_expression(&false_block, fspan, source, block, lu_dog, models, sarzak)?;
                let false_block =
                    if let Expression::Block(false_block) = s_read!(false_block.0).clone() {
                        false_block
                    } else {
                        panic!("Expected a block expression");
                    };
                let false_block = lu_dog.exhume_block(&false_block).unwrap();
                Some(false_block)
            } else {
                None
            };

            let if_expr = XIf::new(false_block.as_ref(), &true_block, &conditional.0, lu_dog);
            let expr = Expression::new_x_if(&if_expr, lu_dog);

            let ty = true_ty;

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Index
        //
        ParserExpression::Index(target, index) => {
            let (target, _target_ty) = inter_expression(
                &new_ref!(ParserExpression, target.0.to_owned()),
                &target.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (index, index_ty) = inter_expression(
                &new_ref!(ParserExpression, index.0.to_owned()),
                &index.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let index = Index::new(&index.0, &target.0, lu_dog);
            let int_ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_integer()), lu_dog);

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&int_ty, &index_ty, tc_span, lu_dog, sarzak, models)?;

            let ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_integer()), lu_dog);
            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&index_ty, &ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Expression::new_index(&index, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);
            // 🚧 We should really check that the target type is some sort of list.

            Ok(((expr, span), ty))
        }
        //
        // IntegerLiteral
        //
        ParserExpression::IntegerLiteral(literal) => {
            let expr = Expression::new_literal(
                &Literal::new_integer_literal(&IntegerLiteral::new(*literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_integer()), lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // LessThanOrEqual
        //
        ParserExpression::LessThanOrEqual(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Comparison::new_less_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&new_ref!(Ty, ty), lu_dog);

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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
                let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);

                Ok(((expr, span), ty))
            } else {
                let mut elements = elements.iter();
                // I'm going to get the type of the first element, and then check
                // that each subsequent element is the same type.
                let element = elements.next().unwrap();
                let ((first, span), first_ty) = inter_expression(
                    &new_ref!(ParserExpression, element.0.to_owned()),
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
                let value = XValue::new_expression(&block, &first_ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);
                let list_expr = ListExpression::new(Some(&element), lu_dog);

                let mut last_element_uuid: Option<Uuid> = Some(s_read!(element).id);
                while let Some(element) = elements.next() {
                    let ((elt, span), elt_ty) = inter_expression(
                        &new_ref!(ParserExpression, element.0.to_owned()),
                        &element.1,
                        source,
                        block,
                        lu_dog,
                        models,
                        sarzak,
                    )?;

                    let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
                    typecheck(&first_ty, &elt_ty, tc_span, lu_dog, sarzak, models)?;

                    let element = ListElement::new(&elt, None, lu_dog);
                    last_element_uuid = link_list_element!(last_element_uuid, element, lu_dog);
                    let expr = Expression::new_list_element(&element, lu_dog);
                    let value = XValue::new_expression(&block, &elt_ty, &expr, lu_dog);
                    s_write!(span).x_value = Some(s_read!(value).id);
                }

                let expr = Expression::new_list_expression(&list_expr, lu_dog);
                let ty = ValueType::new_list(&list, lu_dog);
                let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);

                Ok(((expr, span), ty))
            }
        }
        //
        // LocalVariable
        //
        ParserExpression::LocalVariable(name) => {
            // We need to return an expression and a type.
            debug!("localvariable name {}", name);
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

            // Now search for a value that's a Variable, and see if the access matches
            // the variable.
            let mut expr_type_tuples = values
                .iter()
                .filter_map(|value| {
                    // debug!("value", value);
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
                                    VariableEnum::LocalVariable(_) | VariableEnum::Parameter(_) => {
                                        // let value =
                                            // s_read!(var.r11_x_value(lu_dog)[0]).clone();
                                        let ty = value.r24_value_type(lu_dog)[0].clone();

                                        let lhs_ty =
                                            PrintableValueType(ty.clone(), lu_dog, sarzak, models);

                                        debug!("{}, {:?}, {}", name, &value, lhs_ty.to_string());

                                        // 🚧
                                        // Ok, so I parsed a local variable expression. We need to create
                                        // a VariableExpression, and it in turn needs an Expression, which
                                        // needs a Value, and finally   a ValueType.
                                        // Except that I don't think we want to create values in the walker.
                                        // Doing so wreaks havoc downstream in the interpreter, because
                                        // It sees that value and expects that it's been evaluated.
                                        // And we got here by searching for a value anyway.
                                        //
                                        // We don't want to create more than one of these.
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
                                            XValue::new_expression(&block, &ty, &expr, lu_dog);
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
            // "yes", which I guess is correct, but they type is `()`. So that's werid.
            // Also, there are two different places that b shows up. I'm taking the
            // last one, which maybe corresponts to `b` being in rhs of the previous
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
            } else {
                // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
                // As neat as it is that I'm compiling this into the binary, we should actually
                // bail here, and make the user do something about it. The issue is that this
                // may get run from the repl, and in that case we want to return the expression.
                // So, we need a flag...
                let expr = ErrorExpression::new(
                    format!(
                        "\n  ──➤  variable: `{}` not found\n",
                        Colour::Red.paint(name.to_owned())
                    ),
                    lu_dog,
                );
                let expr = Expression::new_error_expression(&expr, lu_dog);
                (
                    expr,
                    ValueType::new_error(&Error::new_unknown_variable(lu_dog), lu_dog),
                );
                // 🚧 WTF are we doing here? Above it's an error, and here we have
                // a variable expression that we're returning?
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(&expr, lu_dog);
                let ty = ValueType::new_unknown(lu_dog);

                let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                s_write!(span).x_value = Some(s_read!(value).id);

                Ok(((expr, span), ty))
            }
        }
        //
        // MethodCall
        //
        ParserExpression::MethodCall(instance, (method, _meth_span), args) => {
            debug!("ParserExpression::MethodCall {:?}", instance);

            let (instance, instance_ty) = inter_expression(
                &new_ref!(ParserExpression, (*instance).0.to_owned()),
                &instance.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let meth = MethodCall::new(method.to_owned(), lu_dog);
            let call = Call::new_method_call(false, Some(&instance.0), &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            let method_return_type = ValueType::new_unknown(lu_dog);
            let value = XValue::new_expression(&block, &method_return_type, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            let mut last_arg_uuid: Option<Uuid> = None;
            // This is the self parameter
            let this = Argument::new(None, &call, &instance.0, lu_dog);
            last_arg_uuid = link_argument!(last_arg_uuid, this, lu_dog);

            for arg in args {
                let (arg_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, arg.0.to_owned()),
                    &arg.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let value = XValue::new_expression(&block, &ty, &arg_expr.0, lu_dog);
                let _span = LuDogSpan::new(
                    arg.1.end as i64,
                    arg.1.start as i64,
                    source,
                    Some(&value),
                    None,
                    lu_dog,
                );
                let arg = Argument::new(None, &call, &arg_expr.0, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            Ok(((expr, span), method_return_type))
        }
        //
        // Negation
        //
        ParserExpression::Negation(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, (*expr).0.to_owned()),
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let negation = Unary::new_negation(lu_dog);
            let operator = Operator::new_unary(None, &expr.0, &negation, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Print
        //
        ParserExpression::Print(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, (*expr).0.to_owned()),
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let print = Print::new(&expr.0, lu_dog);
            let expr = Expression::new_print(&print, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Multiplication
        //
        ParserExpression::Multiplication(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            // 🚧 We also need to check that the type supports multiplication.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let tc_span = s_read!(span).start as usize..s_read!(span).end as usize;
            typecheck(&lhs_ty, &rhs_ty, tc_span, lu_dog, sarzak, models)?;

            let expr = Binary::new_multiplication(lu_dog);
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
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
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);

            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Range
        //
        ParserExpression::Range(start, end) => {
            let (start, start_ty) = inter_expression(
                &new_ref!(ParserExpression, (*start).0.to_owned()),
                &start.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (end, _end_ty) = inter_expression(
                &new_ref!(ParserExpression, (*end).0.to_owned()),
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
            let value = XValue::new_expression(&block, &start_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), start_ty))
        }
        //
        // Return
        //
        ParserExpression::Return(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, (*expr).0.to_owned()),
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let ret = XReturn::new(&expr.0, lu_dog);
            let expr = Expression::new_x_return(&ret, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Some
        //
        ParserExpression::Some(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, (*expr).0.to_owned()),
                &expr.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            // 🚧 Missing span...
            let value = XValue::new_expression(&block, &ty, &expr.0, lu_dog);
            let some = ZSome::new(&value, lu_dog);
            let expr = Expression::new_z_some(&some, lu_dog);
            let option = WoogOption::new_z_some(&ty, &some, lu_dog);
            let ty = ValueType::new_woog_option(&option, lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);

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
                ValueType::new_ty(&new_ref!(Ty, Ty::new_s_uuid()), lu_dog)
            } else {
                debug!(
                    "ParserExpression::StaticMethodCall: looking up type {}",
                    type_name
                );

                // 🚧  This thing below offends my senses. It needs to be cleaned up.
                let ty = if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(&type_name) {
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                    let ty = if let Some(impl_) =
                        s_read!(woog_struct).r8c_implementation(lu_dog).pop()
                    {
                        let impl_ = s_read!(impl_);

                        let funcs = impl_.r9_function(lu_dog);
                        funcs
                            .iter()
                            .find(|f| s_read!(f).name == *method)
                            .and_then(|f| {
                                let ret_ty = s_read!(f).return_type;
                                let ret_ty = lu_dog.exhume_value_type(&ret_ty).unwrap();
                                Some(ret_ty)
                            })
                    } else {
                        None
                    };
                    ty
                } else {
                    Some(ValueType::new_unknown(lu_dog))
                };

                if let Some(ty) = ty {
                    ty
                } else {
                    ValueType::new_unknown(lu_dog)
                }

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

            let mut last_arg_uuid: Option<Uuid> = None;
            for param in params {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, param.0.to_owned()),
                    &param.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let arg = Argument::new(None, &call, &arg_expr.0, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
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
            let ty = ValueType::new_ty(&new_ref!(Ty, Ty::new_s_string()), lu_dog);
            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), ty))
        }
        //
        // Struct
        //
        ParserExpression::Struct(name, fields) => {
            let name = if let ParserExpression::LocalVariable(obj) = &name.0 {
                obj
            } else {
                return Err(DwarfError::Internal {
                    description: "Expected a local varible in struct expression".to_owned(),
                    location: location!(),
                });
            };

            debug!("ParserExpression::Struct {}", name);

            // dbg!(&lu_dog.iter_woog_struct().collect::<Vec<_>>());

            // Here we don't de_sanitize the name, and we are looking it up in the
            // dwarf model.
            let id = lu_dog.exhume_woog_struct_id_by_name(&name).unwrap();
            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap().clone();

            let expr = StructExpression::new(Uuid::new_v4(), &woog_struct, lu_dog);
            // fields
            //     .iter()
            //     .map(|((name, _), (field_expr, _))| (name, field_expr));

            for (name, field_expr) in fields {
                // 🚧 Do type checking here? I don't think that I have what I need.
                let (field_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, field_expr.0.to_owned()),
                    &field_expr.1,
                    source,
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let _field = FieldExpression::new(name.0.to_owned(), &field_expr.0, &expr, lu_dog);

                // Adding the following actually breaks shit.
                // let expr = Expression::new_field_expression(&field, lu_dog);
                // let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                // s_write!(field_expr.1).x_value = Some(s_read!(value).id);
            }

            // Same name, de_sanitized, in a different model. Oh, right, this is
            // the source model. What's going on above?
            // Here we are looking up the object in one of the models. Above we
            // are pulling the struct and it's fields from the lu_dog. lol
            for model in models {
                if let Some(obj) = model.exhume_object_id_by_name(name.de_sanitize()) {
                    let ty = model.exhume_ty(&obj).unwrap().clone();

                    // let obj = model.exhume_object_id_by_name(name.de_sanitize()).unwrap();
                    // let ty = model.exhume_ty(&obj).unwrap();

                    let expr = Expression::new_struct_expression(&expr, lu_dog);
                    let ty = ValueType::new_ty(&new_ref!(Ty, ty), lu_dog);

                    let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                    s_write!(span).x_value = Some(s_read!(value).id);

                    return Ok(((expr, span), ty));
                }
            }

            // I love that the type of the thing is the same as the thing itself.
            let expr = Expression::new_struct_expression(&expr, lu_dog);
            let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);

            let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            return Ok(((expr, span), ty));
        }
        //
        // Subtraction
        //
        ParserExpression::Subtraction(ref lhs, ref rhs) => {
            debug!("Subtraction");
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                source,
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, _rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
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
            let expr = Operator::new_binary(Some(&rhs.0), &lhs.0, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(&block, &lhs_ty, &expr, lu_dog);
            s_write!(span).x_value = Some(s_read!(value).id);

            Ok(((expr, span), lhs_ty))
        }
        道 => {
            let source = &s_read!(source).source;
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            Err(DwarfError::NoImplementation {
                missing: format!("{:?}", 道),
                code: source[span.clone()].to_owned(),
                span,
            })
        }
    }
}

fn inter_import(
    _path: &Vec<Spanned<String>>,
    _alias: &Option<(String, Range<usize>)>,
    source: &String,
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

    Err(DwarfError::NoImplementation {
        missing: "Use statement not implemented yet".to_owned(),
        // span: path[0].1.start..path[path.len() - 1].1.end,
        span: span.to_owned(),
        code: source[span.to_owned()].to_owned(),
    })
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
    let id = lu_dog
        .exhume_woog_struct_id_by_name(name)
        .expect(&format!("struct {} not found", name));

    let mt = lu_dog.exhume_woog_struct(&id).unwrap();

    let implementation = Implementation::new(&mt, lu_dog);
    let _ = WoogItem::new_implementation(source, &implementation, lu_dog);

    debug!("inter_implementation {}", name);

    for (func, span) in funcs {
        match func {
            Item::Function(ref name, ref params, ref return_type, ref stmts) => inter_func(
                &name.0,
                &params,
                &return_type,
                &stmts,
                Some(&implementation),
                Some(&impl_ty),
                span,
                source,
                lu_dog,
                models,
                sarzak,
            )?,
            _ => return Err(DwarfError::ImplementationBlock { span: span.clone() }),
        }
    }
    // }

    Ok(())
}

fn inter_struct(
    name: &str,
    fields: &[(Spanned<String>, Spanned<Type>)],
    span: &Span,
    source: &RefType<DwarfSourceFile>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_struct {}", name);
    let s_name = name.de_sanitize();

    if let Some((ref model, ref id)) = models
        .iter()
        .find_map(|model| {
            if let Some(id) = model.exhume_object_id_by_name(s_name) {
                Some((model, id))
            } else {
                None
            }
        })
        .map(|id| id.to_owned())
    {
        let obj = model.exhume_object(id);
        ensure!(obj.is_some(), ObjectIdNotFoundSnafu { id: *id });
        let obj = obj.unwrap();

        let mt = WoogStruct::new(
            name.to_owned(),
            Some(&new_ref!(Object, obj.to_owned())),
            lu_dog,
        );
        let _ = WoogItem::new_woog_struct(source, &mt, lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, _)) in fields {
            let name = name.de_sanitize();

            let ty = get_value_type(type_, span, None, lu_dog, models, sarzak)?;
            let _field = Field::new(name.to_owned(), &mt, &ty, lu_dog);
        }
    } else {
        let mt = WoogStruct::new(name.to_owned(), None, lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, _)) in fields {
            let name = name.de_sanitize();

            let ty = get_value_type(type_, span, None, lu_dog, models, sarzak)?;
            let _field = Field::new(name.to_owned(), &mt, &ty, lu_dog);
        }
    }

    Ok(())
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
            Ok(ValueType::new_ty(&new_ref!(Ty, ty), lu_dog))
        }
        Type::Empty => Ok(ValueType::new_empty(lu_dog)),
        Type::Float => {
            let ty = Ty::new_float();
            Ok(ValueType::new_ty(&new_ref!(Ty, ty), lu_dog))
        }
        Type::Integer => {
            let ty = Ty::new_integer();
            Ok(ValueType::new_ty(&new_ref!(Ty, ty), lu_dog))
        }
        Type::List(ref type_) => {
            let inner_type = get_value_type(
                &(*type_).0,
                &(*type_).1,
                enclosing_type,
                lu_dog,
                models,
                sarzak,
            )?;
            let list = List::new(&inner_type, lu_dog);
            Ok(ValueType::new_list(&list, lu_dog))
        }
        Type::Option(ref type_) => {
            let inner_type = get_value_type(
                &(*type_).0,
                &(*type_).1,
                enclosing_type,
                lu_dog,
                models,
                sarzak,
            )?;
            let option = WoogOption::new_z_none(&inner_type, lu_dog);
            Ok(ValueType::new_woog_option(&option, lu_dog))
        }
        Type::Reference(ref type_) => {
            let inner_type = get_value_type(
                &(*type_).0,
                &(*type_).1,
                enclosing_type,
                lu_dog,
                models,
                sarzak,
            )?;
            // We don't know the address yet -- we'll fix it in the interpreter.
            let reference = Reference::new(Uuid::new_v4(), false, &inner_type, lu_dog);
            Ok(ValueType::new_reference(&reference, lu_dog))
        }
        Type::Self_ => match enclosing_type {
            Some(ty) => Ok(ty.clone()),
            None => Err(DwarfError::BadSelf { span: span.clone() }),
        },
        Type::String => {
            let ty = Ty::new_s_string();
            Ok(ValueType::new_ty(&new_ref!(Ty, ty), lu_dog))
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
                    None => Err(DwarfError::BadSelf {
                        span: tok.1.clone(),
                    }),
                }
            } else if name == "String" {
                Ok(ValueType::new_ty(&new_ref!(Ty, Ty::new_s_string()), lu_dog))
            } else if name == "Uuid" {
                Ok(ValueType::new_ty(&new_ref!(Ty, Ty::new_s_uuid()), lu_dog))
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
                        return Ok(ValueType::new_ty(&new_ref!(Ty, ty.to_owned()), lu_dog));
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
                    Ok(ValueType::new_ty(&new_ref!(Ty, ty.to_owned()), lu_dog))
                } else {
                    if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(name) {
                        let ws = lu_dog.exhume_woog_struct(id).unwrap();
                        Ok(ValueType::new_woog_struct(&ws, lu_dog))
                    } else {
                        Ok(ValueType::new_unknown(lu_dog))
                    }
                }
            }
        }
        Type::Uuid => {
            let ty = Ty::new_s_uuid();
            Ok(ValueType::new_ty(&new_ref!(Ty, ty.to_owned()), lu_dog))
        }
        道 => todo!("{:?}", 道),
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
    lhs: &RefType<ValueType>,
    rhs: &RefType<ValueType>,
    span: Span,
    lu_dog: &LuDogStore,
    sarzak: &SarzakStore,
    models: &[SarzakStore],
) -> Result<()> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "single")] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    match (&*s_read!(lhs), &*s_read!(rhs)) {
        (_, ValueType::Empty(_)) => Ok(()),
        (ValueType::Empty(_), _) => Ok(()),
        (_, ValueType::Unknown(_)) => Ok(()),
        (ValueType::Unknown(_), _) => Ok(()),
        (lhs_t, rhs_t) => {
            if lhs_t == rhs_t {
                Ok(())
            } else {
                let lhs = PrintableValueType(lhs.clone(), lu_dog, sarzak, models);
                let rhs = PrintableValueType(rhs.clone(), lu_dog, sarzak, models);

                Err(DwarfError::TypeMismatch {
                    expected: lhs.to_string(),
                    found: rhs.to_string(),
                    span,
                })
            }
        }
    }
}

pub(crate) struct PrintableValueType<'a, 'b, 'c>(
    pub RefType<ValueType>,
    pub &'a LuDogStore,
    pub &'b SarzakStore,
    pub &'c [SarzakStore],
);

impl<'a, 'b, 'c> fmt::Display for PrintableValueType<'a, 'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = s_read!(self.0);
        let lu_dog = self.1;
        let sarzak = self.2;
        let models = self.3;

        match &*value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = lu_dog.exhume_import(import).unwrap();
                let import = s_read!(import);
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap();
                let list = s_read!(list);
                let ty = list.r36_value_type(&lu_dog)[0].clone();
                write!(f, "[{}]", PrintableValueType(ty, lu_dog, sarzak, models))
            }
            ValueType::Range(_) => write!(f, "<range>"),
            ValueType::Reference(ref reference) => {
                let reference = lu_dog.exhume_reference(reference).unwrap();
                let reference = s_read!(reference);
                let ty = reference.r35_value_type(&lu_dog)[0].clone();
                write!(f, "&{}", PrintableValueType(ty, lu_dog, sarzak, models))
            }
            ValueType::Ty(ref ty) => {
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
                        Ty::SString(_) => write!(f, "String"),
                        Ty::SUuid(_) => write!(f, "Uuid"),
                        gamma => {
                            error!("deal with sarzak type {:?}", gamma);
                            write!(f, "todo")
                        }
                    }
                } else {
                    // It's not a sarzak type, so it must be an object imported from
                    // one of the model domains.
                    for model in &*models {
                        if let Some(Ty::Object(ref object)) = model.exhume_ty(ty) {
                            if let Some(object) = model.exhume_object(object) {
                                return write!(f, "{}Proxy", object.name);
                            }
                        }
                    }
                    write!(f, "<unknown object>")
                }
            }
            ValueType::Unknown(_) => write!(f, "<unknown>"),
            ValueType::WoogOption(ref option) => {
                let option = lu_dog.exhume_woog_option(option).unwrap();
                let option = s_read!(option);
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap();
                        let some = s_read!(some);
                        let value = s_read!(some.r23_x_value(&lu_dog)[0]).clone();
                        let ty = value.r24_value_type(&lu_dog)[0].clone();
                        write!(
                            f,
                            "Some({})",
                            PrintableValueType(ty, lu_dog, sarzak, models)
                        )
                    }
                }
            }
            ValueType::WoogStruct(ref woog_struct) => {
                debug!("woog_struct {:?}", woog_struct);
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                let woog_struct = s_read!(woog_struct);
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.exhume_z_object_store(id).unwrap();
                let zobject_store = s_read!(zobject_store);
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}
