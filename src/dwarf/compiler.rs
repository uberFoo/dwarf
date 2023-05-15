use core::fmt;
use std::{fs::File, io::prelude::*, ops::Range, path::PathBuf, sync::Arc, sync::RwLock};

use ansi_term::Colour;
use heck::{ToShoutySnakeCase, ToUpperCamelCase};
use log;
use snafu::{location, prelude::*, Location};
use uuid::Uuid;

use crate::dwarf::{
    DwarfError, Expression as ParserExpression, Item, ObjectIdNotFoundSnafu, Result, Spanned,
    Statement as ParserStatement, Type, TypeMismatchSnafu,
};
use sarzak::{
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{
            Block, Call, Error, ErrorExpression, Expression, ExpressionStatement, Field,
            FieldExpression, ForLoop, Function, Implementation, Import, Index, IntegerLiteral,
            LetStatement, Literal, LocalVariable, Parameter, Print, Statement, StaticMethodCall,
            StringLiteral, StructExpression, Value, ValueEnum, ValueType, Variable,
            VariableExpression, WoogOption, WoogStruct, XIf,
        },
        Argument, Binary, BooleanLiteral, Comparison, FieldAccess, FloatLiteral, List, ListElement,
        ListExpression, MethodCall, Operator, Reference, ResultStatement, VariableEnum,
        WoogOptionEnum, XReturn,
    },
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
};

macro_rules! link_parameter {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = $next.read().unwrap();
        if let Some(last) = $last {
            let last = $store.exhume_parameter(&last).unwrap().clone();
            let mut last = last.write().unwrap();
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_argument {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = $next.read().unwrap();
        if let Some(last) = $last {
            let last = $store.exhume_argument(&last).unwrap().clone();
            let mut last = last.write().unwrap();
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_statement {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = $next.read().unwrap();
        if let Some(last) = $last {
            let last = $store.exhume_statement(&last).unwrap().clone();
            let mut last = last.write().unwrap();
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_list_element {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = $next.read().unwrap();
        if let Some(last) = $last {
            let last = $store.exhume_list_element(&last).unwrap().clone();
            let mut last = last.write().unwrap();
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! debug {
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::debug!(
                "{} --> {:?}\n  --> {}:{}:{}",
                Colour::Yellow.underline().paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:expr) => {
        log::debug!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
}

macro_rules! error {
    ($msg:literal, $($arg:expr),*) => {
        $(
            log::error!(
                "{} --> {:?}\n  --> {}:{}:{}",
                Colour::Red.paint($msg),
                $arg,
                file!(),
                line!(),
                column!()
            );
        )*
    };
    ($arg:expr) => {
        log::error!("{:?}\n  --> {}:{}:{}", $arg, file!(), line!(), column!());
    };
}

// These below are just to avoid cloning things.
struct ConveyFunc<'a> {
    name: &'a str,
    params: &'a [(Spanned<String>, Spanned<Type>)],
    return_type: &'a Spanned<Type>,
    statements: &'a Spanned<ParserExpression>,
}

impl<'a> ConveyFunc<'a> {
    fn new(
        name: &'a str,
        params: &'a [(Spanned<String>, Spanned<Type>)],
        return_type: &'a Spanned<Type>,
        statements: &'a Spanned<ParserExpression>,
    ) -> Self {
        Self {
            name,
            params,
            return_type,
            statements,
        }
    }
}

struct ConveyStruct<'a> {
    name: &'a str,
    fields: &'a [(Spanned<String>, Spanned<Type>)],
}

impl<'a> ConveyStruct<'a> {
    fn new(name: &'a str, fields: &'a [(Spanned<String>, Spanned<Type>)]) -> Self {
        Self { name, fields }
    }
}

struct ConveyImpl<'a> {
    name: &'a str,
    funcs: &'a [Spanned<Item>],
}

impl<'a> ConveyImpl<'a> {
    fn new(name: &'a str, funcs: &'a [Spanned<Item>]) -> Self {
        Self { name, funcs }
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
pub fn populate_lu_dog(
    out_dir: Option<&PathBuf>,
    ast: &[Spanned<Item>],
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<LuDogStore> {
    let mut lu_dog = LuDogStore::new();

    walk_tree(out_dir, ast, &mut lu_dog, models, sarzak)?;

    Ok(lu_dog)
}

fn walk_tree(
    out_dir: Option<&PathBuf>,
    ast: &[Spanned<Item>],
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
            (Item::Function(name, params, return_type, stmts), _span) => {
                funcs.push(ConveyFunc::new(&name.0, params, return_type, stmts))
            }
            (Item::Implementation(name, funcs), _span) => {
                implementations.push(ConveyImpl::new(&name.0, funcs))
            }
            // Imports can happen any time, I think.
            (Item::Import(path, alias), _span) => inter_import(&path.0, alias, lu_dog)?,
            (Item::Struct(name, fields), _span) => structs.push(ConveyStruct::new(&name.0, fields)),
        }
    }

    // Put the type information in first.
    for ConveyStruct { name, fields } in structs {
        debug!("Intering struct {}", name);
        inter_struct(&name, &fields, lu_dog, models, sarzak)?;
    }

    // Using the type information, and the input, inter the implementation blocks.
    for ConveyImpl { name, funcs } in implementations {
        inter_implementation(&name, &funcs, lu_dog, models, sarzak)?;
    }

    // Finally, inter the loose functions.
    for ConveyFunc {
        name,
        params,
        return_type,
        statements,
    } in funcs
    {
        inter_func(
            &name,
            &params,
            &return_type,
            &statements,
            None,
            None,
            lu_dog,
            models,
            sarzak,
        )?;
    }

    if let Some(out_dir) = out_dir {
        // Now write a file containing the WoogStruct id's.
        let mut path = PathBuf::from(out_dir);
        path.push("woog_structs.rs");

        let mut file = File::create(&path).unwrap();
        writeln!(file, "use uuid::{{uuid, Uuid}};\n").unwrap();

        for ws in lu_dog.iter_woog_struct() {
            let ws = ws.read().unwrap();
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
    impl_block: Option<&Arc<RwLock<Implementation>>>,
    impl_ty: Option<&Arc<RwLock<ValueType>>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_func", name);

    let block = Block::new(Uuid::new_v4(), lu_dog);

    let name = name.de_sanitize();
    let stmts = if let ParserExpression::Block(stmts) = &stmts.0 {
        stmts
    } else {
        panic!("Expected a block expression");
    };

    let ret_ty = get_value_type(&return_type.0, impl_ty, lu_dog, models, sarzak)?;
    let func = Function::new(name.to_owned(), &block, impl_block, &ret_ty, lu_dog);
    // Create a type for our function
    ValueType::new_function(&func, lu_dog);

    let mut last_param_uuid: Option<Uuid> = None;
    for ((param_name, _), (param_ty, _)) in params {
        debug!("inter_func param name", param_name);
        debug!("inter_func param ty", param_ty);
        let param = Parameter::new(&func, None, lu_dog);
        debug!("inter_func param param", param);
        let var = Variable::new_parameter(param_name.to_owned(), &param, lu_dog);
        debug!("inter_func param var", var);
        // 🚧 We'll need to do something about this soon. Actually, it never belonged
        // here. It only makes sense that you can only have values in a block. Now the
        // model enforces that.
        //
        // That said, we need to introduce the values into the block, so that we don't
        // error out when parsing the statements.
        //
        let param_ty = get_value_type(&param_ty, impl_ty.clone(), lu_dog, models, sarzak)?;
        debug!("inter_func param param_ty", param_ty);
        let _value = Value::new_variable(&block, &param_ty, &var, lu_dog);
        last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
        debug!("inter_func param last_param_uuid", last_param_uuid);
    }

    let stmts: Vec<Arc<RwLock<ParserStatement>>> = stmts
        .iter()
        .map(|stmt| Arc::new(RwLock::new(stmt.0.clone())))
        .collect();
    inter_statements(&stmts, &block, lu_dog, models, sarzak)?;

    Ok(())
}

pub fn inter_statement(
    stmt: &Arc<RwLock<ParserStatement>>,
    block: &Arc<RwLock<Block>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(Arc<RwLock<Statement>>, Arc<RwLock<ValueType>>)> {
    debug!("inter_statement", stmt);

    match &*stmt.read().unwrap() {
        //
        // Expression
        //
        ParserStatement::Expression((expr, _)) => {
            let (expr, _) = inter_expression(
                &Arc::new(RwLock::new(expr.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ExpressionStatement::new(&expr, lu_dog);
            let stmt = Statement::new_expression_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ValueType::new_empty(lu_dog)))
        }
        //
        // Let
        //
        ParserStatement::Let((var_name, _), type_, (expr, _)) => {
            // We only want one storage location per name per block, so we look
            // for, and remove an existing one -- all the way up to the value.
            let values = lu_dog
                .iter_value()
                .filter(|value| value.read().unwrap().block == block.read().unwrap().id)
                .collect::<Vec<_>>();
            for value in values {
                let value = value.read().unwrap();
                match value.subtype {
                    ValueEnum::Variable(ref var) => {
                        let var = lu_dog.exhume_variable(var).unwrap();
                        let var = var.read().unwrap();
                        if var.name == *var_name {
                            match var.subtype {
                                VariableEnum::LocalVariable(ref local) => {
                                    lu_dog.exorcise_local_variable(local);
                                    lu_dog.exorcise_variable(&var.id);
                                    lu_dog.exorcise_value(&value.id);
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

            dbg!(&var, &block);

            // Now parse the RHS, which is an expression.
            let (expr, ty) = inter_expression(
                &Arc::new(RwLock::new(expr.to_owned())),
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

            let foo = PrintableValueType(ty.clone(), lu_dog, sarzak, models);
            debug!("inter_statement let foo", foo.to_string());

            // let ty = ty.read().unwrap().to_owned();
            if let ValueType::Unknown(_) = &*ty.read().unwrap() {
                error!("Unknown type for variable", var_name);
            }

            // Create a variable, now that we (hopefully) have a type from the expression.
            let _value = Value::new_variable(&block, &ty, &var, lu_dog);

            // Setup the let statement itself.
            let stmt = LetStatement::new(&expr, &local, lu_dog);
            let stmt = Statement::new_let_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ValueType::new_empty(lu_dog)))
        }
        //
        // Result
        //
        ParserStatement::Result((ref expr, _)) => {
            let (expr, ty) = inter_expression(
                &Arc::new(RwLock::new(expr.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let stmt = ResultStatement::new(&expr, lu_dog);
            let stmt = Statement::new_result_statement(&block, None, &stmt, lu_dog);

            Ok((stmt, ty))
        }
        道 => todo!("{:?}", 道),
    }
}

fn inter_statements(
    statements: &[Arc<RwLock<ParserStatement>>],
    block: &Arc<RwLock<Block>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<Arc<RwLock<ValueType>>> {
    let mut value_type = ValueType::new_empty(lu_dog);

    let mut last_stmt_uuid: Option<Uuid> = None;
    for stmt in statements {
        let (stmt, ty) = inter_statement(stmt, block, lu_dog, models, sarzak)?;
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
    expr: &Arc<RwLock<ParserExpression>>,
    block: &Arc<RwLock<Block>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<(Arc<RwLock<Expression>>, Arc<RwLock<ValueType>>)> {
    debug!("inter_expression", expr);

    match &*expr.read().unwrap() {
        //
        // Addition
        //
        ParserExpression::Addition(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &Arc::new(RwLock::new(lhs.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &Arc::new(RwLock::new(rhs.0.to_owned())),
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

            ensure!(*lhs_ty.read().unwrap() == *rhs_ty.read().unwrap(), {
                let lhs_ty = PrintableValueType(lhs_ty, lu_dog, sarzak, models);
                let rhs_ty = PrintableValueType(rhs_ty, lu_dog, sarzak, models);

                TypeMismatchSnafu {
                    expected: lhs_ty.to_string(),
                    found: rhs_ty.to_string(),
                }
            });

            let expr = Binary::new_addition(lu_dog);
            let expr = Operator::new_binary(Some(&rhs), &lhs, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            Ok((expr, lhs_ty))
        }
        //
        // Assignment
        //
        ParserExpression::Assignment(ref lhs, ref rhs) => {
            dbg!("raw", &lhs, &rhs);
            let (lhs, lhs_ty) = inter_expression(
                &Arc::new(RwLock::new(lhs.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &Arc::new(RwLock::new(rhs.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            dbg!("expr", &lhs, &rhs);
            dbg!(&lhs_ty, &rhs_ty);

            ensure!(*lhs_ty.read().unwrap() == *rhs_ty.read().unwrap(), {
                let lhs_ty = PrintableValueType(lhs_ty, lu_dog, sarzak, models);
                let rhs_ty = PrintableValueType(rhs_ty, lu_dog, sarzak, models);

                TypeMismatchSnafu {
                    expected: lhs_ty.to_string(),
                    found: rhs_ty.to_string(),
                }
            });

            let expr = Binary::new_assignment(lu_dog);
            let expr = Operator::new_binary(Some(&rhs), &lhs, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            Ok((expr, lhs_ty))
        }
        //
        // Block
        //
        ParserExpression::Block(ref stmts) => {
            let block = Block::new(Uuid::new_v4(), lu_dog);
            debug!("ParserExpression::Block", block);
            let stmts: Vec<Arc<RwLock<ParserStatement>>> = stmts
                .iter()
                .map(|stmt| Arc::new(RwLock::new(stmt.0.to_owned())))
                .collect();
            Ok((
                Expression::new_block(&block, lu_dog),
                inter_statements(&stmts, &block, lu_dog, models, sarzak)?,
            ))
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
            Ok((
                Expression::new_literal(&Literal::new_boolean_literal(&literal, lu_dog), lu_dog),
                ValueType::new_ty(&Ty::new_boolean(), lu_dog),
            ))
        }
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
            Ok((expr, ValueType::new_empty(lu_dog)))
        }
        //
        // FieldAccess
        //
        ParserExpression::FieldAccess(lhs, rhs) => {
            debug!("ParserExpression::FieldAccess lhs", lhs);
            debug!("ParserExpression::FieldAccess rhs", rhs);

            let (lhs, lhs_ty) = inter_expression(
                &Arc::new(RwLock::new((*lhs).0.clone())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let (rhs, rhs_ty) = inter_expression(
                &Arc::new(RwLock::new((*rhs).0.clone())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let id = lhs_ty.read().unwrap().id();
            let ty = lu_dog.exhume_value_type(&id).unwrap();
            let ty = ty.read().unwrap();

            match &*ty {
                ValueType::Ty(ref id) => {
                    for model in models {
                        if let Some(Ty::Object(ref _object)) = model.exhume_ty(id) {
                            // let object = model.exhume_object(object).unwrap();
                            let expr =
                                FieldAccess::new("💥figure this out🖕".to_owned(), &lhs, lu_dog);
                            let expr = Expression::new_field_access(&expr, lu_dog);

                            // 🚧 Can we not do better?
                            return Ok((expr, ValueType::new_unknown(lu_dog)));
                        }
                    }

                    // Return an error (really need to get result in here) if it wasn't
                    // in one of the models.
                    let error = ErrorExpression::new(
                        format!("💥 {:?} is not a proxy object\n", ty),
                        lu_dog,
                    );
                    let expr = Expression::new_error_expression(&error, lu_dog);
                    // 🚧
                    // Returning an empty, because the error stuff in ValueType is fucked.
                    Ok((expr, ValueType::new_empty(lu_dog)))
                }
                ValueType::WoogStruct(ref id) => {
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                    let expr = FieldAccess::new("💥figure this out🖕".to_owned(), &lhs, lu_dog);
                    let expr = Expression::new_field_access(&expr, lu_dog);
                    let field = woog_struct.read().unwrap();
                    let field = field.r7_field(lu_dog);
                    let field = field
                        .iter()
                        .find(|f| f.read().unwrap().name == "💥figure this out🖕");

                    // We need to grab the type from the field: what we have above is the type
                    // of the struct.
                    if let Some(field) = field {
                        let ty = field.read().unwrap().r5_value_type(lu_dog)[0].clone();
                        Ok((expr, ty))
                    } else {
                        let error = ErrorExpression::new("💥 No such field\n".to_owned(), lu_dog);
                        let expr = Expression::new_error_expression(&error, lu_dog);
                        // 🚧
                        // Returning an empty, because the error stuff in ValueType is fucked.
                        Ok((expr, ValueType::new_empty(lu_dog)))
                    }
                }
                ty => {
                    let error =
                        ErrorExpression::new(format!("💥 {:?} is not a struct\n", ty), lu_dog);
                    let expr = Expression::new_error_expression(&error, lu_dog);
                    // 🚧 Returning an empty, because the error stuff in ValueType is fucked.
                    // I wonder what we mean?
                    Ok((expr, ValueType::new_empty(lu_dog)))
                }
            }
        }
        //
        // FloatLiteral
        //
        ParserExpression::FloatLiteral(literal) => Ok((
            Expression::new_literal(
                &Literal::new_float_literal(&FloatLiteral::new(*literal, lu_dog), lu_dog),
                lu_dog,
            ),
            ValueType::new_ty(&Ty::new_integer(), lu_dog),
        )),
        //
        // For Loop
        //
        ParserExpression::For(iter, collection, body) => {
            debug!("ParserExpresssion::For");
            let iter = iter.0.clone();

            let collection = Arc::new(RwLock::new(collection.0.clone()));

            // 🚧 Should we be checking this to ensure that it's an iterable?
            let (collection, _collection_ty) =
                inter_expression(&collection, block, lu_dog, models, sarzak)?;

            let body = Arc::new(RwLock::new((&body.0).to_owned()));
            let (body, _body_ty) = inter_expression(&body, block, lu_dog, models, sarzak)?;

            let body = if let Expression::Block(body) = body.read().unwrap().clone() {
                body
            } else {
                panic!("Expected a block expression");
            };
            let body = lu_dog.exhume_block(&body).unwrap();

            let for_loop = ForLoop::new(iter, &body, &collection, lu_dog);
            let expr = Expression::new_for_loop(&for_loop, lu_dog);

            Ok((expr, ValueType::new_empty(lu_dog)))
        }
        //
        // FunctionCall
        //
        ParserExpression::FunctionCall(func, params) => {
            debug!("ParserExpression::FunctionCall", func);
            let func = &func.0;
            let params: Vec<&ParserExpression> = params.iter().map(|param| &param.0).collect();
            debug!("ParserExpression::FunctionCall", params);
            let (func_expr, ret_ty) = inter_expression(
                &Arc::new(RwLock::new(func.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let func_call = Call::new_function_call(Some(&func_expr), lu_dog);
            let func = Expression::new_call(&func_call, lu_dog);

            let mut last_arg_uuid: Option<Uuid> = None;
            for param in params {
                let (arg_expr, ty) = inter_expression(
                    &Arc::new(RwLock::new(param.to_owned())),
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let _value = Value::new_expression(&block, &ty, &arg_expr, lu_dog);
                let arg = Argument::new(None, &func_call, &arg_expr, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "ParserExpression::FunctionCall exit",
                (&func_call, &func_call.read().unwrap().r28_argument(lu_dog))
            );

            Ok((func, ret_ty))
        }
        //
        // If
        //
        ParserExpression::If(conditional, true_block, false_block) => {
            debug!("ParserExpression::If", conditional);
            let conditional = Arc::new(RwLock::new(conditional.0.clone()));
            let (conditional, conditional_ty) =
                inter_expression(&conditional, block, lu_dog, models, sarzak)?;
            debug!("ParserExpression::If", conditional_ty);

            // We really need to get some error handling in here.
            let ty = conditional_ty.read().unwrap().to_owned();
            if let ValueType::Ty(ref ty) = conditional_ty.read().unwrap().to_owned() {
                error!("exhume Ty from sarzak -- bool check", ty);
                let ty = sarzak.exhume_ty(ty).unwrap();
                if let Ty::Boolean(_) = ty {
                    // We're good.
                } else {
                    panic!("Expected a boolean");
                }
            } else {
                let ty = PrintableValueType(Arc::new(RwLock::new(ty)), lu_dog, sarzak, models);
                return Err(DwarfError::TypeMismatch {
                    expected: "boolean".to_owned(),
                    found: ty.to_string(),
                });
            }

            let true_block = Arc::new(RwLock::new(true_block.0.clone()));
            let (true_block, _true_ty) =
                inter_expression(&true_block, block, lu_dog, models, sarzak)?;
            let true_block =
                if let Expression::Block(true_block) = true_block.read().unwrap().clone() {
                    true_block
                } else {
                    panic!("Expected a block expression");
                };
            let true_block = lu_dog.exhume_block(&true_block).unwrap();

            let false_block = if let Some(false_block) = false_block {
                let false_block = Arc::new(RwLock::new(false_block.0.clone()));
                let (false_block, _false_ty) =
                    inter_expression(&false_block, block, lu_dog, models, sarzak)?;
                let false_block =
                    if let Expression::Block(false_block) = false_block.read().unwrap().clone() {
                        false_block
                    } else {
                        panic!("Expected a block expression");
                    };
                let false_block = lu_dog.exhume_block(&false_block).unwrap();
                Some(false_block)
            } else {
                None
            };

            let if_expr = XIf::new(false_block.as_ref(), &true_block, &conditional, lu_dog);
            let expr = Expression::new_x_if(&if_expr, lu_dog);

            // 🚧 I'd really like to see this return the type of the if expression.
            Ok((expr, ValueType::new_empty(lu_dog)))
        }
        //
        // Index
        //
        ParserExpression::Index(target, index) => {
            let (target, target_ty) = inter_expression(
                &Arc::new(RwLock::new(target.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (index, index_ty) = inter_expression(
                &Arc::new(RwLock::new(index.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;

            let index = Index::new(&index, &target, lu_dog);
            let int_ty = ValueType::new_ty(&Ty::new_integer(), lu_dog);
            ensure!(&*int_ty.read().unwrap() == &*index_ty.read().unwrap(), {
                let int_ty = PrintableValueType(int_ty, lu_dog, sarzak, models);
                let index_ty = PrintableValueType(index_ty, lu_dog, sarzak, models);
                TypeMismatchSnafu {
                    expected: int_ty.to_string(),
                    found: index_ty.to_string(),
                }
            });

            let expr = Expression::new_index(&index, lu_dog);

            let ty = ValueType::new_ty(&Ty::new_integer(), lu_dog);

            ensure!(&*index_ty.read().unwrap() == &*ty.read().unwrap(), {
                let index_ty = PrintableValueType(index_ty, lu_dog, sarzak, models);
                let ty = PrintableValueType(ty, lu_dog, sarzak, models);
                TypeMismatchSnafu {
                    expected: ty.to_string(),
                    found: index_ty.to_string(),
                }
            });

            // 🚧 We should really check that the target type is some sort of list.

            Ok((expr, target_ty))
        }
        //
        // IntegerLiteral
        //
        ParserExpression::IntegerLiteral(literal) => Ok((
            Expression::new_literal(
                &Literal::new_integer_literal(&IntegerLiteral::new(*literal, lu_dog), lu_dog),
                lu_dog,
            ),
            ValueType::new_ty(&Ty::new_integer(), lu_dog),
        )),
        //
        // LessThanOrEqual
        //
        ParserExpression::LessThanOrEqual(ref lhs, ref rhs) => {
            let (lhs, _lhs_ty) = inter_expression(
                &Arc::new(RwLock::new(lhs.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, _rhs_ty) = inter_expression(
                &Arc::new(RwLock::new(rhs.0.to_owned())),
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

            let expr = Comparison::new_less_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(Some(&rhs), &lhs, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean();
            let ty = ValueType::new_ty(&ty, lu_dog);

            Ok((expr, ty))
        }
        //
        // List
        //
        ParserExpression::List(ref elements) => {
            if elements.is_empty() {
                let list = List::new(&ValueType::new_empty(lu_dog), lu_dog);
                Ok((
                    Expression::new_list_expression(&ListExpression::new(None, lu_dog), lu_dog),
                    ValueType::new_list(&list, lu_dog),
                ))
            } else {
                let mut elements = elements.iter();
                // I'm going to get the type of the first element, and then check
                // that each subsequent element is the same type.
                let (first, first_ty) = inter_expression(
                    &Arc::new(RwLock::new(elements.next().unwrap().0.to_owned())),
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;

                let list = List::new(&first_ty, lu_dog);
                let element = ListElement::new(&first, None, lu_dog);
                let _ = Expression::new_list_element(&element, lu_dog);
                let list_expr = ListExpression::new(Some(&element), lu_dog);

                let mut last_element_uuid: Option<Uuid> = Some(element.read().unwrap().id);
                while let Some(element) = elements.next() {
                    let (element, element_ty) = inter_expression(
                        &Arc::new(RwLock::new(element.0.to_owned())),
                        block,
                        lu_dog,
                        models,
                        sarzak,
                    )?;

                    let element = ListElement::new(&element, None, lu_dog);
                    last_element_uuid = link_list_element!(last_element_uuid, element, lu_dog);
                    let _ = Expression::new_list_element(&element, lu_dog);

                    ensure!(
                        &*first_ty.read().unwrap() == &*element_ty.read().unwrap(),
                        {
                            let first_ty = PrintableValueType(first_ty, lu_dog, sarzak, models);
                            let element_ty = PrintableValueType(element_ty, lu_dog, sarzak, models);
                            TypeMismatchSnafu {
                                expected: first_ty.to_string(),
                                found: element_ty.to_string(),
                            }
                        }
                    );
                }

                Ok((
                    Expression::new_list_expression(&list_expr, lu_dog),
                    ValueType::new_list(&list, lu_dog),
                ))
            }
        }
        //
        // LocalVariable
        //
        ParserExpression::LocalVariable(name) => {
            debug!("ParserExpression::LocalVariable", name);
            // We need to return an expression and a type.
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
                .iter_value()
                .filter(|value| value.read().unwrap().block == block.read().unwrap().id)
                .collect::<Vec<Arc<RwLock<Value>>>>();

            debug!("ParserExpression::LocalVariable values", values);

            // Now search for a value that's a Variable, and see if the access matches
            // the variable.
            let mut expr_type_tuples = values
                .iter()
                .filter_map(|value| {
                    debug!("ParserExpression::LocalVariable: value", value);

                    match value.read().unwrap().subtype {
                        ValueEnum::Expression(ref _expr) => {
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
                            // Fuck me. I've been debugging something that's completely normal. I'm
                            // stoned now, so don't blame yourself later for it being that. What's
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
                        ValueEnum::Variable(ref var) => {
                            let var = lu_dog.exhume_variable(var).unwrap().read().unwrap().clone();
                            debug!("ParserExpression::LocalVariable: var", var);
                            // Check the name
                            if var.name == *name {
                                match var.subtype {
                                    VariableEnum::LocalVariable(_) => {
                                        let value =
                                            var.r11_value(lu_dog)[0].read().unwrap().clone();
                                        let ty = value.r24_value_type(lu_dog)[0].clone();

                                        let lhs_ty =
                                            PrintableValueType(ty.clone(), lu_dog, sarzak, models);

                                        dbg!(name, &value, lhs_ty.to_string());

                                        // Ok, so I parsed a local variable expression. We need to create
                                        // a VariableExpression, and it in turn needs an Expression, which
                                        // needs a Value, and finally a ValueType.
                                        // Except that I don't think we want to create values in the walker.
                                        // Doing so wreaks havoc downstream in the interpreter, because
                                        // It sees that value and expects that it's been evaluated.
                                        // And we got here by searching for a value anyway.
                                        //
                                        // We don't want to create more than one of these.
                                        let expr = lu_dog
                                            .iter_variable_expression()
                                            .find(|expr| expr.read().unwrap().name == *name);

                                        let expr = if let Some(expr) = expr {
                                            expr.read().unwrap().r15_expression(lu_dog)[0].clone()
                                        } else {
                                            let expr =
                                                VariableExpression::new(name.to_owned(), lu_dog);
                                            debug!("created a new variable expression", expr);
                                            Expression::new_variable_expression(&expr, lu_dog)
                                        };

                                        Some((expr, ty))
                                    }
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                    }
                })
                .collect::<Vec<(Arc<RwLock<Expression>>, Arc<RwLock<ValueType>>)>>();
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

            debug!("ParserExpression::LocalVariable: expr_ty", expr_type_tuples);

            // Why are we taking the last one? -- Oh, read above.
            if let Some(expr_ty_tuple) = expr_type_tuples.pop() {
                debug!("ParserExpression::LocalVariable: returning", expr_ty_tuple);
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
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(&expr, lu_dog);

                Ok((expr, ValueType::new_unknown(lu_dog)))
            }
        }
        //
        // MethodCall
        //
        ParserExpression::MethodCall(instance, (method, _), params) => {
            debug!("ParserExpression::MethodCall", instance);

            let (instance, instance_ty) = inter_expression(
                &Arc::new(RwLock::new((*instance).0.clone())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let meth = MethodCall::new(method.to_owned(), lu_dog);
            let call = Call::new_method_call(Some(&instance), &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            let mut last_arg_uuid: Option<Uuid> = None;
            let params: Vec<&ParserExpression> = params.iter().map(|param| &param.0).collect();
            for param in params {
                let (arg_expr, ty) = inter_expression(
                    &Arc::new(RwLock::new(param.to_owned())),
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let _value = Value::new_expression(&block, &ty, &arg_expr, lu_dog);
                let arg = Argument::new(None, &call, &arg_expr, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            Ok((expr, instance_ty))
        }
        ParserExpression::Print(expr) => {
            let (expr, ty) = inter_expression(
                &Arc::new(RwLock::new((*expr).0.clone())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let print = Print::new(&expr, lu_dog);

            Ok((Expression::new_print(&print, lu_dog), ty))
        }
        //
        // Return
        //
        ParserExpression::Return(expr) => {
            let (expr, ty) = inter_expression(
                &Arc::new(RwLock::new((*expr).0.clone())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let ret = XReturn::new(&expr, lu_dog);

            Ok((Expression::new_x_return(&ret, lu_dog), ty))
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

            debug!("ParserExpression::StaticMethodCall", type_name);
            // let type_name = if let Token::Ident(obj) = obj {
            //     obj.de_sanitize().to_owned()
            // } else if obj == &Token::Uuid {
            //     "Uuid".to_owned()
            // } else {
            //     panic!("I don't think that we should ever see anything other than an object or Uuid here: {:?}", obj);
            // };

            let meth = StaticMethodCall::new(method.to_owned(), type_name.to_owned(), lu_dog);
            let call = Call::new_static_method_call(None, &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            debug!("ParserExpression::StaticMethodCall: name", type_name);
            debug!("ParserExpression::StaticMethodCall: method", method);

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // So we are down to this. I suppose that we can check the obj against
            // what's been entered thus far. Really this should be a second pass
            // then. For now, I'm going to hack something in...
            // We could do something with the imports...
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            let ty = if type_name == "Uuid" && method == "new" {
                ValueType::new_ty(&Ty::new_s_uuid(), lu_dog)
            } else {
                debug!(
                    "ParserExpression::StaticMethodCall: looking up type",
                    type_name
                );

                let mut ty = ValueType::new_unknown(lu_dog);
                for model in models {
                    if let Some(obj) = model.exhume_object_id_by_name(&type_name) {
                        let id = if let Some(s) = lu_dog
                            .iter_woog_struct()
                            .find(|s| s.read().unwrap().object == Some(obj))
                        {
                            s.read().unwrap().id
                        } else {
                            model.exhume_ty(&obj).unwrap().id()
                        };

                        ty = lu_dog.exhume_value_type(&id).unwrap().clone();
                        break;
                    }
                }
                ty
            };

            let mut last_arg_uuid: Option<Uuid> = None;
            let params: Vec<&ParserExpression> = params.iter().map(|param| &param.0).collect();
            for param in params {
                let (arg_expr, ty) = inter_expression(
                    &Arc::new(RwLock::new(param.to_owned())),
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let _value = Value::new_expression(&block, &ty, &arg_expr, lu_dog);
                let arg = Argument::new(None, &call, &arg_expr, lu_dog);
                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            Ok((expr, ty))
        }
        //
        // StringLiteral
        //
        ParserExpression::StringLiteral(literal) => {
            debug!("ParserExpression::StringLiteral", literal);
            Ok((
                Expression::new_literal(
                    &Literal::new_string_literal(
                        &StringLiteral::new(literal.to_owned(), lu_dog),
                        lu_dog,
                    ),
                    lu_dog,
                ),
                ValueType::new_ty(&Ty::new_s_string(), lu_dog),
            ))
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

            debug!("ParserExpression::Struct", name);

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
                let (field_expr, _) = inter_expression(
                    &Arc::new(RwLock::new(field_expr.0.to_owned())),
                    block,
                    lu_dog,
                    models,
                    sarzak,
                )?;
                let _field = FieldExpression::new(name.0.to_owned(), &field_expr, &expr, lu_dog);
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

                    return Ok((
                        Expression::new_struct_expression(&expr, lu_dog),
                        ValueType::new_ty(&ty, lu_dog),
                    ));
                }
            }

            // Ilove that the type of the thing is the same as the thing itself.
            return Ok((
                Expression::new_struct_expression(&expr, lu_dog),
                ValueType::new_woog_struct(&woog_struct, lu_dog),
            ));

            panic!("badness happened: {}", name);
        }
        //
        // Subtraction
        //
        ParserExpression::Subtraction(ref lhs, ref rhs) => {
            let (lhs, lhs_ty) = inter_expression(
                &Arc::new(RwLock::new(lhs.0.to_owned())),
                block,
                lu_dog,
                models,
                sarzak,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &Arc::new(RwLock::new(rhs.0.to_owned())),
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
            let expr = Operator::new_binary(Some(&rhs), &lhs, &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            Ok((expr, lhs_ty))
        }
        道 => Err(DwarfError::NoImplementation {
            missing: format!("{:?}", 道),
            location: location!(),
        }),
    }
}

fn inter_import(
    path: &Vec<Spanned<String>>,
    _alias: &Option<(String, Range<usize>)>,
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
        location: location!(),
    })
}

fn inter_implementation(
    name: &str,
    funcs: &[Spanned<Item>],
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    let name = name.de_sanitize();

    let impl_ty = get_value_type(
        &Type::UserType((name.to_owned(), 0..0)),
        None,
        lu_dog,
        models,
        sarzak,
    )?;

    for model in models {
        let obj_id = model
            .exhume_object_id_by_name(name)
            .expect(&format!("Object {} not found", name));

        let obj = model
            .exhume_object(&obj_id)
            .expect(&format!("Object {} not found", name));

        let mt = lu_dog
            .iter_woog_struct()
            .find(|mt| mt.read().unwrap().object == Some(obj.id))
            .expect(&format!("Struct for {} not found", name))
            .clone();
        let implementation = Implementation::new(&mt, lu_dog);

        for (func, span) in funcs {
            match func {
                Item::Function(ref name, ref params, ref return_type, ref stmts) => inter_func(
                    &name.0,
                    &params,
                    &return_type,
                    &stmts,
                    Some(&implementation),
                    Some(&impl_ty),
                    lu_dog,
                    models,
                    sarzak,
                )?,
                _ => return Err(DwarfError::ImplementationBlockError { span: span.clone() }),
            }
        }
    }

    Ok(())
}

fn inter_struct(
    name: &str,
    fields: &[(Spanned<String>, Spanned<Type>)],
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<()> {
    debug!("inter_struct", name);
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

        let mt = WoogStruct::new(name.to_owned(), Some(obj.clone()), lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, _)) in fields {
            let name = name.de_sanitize();

            let ty = get_value_type(type_, None, lu_dog, models, sarzak)?;
            let _field = Field::new(name.to_owned(), &mt, &ty, lu_dog);
        }
    } else {
        let mt = WoogStruct::new(name.to_owned(), None, lu_dog);
        let _ty = ValueType::new_woog_struct(&mt, lu_dog);
        for ((name, _), (type_, _)) in fields {
            let name = name.de_sanitize();

            let ty = get_value_type(type_, None, lu_dog, models, sarzak)?;
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
    enclosing_type: Option<&Arc<RwLock<ValueType>>>,
    lu_dog: &mut LuDogStore,
    models: &[SarzakStore],
    sarzak: &SarzakStore,
) -> Result<Arc<RwLock<ValueType>>> {
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
            let inner_type = get_value_type(&(*type_).0, enclosing_type, lu_dog, models, sarzak)?;
            let list = List::new(&inner_type, lu_dog);
            Ok(ValueType::new_list(&list, lu_dog))
        }
        Type::Option(ref type_) => {
            let inner_type = get_value_type(&(*type_).0, enclosing_type, lu_dog, models, sarzak)?;
            let option = WoogOption::new_z_none(&inner_type, lu_dog);
            Ok(ValueType::new_woog_option(&option, lu_dog))
        }
        Type::Reference(ref type_) => {
            let inner_type = get_value_type(&(*type_).0, enclosing_type, lu_dog, models, sarzak)?;
            // We don't know the address yet -- we'll fix it in the interpreter.
            let reference = Reference::new(Uuid::new_v4(), false, &inner_type, lu_dog);
            Ok(ValueType::new_reference(&reference, lu_dog))
        }
        Type::String => {
            let ty = Ty::new_s_string();
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::UserType(tok) => {
            let name = tok.0.de_sanitize();

            // Deal with imports
            let import = lu_dog.iter_import().find(|import| {
                let import = import.read().unwrap();
                import.name == name || (import.has_alias && import.alias == name)
            });

            if let Some(import) = import {
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
                Ok(ValueType::new_ty(&Ty::new_s_string(), lu_dog))
            } else {
                let name = name.de_sanitize();
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
                        error!("sarzak.iter_ty", obj);
                        let obj = sarzak.exhume_object(obj).unwrap();
                        let obj = obj.name.to_upper_camel_case();
                        obj == *name || name == format!("{}Proxy", obj)
                    }
                    _ => false,
                }) {
                    Ok(ValueType::new_ty(ty, lu_dog))
                } else {
                    Err(DwarfError::UnknownType {
                        ty: name.to_string(),
                    })
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
        _ => None,
    }
}

pub(crate) struct PrintableValueType<'a, 'b, 'c>(
    pub Arc<RwLock<ValueType>>,
    pub &'a LuDogStore,
    pub &'b SarzakStore,
    pub &'c [SarzakStore],
);

impl<'a, 'b, 'c> fmt::Display for PrintableValueType<'a, 'b, 'c> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let value = self.0.read().unwrap();
        let lu_dog = self.1;
        let sarzak = self.2;
        let models = self.3;

        match &*value {
            ValueType::Empty(_) => write!(f, "()"),
            ValueType::Error(_) => write!(f, "<error>"),
            ValueType::Function(_) => write!(f, "<function>"),
            ValueType::Import(ref import) => {
                let import = lu_dog.exhume_import(import).unwrap();
                let import = import.read().unwrap();
                if import.has_alias {
                    write!(f, "{}", import.alias)
                } else {
                    write!(f, "{}", import.name)
                }
            }
            ValueType::List(ref list) => {
                let list = lu_dog.exhume_list(list).unwrap();
                let list = list.read().unwrap();
                let ty = list.r36_value_type(&lu_dog)[0].clone();
                write!(f, "[{}]", PrintableValueType(ty, lu_dog, sarzak, models))
            }
            ValueType::Reference(ref reference) => {
                let reference = lu_dog.exhume_reference(reference).unwrap();
                let reference = reference.read().unwrap();
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
                            // panic!("Bitches come!");
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
                            error!("deal with sarzak type", gamma);
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
                let option = option.read().unwrap();
                match option.subtype {
                    WoogOptionEnum::ZNone(_) => write!(f, "None"),
                    WoogOptionEnum::ZSome(ref some) => {
                        let some = lu_dog.exhume_z_some(some).unwrap();
                        let some = some.read().unwrap();
                        let value = some.r23_value(&lu_dog)[0].read().unwrap().clone();
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
                debug!("woog_struct", woog_struct);
                let woog_struct = lu_dog.exhume_woog_struct(woog_struct).unwrap();
                let woog_struct = woog_struct.read().unwrap();
                write!(f, "{}", woog_struct.name)
            }
            ValueType::ZObjectStore(ref id) => {
                let zobject_store = lu_dog.exhume_z_object_store(id).unwrap();
                let zobject_store = zobject_store.read().unwrap();
                let domain_name = &zobject_store.domain;

                write!(f, "{}Store", domain_name.to_upper_camel_case())
            }
        }
    }
}
