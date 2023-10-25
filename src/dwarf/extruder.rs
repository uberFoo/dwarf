use std::{env, fs, ops::Range, path::PathBuf};

use ansi_term::Colour;
use heck::ToUpperCamelCase;
use log;
use rustc_hash::FxHashMap as HashMap;
use sarzak::domain::DomainBuilder;
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        expression::{addition, and, expr_as, static_method_call},
        parse_dwarf, AttributeMap, BlockType, DwarfInteger, EnumField,
        Expression as ParserExpression, Generics, InnerAttribute, InnerItem, Item,
        PrintableValueType, Spanned, Statement as ParserStatement, Type, WrappedValueType,
    },
    keywords::{CHACHA, FN_NEW, FORMAT, JOIN, LEN, UUID_TYPE},
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{
            AWait, Block, Body, BooleanOperator, Call, DataStructure, EnumField as LuDogEnumField,
            EnumFieldEnum, Enumeration, Expression, ExpressionEnum, ExpressionStatement,
            ExternalImplementation, Field, FieldExpression, ForLoop, Function, Generic,
            ImplementationBlock, Index, IntegerLiteral, Item as WoogItem, ItemStatement, Lambda,
            LambdaParameter, LetStatement, Literal, LocalVariable, NamedFieldExpression, Parameter,
            PathElement, Pattern as AssocPat, Plugin, RangeExpression, Span as LuDogSpan,
            Statement, StringLiteral, StructExpression, StructField, StructGeneric, TupleField,
            Unit, ValueType, ValueTypeEnum, Variable, VariableExpression, WoogStruct, XFuture, XIf,
            XMatch, XPath, XPrint, XValue, XValueEnum, ZObjectStore,
        },
        Argument, Binary, BooleanLiteral, Comparison, DwarfSourceFile, FieldAccess,
        FieldAccessTarget, FloatLiteral, List, ListElement, ListExpression, MethodCall, Operator,
        ResultStatement, Unary, VariableEnum, XReturn,
    },
    new_ref, s_read, s_write,
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
    Context as InterContext, Dirty, ModelStore, NewRef, RefType,
};

const LIB_TAO: &str = "lib.tao";
const SRC_DIR: &str = "src";
const MODEL_DIR: &str = "models";
const TAO_EXT: &str = "tao";
const EXTENSION_DIR: &str = "extensions";
const JSON_EXT: &str = "json";

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

macro_rules! link_generic {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_generic(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_struct_generic {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_struct_generic(&last).unwrap().clone();
            let mut last = s_write!(last);
            last.next = Some(next.id);
        }

        Some(next.id)
    }};
}

macro_rules! link_ƛ_parameter {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_lambda_parameter(&last).unwrap().clone();
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

pub(crate) use link_argument;

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
pub(crate) use function;

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
pub(crate) use debug;

macro_rules! e_warn {
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
pub(crate) use e_warn;

macro_rules! error {
    ($($arg:tt)*) => {
        log::error!(
            target: "extruder",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Red.underline().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        )
    };
}
pub(crate) use error;

type Span = Range<usize>;
pub(super) type ExprSpan = (RefType<Expression>, RefType<LuDogSpan>);

// These below are just to avoid cloning things.
struct ConveyFunc<'a> {
    a_sink: &'a BlockType,
    name: &'a str,
    attributes: &'a AttributeMap,
    span: &'a Span,
    params: &'a [(Spanned<String>, Spanned<Type>)],
    return_type: &'a Spanned<Type>,
    statements: Option<&'a Spanned<ParserExpression>>,
}

impl<'a> ConveyFunc<'a> {
    fn new(
        a_sink: &'a BlockType,
        name: &'a str,
        attributes: &'a AttributeMap,
        span: &'a Span,
        params: &'a [(Spanned<String>, Spanned<Type>)],
        return_type: &'a Spanned<Type>,
        statements: Option<&'a Spanned<ParserExpression>>,
    ) -> Self {
        Self {
            a_sink,
            name,
            attributes,
            span,
            params,
            return_type,
            statements,
        }
    }
}

struct ConveyStruct<'a> {
    name: String,
    span: &'a Span,
    attributes: &'a AttributeMap,
    fields: &'a [(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: HashMap<String, Type>,
}

impl<'a> ConveyStruct<'a> {
    fn new(
        name: String,
        span: &'a Span,
        attributes: &'a AttributeMap,
        fields: &'a [(Spanned<String>, Spanned<Type>, AttributeMap)],
        generics: HashMap<String, Type>,
    ) -> Self {
        Self {
            name,
            span,
            attributes,
            fields,
            generics,
        }
    }
}

struct ConveyEnum<'a> {
    name: &'a str,
    attributes: &'a AttributeMap,
    fields: &'a [(Spanned<String>, Option<EnumField>)],
    generics: HashMap<String, Type>,
}

impl<'a> ConveyEnum<'a> {
    fn new(
        name: &'a str,
        attributes: &'a AttributeMap,
        fields: &'a [(Spanned<String>, Option<EnumField>)],
        generics: HashMap<String, Type>,
    ) -> Self {
        Self {
            name,
            attributes,
            fields,
            generics,
        }
    }
}

struct ConveyImpl<'a> {
    name: &'a str,
    span: &'a Span,
    attributes: &'a AttributeMap,
    funcs: &'a [Item],
}

impl<'a> ConveyImpl<'a> {
    fn new(name: &'a str, span: &'a Span, attributes: &'a AttributeMap, funcs: &'a [Item]) -> Self {
        Self {
            name,
            span,
            attributes,
            funcs,
        }
    }
}

#[derive(Debug)]
pub struct StructFields {
    woog_struct: RefType<WoogStruct>,
    fields: Vec<(Spanned<String>, Spanned<Type>, AttributeMap)>,
    generics: HashMap<String, Type>,

    location: Location,
}

pub struct Context<'a> {
    /// Location of the current item
    ///
    /// We keep this here because we sometimes report errors far away from where
    /// they occur.
    pub location: Location,
    /// Struct Field Storage
    ///
    /// As we inter structs we store their fields here until they are all read
    /// in. At that point we can typecheck them since any valid references will
    /// have also been read in.
    pub struct_fields: Vec<StructFields>,
    pub source: RefType<DwarfSourceFile>,
    pub models: &'a mut ModelStore,
    pub sarzak: &'a SarzakStore,
    pub dwarf_home: &'a PathBuf,
    pub cwd: PathBuf,
    pub dirty: &'a mut Vec<Dirty>,
    pub file_name: &'a str,
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
pub fn new_lu_dog<'a>(
    file_name: String,
    source: Option<(String, &[Item])>,
    dwarf_home: &PathBuf,
    sarzak: &SarzakStore,
) -> Result<InterContext> {
    let mut lu_dog = LuDogStore::new();

    // We need to stuff all of the sarzak types into the store.
    ValueType::new_ty(&Ty::new_boolean(sarzak), &mut lu_dog);
    ValueType::new_ty(&Ty::new_float(sarzak), &mut lu_dog);
    ValueType::new_ty(&Ty::new_integer(sarzak), &mut lu_dog);
    ValueType::new_ty(&Ty::new_s_string(sarzak), &mut lu_dog);
    ValueType::new_ty(&Ty::new_s_uuid(sarzak), &mut lu_dog);

    let mut models = HashMap::default();
    let mut dirty = Vec::new();

    if let Some((source, ast)) = source {
        let mut context = Context {
            location: location!(),
            struct_fields: Vec::new(),
            source: DwarfSourceFile::new(source, &mut lu_dog),
            models: &mut models,
            sarzak,
            dwarf_home,
            cwd: env::current_dir().unwrap(),
            dirty: &mut dirty,
            file_name: file_name.as_str(),
        };

        walk_tree(ast, &mut context, &mut lu_dog)?;
    }

    Ok(InterContext {
        source: file_name.into(),
        lu_dog: new_ref!(LuDogStore, lu_dog),
        models,
        dirty,
    })
}

fn walk_tree(ast: &[Item], context: &mut Context, lu_dog: &mut LuDogStore) -> Result<()> {
    let mut funcs = Vec::new();
    let mut implementations = Vec::new();
    let mut structs = Vec::new();
    let mut enums = Vec::new();

    // We need the structs before the impls, so we do this.
    for item in ast {
        match item {
            Item {
                item: (InnerItem::Enum((name, _), fields, generics), _),
                attributes,
            } => enums.push(ConveyEnum::new(
                name,
                attributes,
                fields,
                generics
                    .0
                    .clone()
                    .into_iter()
                    .map(|(t, _)| match t {
                        Type::Generic((t, s)) => (t.to_owned(), Type::Generic((t, s))),
                        _ => unreachable!(),
                    })
                    .collect(),
            )),
            Item {
                item:
                    (InnerItem::Function(a_sink, (name, _name_span), params, return_type, stmts), span),
                attributes,
            } => funcs.push(ConveyFunc::new(
                a_sink,
                name,
                attributes,
                span,
                params,
                return_type,
                stmts.as_ref(),
            )),
            Item {
                item: (InnerItem::Implementation((name, _name_span), funcs), span),
                attributes,
            } => implementations.push(ConveyImpl::new(name, span, attributes, funcs)),
            // Imports can happen any time, I think.
            Item {
                item: (InnerItem::Import((path, _path_span), alias), _span),
                attributes: _,
            } => inter_import(path, alias, context, lu_dog)?,
            Item {
                item: (InnerItem::Module((name, _name_span)), _span),
                attributes: _,
            } => inter_module(name, context, lu_dog)?,
            Item {
                item: (InnerItem::Struct((name, _), fields, generics), span),
                attributes,
            } => {
                // let name = if !generics.0.is_empty() {
                //     let mut name = name.to_owned();
                //     name.push_str("<");
                //     for (i, (n, _)) in generics.0.iter().enumerate() {
                //         if i > 0 {
                //             name.push_str(", ");
                //         }
                //         match n {
                //             Type::Generic((t, _)) => name.push_str(t),
                //             _ => unreachable!(),
                //         }
                //     }
                //     name.push_str(">");
                //     name
                // } else {
                //     name.to_owned()
                // };

                structs.push(ConveyStruct::new(
                    name.to_owned(),
                    span,
                    attributes,
                    fields,
                    generics
                        .0
                        .clone()
                        .into_iter()
                        .map(|(t, _)| match t {
                            Type::Generic((t, s)) => (t.to_owned(), Type::Generic((t, s))),
                            fubared => {
                                dbg!(fubared);
                                unreachable!();
                            }
                        })
                        .collect(),
                ))
            }
        }
    }

    let mut errors = Vec::new();
    // Put the type information in first.
    // This first pass over the structs just records the name, but not the fields.
    // We wait until we've seen all of the structs to do that. This allows us to
    // at least have the type name of any fields that use the types being interred.
    for ConveyStruct {
        name,
        span,
        attributes,
        fields,
        generics,
    } in &structs
    {
        debug!("Interring struct `{}` fields", name);
        let _ = inter_struct(name, span, attributes, fields, generics, context, lu_dog).map_err(
            |mut e| {
                errors.append(&mut e);
            },
        );
    }
    // Same exercise for enums.
    for ConveyEnum {
        name,
        attributes,
        fields,
        generics,
    } in &enums
    {
        debug!("Interring enum `{}` fields", name);
        let _ = inter_enum(name, attributes, fields, generics, context, lu_dog).map_err(|mut e| {
            errors.append(&mut e);
        });
    }

    for _ in &structs {
        let params = context.struct_fields.drain(..).collect::<Vec<_>>();
        for StructFields {
            woog_struct,
            fields,
            generics,
            location,
        } in params
        {
            let _ = inter_struct_fields(woog_struct, &fields, &generics, location, context, lu_dog)
                .map_err(|mut e| {
                    errors.append(&mut e);
                });
        }
    }
    // Using the type information, and the input, inter the implementation blocks.
    for ConveyImpl {
        name,
        span,
        attributes,
        funcs,
    } in implementations
    {
        debug!("Interring implementation `{}`", name);
        let _ = inter_implementation(name, attributes, funcs, span, context, lu_dog).map_err(
            |mut e| {
                errors.append(&mut e);
            },
        );
    }

    // Finally, inter the loose functions.
    for ConveyFunc {
        a_sink,
        name,
        attributes,
        span,
        params,
        return_type,
        statements,
    } in funcs
    {
        debug!("Interring function `{}`", name);
        let _ = inter_func(
            a_sink,
            name,
            attributes,
            params,
            return_type,
            statements,
            None,
            None,
            span,
            context,
            lu_dog,
        )
        .map_err(|mut e| errors.append(&mut e));
    }

    // Now we remove the generics from the store.
    // for ConveyStruct {
    //     name,
    //     span: _,
    //     attributes,
    //     fields,
    //     generics,
    // } in &structs
    // {
    //     debug!("Interring struct `{}`", name);
    //     let _ = exorcise_generic_struct(name, attributes, fields, generics, context, lu_dog)
    //         .map_err(|mut e| {
    //             errors.append(&mut e);
    //         });
    // }

    // for ConveyEnum {
    //     name,
    //     attributes,
    //     fields,
    //     generics,
    // } in &enums
    // {
    //     debug!("Interring enum `{}`", name);
    //     let _ = exorcise_generic_enum(name, attributes, fields, generics, context, lu_dog).map_err(
    //         |mut e| {
    //             errors.append(&mut e);
    //         },
    //     );
    // }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[allow(clippy::too_many_arguments)]
fn inter_func(
    a_sink: &BlockType,
    name: &str,
    attributes: &AttributeMap,
    params: &[(Spanned<String>, Spanned<Type>)],
    return_type: &Spanned<Type>,
    stmts: Option<&Spanned<ParserExpression>>,
    impl_block: Option<&RefType<ImplementationBlock>>,
    impl_ty: Option<&RefType<ValueType>>,
    span: &Span,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_func {}", name);

    let a_sink = match a_sink {
        BlockType::Async => true,
        BlockType::Sync => false,
    };

    let external = if let Some(proxy_vec) = attributes.get(PROXY) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
            debug!("proxy");

            if let Some(store_vec) = attributes.get(STORE) {
                if let Some((_, ref value)) = store_vec.get(0) {
                    let store_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.store.: {store_name}");

                    if let Some(func_vec) = attributes.get(FUNC) {
                        if let Some((_, ref value)) = func_vec.get(0) {
                            let func_name: String = value.try_into().map_err(|e| vec![e])?;
                            debug!("proxy.func: {func_name}");

                            if let Some(obj_vec) = attributes.get("object") {
                                if let Some((_, ref value)) = obj_vec.get(0) {
                                    let obj_name: String = value.try_into().map_err(|e| vec![e])?;
                                    debug!("proxy.object: {obj_name}");

                                    let external = ExternalImplementation::new(
                                        func_name, store_name, obj_name, lu_dog,
                                    );
                                    Some(Body::new_external_implementation(
                                        a_sink, &external, lu_dog,
                                    ))
                                } else {
                                    unreachable!();
                                }
                            } else {
                                return Err(vec![DwarfError::Generic {
                                    description: "No model specified".to_owned(),
                                }]);
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        return Err(vec![DwarfError::Generic {
                            description: "No model specified".to_owned(),
                        }]);
                    }
                } else {
                    unreachable!();
                }
            } else {
                return Err(vec![DwarfError::Generic {
                    description: "No store specified".to_owned(),
                }]);
            }
        } else {
            unreachable!();
        }
    } else {
        None
    };

    context.location = location!();
    let ret_ty = make_value_type(&return_type.0, &return_type.1, impl_ty, context, lu_dog)?;

    let name = name.de_sanitize();
    let (func, block) =
        if let Some((ParserExpression::Block(block_a_sink, stmts, vars, tys), span)) = &stmts {
            let a_sink = a_sink
                || match block_a_sink {
                    BlockType::Async => true,
                    BlockType::Sync => false,
                };

            let block = Block::new(a_sink, Uuid::new_v4(), None, None, lu_dog);
            for (var, ty) in vars.iter().zip(tys.iter()) {
                let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                let var = Variable::new_local_variable(var.to_owned(), &local, lu_dog);
                let _value = XValue::new_variable(&block, &ty.0, &var, lu_dog);
                // 🚧 We should really be passing a span in the Block so that
                // we can link this XValue to it.
            }

            let body = Body::new_block(a_sink, &block, lu_dog);
            let func = Function::new(name.to_owned(), &body, None, impl_block, &ret_ty, lu_dog);
            context.dirty.push(Dirty::Func(func.clone()));
            let _ = ValueType::new_function(&func, lu_dog);

            (func, Some((block, stmts, span)))
        } else if let Some(body) = external {
            (
                Function::new(name.to_owned(), &body, None, impl_block, &ret_ty, lu_dog),
                None,
            )
        } else {
            return Err(vec![DwarfError::Generic {
                description: "No body specified".to_owned(),
            }]);
        };

    let _ = WoogItem::new_function(&context.source, &func, lu_dog);
    // Create a type for our function
    let ty = ValueType::new_function(&func, lu_dog);
    LuDogSpan::new(
        span.end as i64,
        span.start as i64,
        &context.source,
        Some(&ty),
        None,
        lu_dog,
    );

    // Check the parameters
    //
    let mut errors = Vec::new();
    let mut last_param_uuid: Option<usize> = None;
    let mut position = 0;
    for ((param_name, name_span), (param_ty, ty_span)) in params {
        debug!("param name {}", param_name);
        debug!("param ty {}", param_ty);

        // We need to introduce the values into the block, so that we don't
        // error out when parsing the statements.
        //
        context.location = location!();
        let param_ty = match make_value_type(param_ty, ty_span, impl_ty, context, lu_dog) {
            Ok(ty) => ty,
            Err(mut e) => {
                errors.append(&mut e);
                continue;
            }
        };
        LuDogSpan::new(
            ty_span.end as i64,
            ty_span.start as i64,
            &context.source,
            Some(&param_ty),
            None,
            lu_dog,
        );

        debug!("param_ty {:?}", param_ty);

        let param = Parameter::new(position, &func, None, &param_ty, lu_dog);

        if position == 0 {
            s_write!(func).first_param = Some(s_read!(param).id);
        }
        position += 1;

        debug!("param {:?}", param);
        debug!("param_ty {:?}", param_ty);

        let var = Variable::new_parameter(param_name.to_owned(), &param, lu_dog);
        debug!("var {:?}", var);

        if let Some((ref block, _, _)) = block {
            let value = XValue::new_variable(block, &param_ty, &var, lu_dog);
            LuDogSpan::new(
                name_span.end as i64,
                name_span.start as i64,
                &context.source,
                None,
                Some(&value),
                lu_dog,
            );
        }

        last_param_uuid = link_parameter!(last_param_uuid, param, lu_dog);
    }

    // Note that we don't do anything if we didn't create a block, and that we
    // don't create a block if we don't have any statements.
    if let Some((block, stmts, stmt_span)) = block {
        let stmts: Vec<RefType<ParserStatement>> = stmts
            .iter()
            .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
            .collect();

        let (block_ty, block_span) = inter_statements(&stmts, stmt_span, &block, context, lu_dog)?;

        let block_ty = match a_sink {
            true => {
                let future = XFuture::new(&block_ty, lu_dog);
                ValueType::new_x_future(&future, lu_dog)
            }
            false => block_ty,
        };

        typecheck(
            (&ret_ty, span),
            (&block_ty, &block_span),
            location!(),
            context,
            lu_dog,
        )?;
    }

    debug!("func `{name}` saved");

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn inter_statement(
    stmt: &RefType<ParserStatement>,
    index: i64,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(Spanned<RefType<Statement>>, RefType<ValueType>)> {
    debug!("inter_statement {:?}", stmt);

    match &*s_read!(stmt) {
        //
        // Expression
        //
        ParserStatement::Expression((expr, span)) => {
            let (expr, _) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                span,
                block,
                context,
                lu_dog,
            )?;
            let stmt = ExpressionStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_expression_statement(index, block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Item
        //
        ParserStatement::Item(item) => {
            let span = match item {
                // Item::Function((name, _name_span), params, return_type, stmts) => {
                Item {
                    item:
                        (
                            InnerItem::Function(
                                ref a_sink,
                                ref name,
                                ref params,
                                ref return_type,
                                ref stmts,
                            ),
                            span,
                        ),
                    attributes,
                } => {
                    inter_func(
                        a_sink,
                        &name.0,
                        attributes,
                        params,
                        return_type,
                        stmts.as_ref(),
                        None,
                        None,
                        span,
                        context,
                        lu_dog,
                    )?;
                    span
                }
                Item {
                    item: (InnerItem::Implementation((name, _name_span), funcs), span),
                    attributes,
                } => {
                    inter_implementation(name, attributes, funcs, span, context, lu_dog)?;
                    span
                }
                Item {
                    item: (InnerItem::Import((path, _path_span), alias), span),
                    attributes: _,
                } => {
                    inter_import(path, alias, context, lu_dog)?;
                    span
                }
                Item {
                    item: (InnerItem::Struct((name, _span), fields, generics), outer_span),
                    attributes,
                } => {
                    inter_struct(
                        name,
                        outer_span,
                        attributes,
                        fields,
                        &generics
                            .0
                            .clone()
                            .into_iter()
                            .map(|(t, _)| match t {
                                Type::Generic((t, s)) => (t.to_owned(), Type::Generic((t, s))),
                                _ => unreachable!(),
                            })
                            .collect(),
                        context,
                        lu_dog,
                    )
                    .and_then(|_| {
                        // There had better be one and only one.
                        let StructFields {
                            woog_struct,
                            fields,
                            generics,
                            location,
                        } = context.struct_fields.pop().unwrap();
                        inter_struct_fields(
                            woog_struct,
                            &fields,
                            &generics,
                            location,
                            context,
                            lu_dog,
                        )
                    })?;
                    outer_span
                }
                _ => unimplemented!(),
            };
            let _stmt = ItemStatement::new();
            let stmt = Statement::new_item_statement(index, block, None, lu_dog);
            Ok(((stmt, span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Let
        //
        ParserStatement::Let((var_name, var_span), type_, (expr, expr_span)) => {
            // Setup the local variable that is the LHS of the statement.
            let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
            let var = Variable::new_local_variable(var_name.to_owned(), &local, lu_dog);

            debug!("inter let {var:?}");

            // Now parse the RHS, which is an expression.
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                expr_span,
                block,
                context,
                lu_dog,
            )?;

            debug!("inter let expr {expr:?}, ty {ty:?}");

            let ty = if let Some((type_, span)) = type_ {
                let lhs_ty = type_.into_value_type(
                    context.file_name,
                    span,
                    lu_dog,
                    context.models,
                    context.sarzak,
                )?;
                typecheck(
                    (&lhs_ty, span),
                    (&ty, expr_span),
                    location!(),
                    context,
                    lu_dog,
                )?;
                lhs_ty
            } else {
                ty
            };

            // 🚧
            // Let's keep an eye on this. I've had the notion of having a separate
            // entry point for the REPL, and conditionally needing to generate an
            // error would support the idea.
            if let ValueTypeEnum::Unknown(_) = s_read!(ty).subtype {
                e_warn!("Unknown type for variable {}", var_name);
            }

            // Create a variable, now that we have a type from the expression.
            let value = XValue::new_variable(block, &ty, &var, lu_dog);
            LuDogSpan::new(
                var_span.end as i64,
                var_span.start as i64,
                &context.source,
                None,
                Some(&value),
                lu_dog,
            );

            // Setup the let statement itself.
            let stmt = LetStatement::new(&expr.0, &local, lu_dog);
            let stmt = Statement::new_let_statement(index, block, None, &stmt, lu_dog);

            Ok(((stmt, expr_span.to_owned()), ValueType::new_empty(lu_dog)))
        }
        //
        // Result
        //
        ParserStatement::Result((ref expr, span)) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                span,
                block,
                context,
                lu_dog,
            )?;
            let stmt = ResultStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_result_statement(index, block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ty))
        }
        道 => todo!("{:?}", 道),
    }
}

fn inter_statements(
    statements: &[RefType<ParserStatement>],
    span: &Span,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<Spanned<RefType<ValueType>>> {
    let mut value_type = ValueType::new_empty(lu_dog);
    let mut span = span.to_owned();
    let mut errors = Vec::new();

    let mut last_stmt_uuid: Option<usize> = None;
    let mut index = 0;
    for stmt in statements {
        let (stmt, ty) = match inter_statement(stmt, index, block, context, lu_dog) {
            Ok((stmt, ty)) => (stmt, ty),
            Err(err) => {
                errors.extend(err);
                continue;
            }
        };

        index += 1;

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

pub(super) fn inter_expression(
    expr: &RefType<ParserExpression>,
    span: &Span,
    block: &RefType<Block>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("expr {expr:?}, span {span:?}");

    let span = LuDogSpan::new(
        span.end as i64,
        span.start as i64,
        &context.source,
        None,
        None,
        lu_dog,
    );

    let expr = s_read!(expr).clone();
    match expr {
        ParserExpression::Addition(ref lhs_p, ref rhs_p) => {
            addition::inter(lhs_p, rhs_p, span, block, context, lu_dog)
        }
        ParserExpression::And(ref lhs_p, ref rhs_p) => {
            and::inter(lhs_p, rhs_p, span, block, context, lu_dog)
        }
        ParserExpression::As(ref expr, ref ty) => {
            expr_as::inter(expr, ty, span, block, context, lu_dog)
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
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Binary::new_assignment(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Await
        //
        ParserExpression::Await(ref expr_p) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr_p.0.to_owned()),
                &expr_p.1,
                block,
                context,
                lu_dog,
            )?;

            if !matches!(s_read!(ty).subtype, ValueTypeEnum::XFuture(_)) {
                dbg!(ty);
                Err(vec![DwarfError::AwaitNotFuture {
                    file: context.file_name.to_owned(),
                    span: expr_p.1.clone(),
                }])
            } else {
                let future = match s_read!(ty).subtype {
                    ValueTypeEnum::XFuture(ref id) => lu_dog.exhume_x_future(id).unwrap(),
                    _ => unreachable!(),
                };
                let ty = s_read!(future).r2_value_type(lu_dog)[0].clone();
                let expr = AWait::new(&expr.0, lu_dog);
                let expr = Expression::new_a_wait(&expr, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                Ok(((expr, span), ty))
            }
            // }
        }
        //
        // Bang
        //
        ParserExpression::Bang(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                &expr.1,
                block,
                context,
                lu_dog,
            )?;
            let not = Unary::new_not(lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &not, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Block
        //
        ParserExpression::Block(a_sink, ref stmts, vars, tys) => {
            let sync = match a_sink {
                BlockType::Async => false,
                BlockType::Sync => true,
            };
            let block = Block::new(!sync, Uuid::new_v4(), Some(block), None, lu_dog);

            for (var, ty) in vars.into_iter().zip(tys.into_iter()) {
                let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                let var = Variable::new_local_variable(var, &local, lu_dog);
                let _value = XValue::new_variable(&block, &ty.0, &var, lu_dog);
                // 🚧 We should really be passing a span in the Block so that
                // we can link this XValue to it.
            }

            // let block = create_block::<P>(None, lu_dog)?;
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
            let ty = inter_statements(&stmts_vec, &stmts_span, &block, context, lu_dog)?;
            let value = XValue::new_expression(&block, &ty.0, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            // If it's an async block then wrap it in a future.
            let ty = match a_sink {
                BlockType::Async => {
                    let span = ty.1;
                    let future = XFuture::new(&ty.0, lu_dog);
                    (ValueType::new_x_future(&future, lu_dog), span)
                }
                BlockType::Sync => ty,
            };

            debug!("block {expr:?}");
            Ok(((expr, span), ty.0))
        }
        //
        // BooleanLiteral
        //
        ParserExpression::BooleanLiteral(literal) => {
            let literal = if literal {
                BooleanLiteral::new_true_literal(lu_dog)
            } else {
                BooleanLiteral::new_false_literal(lu_dog)
            };
            let expr =
                Expression::new_literal(&Literal::new_boolean_literal(&literal, lu_dog), lu_dog);
            let ty = ValueType::new_ty(&Ty::new_boolean(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Debug
        //
        ParserExpression::Debug => {
            let expr = Expression::new_debugger(lu_dog);
            let ty = ValueType::new_empty(lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Division
        //
        ParserExpression::Division(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We also need to check that the type supports division.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Binary::new_division(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Empty
        //
        ParserExpression::Empty => {
            let expr = Expression::new_empty_expression(lu_dog);
            let ty = ValueType::new_empty(lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Equals
        //
        ParserExpression::Equals(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

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
                &lhs.1,
                block,
                context,
                lu_dog,
            )?;

            let id = s_read!(lhs_ty).id;
            let ty = lu_dog.exhume_value_type(&id).unwrap();
            let ty_read = s_read!(ty);

            match &ty_read.subtype {
                // We matched on the lhs type.
                ValueTypeEnum::Function(ref _id) => Ok((lhs, ty.clone())),
                ValueTypeEnum::WoogStruct(ref id) => {
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                    let fields = s_read!(woog_struct).r7_field(lu_dog);
                    let field = fields.iter().find(|f| s_read!(f).name == rhs.0);

                    if let Some(field) = field {
                        let field = lu_dog.exhume_field(&s_read!(field).id);
                        let func = if let Some(impl_) =
                            s_read!(woog_struct).r8c_implementation_block(lu_dog).pop()
                        {
                            let impl_ = s_read!(impl_);

                            let funcs = impl_.r9_function(lu_dog);
                            funcs.iter().find(|f| s_read!(f).name == rhs.0).cloned()
                        } else {
                            None
                        };

                        debug!("field {:?}", field);
                        debug!("func {:?}", func);

                        // We need to grab the type from the field: what we have above is the type
                        // of the struct.
                        if let Some(field) = field {
                            let fat = FieldAccessTarget::new_field(&field, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(&expr, lu_dog);
                            let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            update_span_value(&span, &value, location!());

                            Ok(((expr, span), ty))
                        } else if let Some(func) = func {
                            let fat = FieldAccessTarget::new_function(&func, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(&expr, lu_dog);
                            let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            update_span_value(&span, &value, location!());

                            Ok(((expr, span), ty))
                        } else {
                            let span = s_read!(span);
                            let span = span.start as usize..span.end as usize;
                            Err(vec![DwarfError::StructFieldNotFound {
                                field: rhs.0.clone(),
                                file: context.file_name.to_owned(),
                                span,
                                location: location!(),
                            }])
                        }
                    } else {
                        Err(vec![DwarfError::StructFieldNotFound {
                            field: rhs.0.clone(),
                            file: context.file_name.to_owned(),
                            span: rhs.1.to_owned(),
                            location: location!(),
                        }])
                    }
                }
                ValueTypeEnum::Ty(id) => {
                    debug!("FieldAccess: ValueTypeEnum::Ty() {:?}", id);
                    let ty = context.sarzak.exhume_ty(id).unwrap();
                    let ty = ty.read().unwrap();
                    match *ty {
                        Ty::Object(id) => {
                            // We get here for objects imported from a plug-in.
                            let woog_struct = lu_dog
                                .iter_woog_struct()
                                .inspect(|ref ws| {
                                    debug!("{ws:?}");
                                })
                                .find(|ws| s_read!(ws).object == Some(id))
                                .unwrap();

                            if let Some(field) = lu_dog.exhume_field_id_by_name(&rhs.0) {
                                let field = lu_dog.exhume_field(&field);
                                // let field = woog_struct.r7_field(lu_dog);
                                // let field = field.iter().find(|f| s_read!(f).name == rhs);
                                let func = if let Some(impl_) =
                                    s_read!(woog_struct).r8c_implementation_block(lu_dog).pop()
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
                                    update_span_value(&span, &value, location!());

                                    Ok(((expr, span), ty))
                                } else if let Some(func) = func {
                                    let fat = FieldAccessTarget::new_function(&func, lu_dog);
                                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                                    let expr = Expression::new_field_access(&expr, lu_dog);
                                    let ty = s_read!(func).r10_value_type(lu_dog)[0].clone();
                                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                                    update_span_value(&span, &value, location!());

                                    Ok(((expr, span), ty))
                                } else {
                                    let span = s_read!(span);
                                    let span = span.start as usize..span.end as usize;
                                    Err(vec![DwarfError::StructFieldNotFound {
                                        field: rhs.0.clone(),
                                        file: context.file_name.to_owned(),
                                        span,
                                        location: location!(),
                                    }])
                                }
                            } else {
                                Err(vec![DwarfError::StructFieldNotFound {
                                    field: rhs.0.clone(),
                                    file: context.file_name.to_owned(),
                                    span: rhs.1.to_owned(),
                                    location: location!(),
                                }])
                            }
                        }
                        _ => {
                            debug!("returning lhs");
                            Ok((lhs, lhs_ty))
                        }
                    }
                }
                _ => Err(vec![DwarfError::NotAStruct {
                    file: context.file_name.to_owned(),
                    span: rhs.1.to_owned(),
                    ty: PrintableValueType(&ty, context, lu_dog).to_string(),
                }]),
            }
        }
        //
        // FloatLiteral
        //
        ParserExpression::FloatLiteral(literal) => {
            let expr = Expression::new_literal(
                &Literal::new_float_literal(&FloatLiteral::new(literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(&Ty::new_float(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // For Loop
        //
        ParserExpression::For(iter, collection, body) => {
            debug!("For");

            let cspan = &collection.1;
            let collection = new_ref!(ParserExpression, collection.0.clone());

            let (collection, collection_ty) =
                inter_expression(&collection, cspan, block, context, lu_dog)?;

            let collection_ty = match s_read!(collection_ty).subtype {
                ValueTypeEnum::List(ref id) => {
                    let list = lu_dog.exhume_list(id).unwrap();
                    let list = s_read!(list);
                    list.r36_value_type(lu_dog)[0].clone()
                }
                ValueTypeEnum::Range(_) => {
                    // 🚧  I'm punting here. I think range can be something other than an int.
                    // For example, what if you wanted a..f? I need to think about this, and
                    // check what rust does. I'm actually too tired right now to think about
                    // it. Related to range_type_bug.
                    ValueType::new_ty(&Ty::new_integer(context.sarzak), lu_dog)
                }
                ValueTypeEnum::Ty(ref id) => {
                    let ty = context.sarzak.exhume_ty(id).unwrap();
                    let ty = ty.read().unwrap();
                    match &*ty {
                        Ty::SString(_) => ValueType::new_char(lu_dog),
                        _ => {
                            let ty =
                                PrintableValueType(&collection_ty, context, lu_dog).to_string();
                            return Err(vec![DwarfError::NotAList {
                                file: context.file_name.to_owned(),
                                span: cspan.to_owned(),
                                ty,
                                location: location!(),
                            }]);
                        }
                    }
                }
                _ => {
                    let ty = PrintableValueType(&collection_ty, context, lu_dog).to_string();
                    return Err(vec![DwarfError::NotAList {
                        file: context.file_name.to_owned(),
                        span: cspan.to_owned(),
                        ty,
                        location: location!(),
                    }]);
                }
            };

            let bspan = &body.1;
            let body = match &body.0 {
                ParserExpression::Block(a_sink, body, vars, tys)
                    if vars.is_empty() && tys.is_empty() =>
                {
                    ParserExpression::Block(
                        a_sink.to_owned(),
                        body.to_owned(),
                        vec![iter.0.to_owned()],
                        vec![WrappedValueType(collection_ty)],
                    )
                }
                _ => unreachable!(),
            };
            let body = new_ref!(ParserExpression, body.to_owned());

            let (body, _body_ty) = inter_expression(&body, bspan, block, context, lu_dog)?;

            // 🚧 This is dumb. I'm extracting the body here, just to stick it back
            // into an expression in the interpreter. The model will need to be fixed
            // so that the for loop takes an expression and not a body, which I think
            // is sensible.
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
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // FunctionCall
        //
        ParserExpression::FunctionCall(func, args) => {
            debug!("func {:?}", func);
            let fspan = &func.1;
            let func = &func.0;
            debug!("args {:?}", args);

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
                fspan,
                block,
                context,
                lu_dog,
            )?;

            if let ValueTypeEnum::Unknown(_) = s_read!(ret_ty).subtype {
                // Here's where we need to lookup the function definition.
                error!("🚧 we need a function definition");
            }

            let func_call = Call::new_function_call(true, None, Some(&func_expr.0), lu_dog);
            let func = Expression::new_call(&func_call, lu_dog);
            let value = XValue::new_expression(block, &ret_ty, &func, lu_dog);
            update_span_value(&span, &value, location!());

            let mut last_arg_uuid: Option<usize> = None;
            // Note that position makes each arg unique. I don't remember if
            // that is the explicit intention or not.
            for (position, arg) in args.iter().enumerate() {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, arg.0.to_owned()),
                    &arg.1,
                    block,
                    context,
                    lu_dog,
                )?;
                let arg = Argument::new(
                    position as DwarfInteger,
                    &arg_expr.0,
                    &func_call,
                    None,
                    lu_dog,
                );

                if position == 0 {
                    s_write!(func_call).argument = Some(s_read!(arg).id);
                }

                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "ParserExpression::FunctionCall exit {:?}",
                (&func_call, s_read!(func_call).r28_argument(lu_dog))
            );

            debug!(
                "return type {}",
                PrintableValueType(&ret_ty, context, lu_dog).to_string()
            );

            Ok(((func, span), ret_ty))
        }
        //
        // GreaterThan: >
        //
        ParserExpression::GreaterThan(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_greater_than(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // GreaterThanOrEqual: >=
        //
        ParserExpression::GreaterThanOrEqual(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_greater_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Group
        //
        ParserExpression::Group(ref expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                &expr.1,
                block,
                context,
                lu_dog,
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
            let (conditional, conditional_ty) =
                inter_expression(&conditional, cspan, block, context, lu_dog)?;
            debug!("ParserExpression::If {:?}", conditional_ty);

            // Check that the conditional expression evaluates to a boolean.
            // Note that this first check is necessary to unwrap the sarzak type
            // from the lu_dog type.
            if let ValueTypeEnum::Ty(ref ty) = s_read!(conditional_ty).subtype {
                let s_ty = context.sarzak.exhume_ty(ty).unwrap();
                let s_ty = s_ty.read().unwrap();
                if let Ty::Boolean(_) = &*s_ty {
                    // Good Times.
                } else {
                    let bty = ValueType::new_ty(&Ty::new_boolean(context.sarzak), lu_dog);
                    let bty = PrintableValueType(&bty, context, lu_dog);
                    let ty = PrintableValueType(&conditional_ty, context, lu_dog);
                    return Err(vec![DwarfError::TypeMismatch {
                        expected: bty.to_string(),
                        found: ty.to_string(),
                        file: context.file_name.to_owned(),
                        expected_span: cspan.to_owned(),
                        found_span: cspan.to_owned(),
                        location: location!(),
                    }]);
                }
            } else {
                let bty = ValueType::new_ty(&Ty::new_boolean(context.sarzak), lu_dog);
                let bty = PrintableValueType(&bty, context, lu_dog);
                let ty = PrintableValueType(&conditional_ty, context, lu_dog);
                return Err(vec![DwarfError::TypeMismatch {
                    expected: bty.to_string(),
                    found: ty.to_string(),
                    file: context.file_name.to_owned(),
                    expected_span: cspan.to_owned(),
                    found_span: cspan.to_owned(),
                    location: location!(),
                }]);
            }

            let tspan = &true_block.1;
            let true_block = new_ref!(ParserExpression, true_block.0.to_owned());
            let (true_block, true_ty) =
                inter_expression(&true_block, tspan, block, context, lu_dog)?;
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
                let (false_block, _false_ty) =
                    inter_expression(&false_block, fspan, block, context, lu_dog)?;
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
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Index
        //
        ParserExpression::Index(target_p, index_p) => {
            debug!("index {target_p:?}, {index_p:?}");
            let (target, target_ty) = inter_expression(
                &new_ref!(ParserExpression, target_p.0.to_owned()),
                &target_p.1,
                block,
                context,
                lu_dog,
            )?;
            debug!("target: {target:?}, ty: {target_ty:?}");
            let (index, index_ty) = inter_expression(
                &new_ref!(ParserExpression, index_p.0.to_owned()),
                &index_p.1,
                block,
                context,
                lu_dog,
            )?;

            let int_ty = ValueType::new_ty(&Ty::new_integer(context.sarzak), lu_dog);

            let index_span = s_read!(index.1).start as usize..s_read!(index.1).end as usize;
            typecheck(
                (&int_ty, &index_span),
                (&index_ty, &index_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            // We need to dereference the list and return the underlying type.
            let target_ty = if let ValueTypeEnum::List(ref ty) = s_read!(target_ty).subtype {
                let list = lu_dog.exhume_list(ty).unwrap();
                let ty = &s_read!(list).r36_value_type(lu_dog)[0];
                ty.clone()
            } else if let ValueTypeEnum::Ty(ref ty) = s_read!(target_ty).subtype {
                let ty = context.sarzak.exhume_ty(ty).unwrap();
                let ty = ty.read().unwrap();
                if let Ty::SString(_) = &*ty {
                    ValueType::new_char(lu_dog)
                } else {
                    let ty = PrintableValueType(&target_ty, context, lu_dog).to_string();
                    return Err(vec![DwarfError::NotAList {
                        file: context.file_name.to_owned(),
                        span: target_p.1.clone(),
                        ty,
                        location: location!(),
                    }]);
                }
            } else {
                let ty = PrintableValueType(&target_ty, context, lu_dog).to_string();
                return Err(vec![DwarfError::NotAList {
                    file: context.file_name.to_owned(),
                    span: target_p.1.clone(),
                    ty,
                    location: location!(),
                }]);
            };

            let index = Index::new(&index.0, &target.0, lu_dog);

            let expr = Expression::new_index(&index, lu_dog);
            let value = XValue::new_expression(block, &target_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), target_ty))
        }
        //
        // IntegerLiteral
        //
        ParserExpression::IntegerLiteral(literal) => {
            let expr = Expression::new_literal(
                &Literal::new_integer_literal(&IntegerLiteral::new(literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(&Ty::new_integer(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Lambda
        //
        ParserExpression::Lambda(a_sink, params, return_type, body) => {
            let (stmts, a_sink) = if let ParserExpression::Block(_, body, _, _) = &body.0 {
                let a_sink = match a_sink {
                    BlockType::Async => true,
                    BlockType::Sync => false,
                };
                (body, a_sink)
            } else {
                unreachable!();
            };
            let block = Block::new(a_sink, Uuid::new_v4(), Some(block), None, lu_dog);
            let _body = Body::new_block(a_sink, &block, lu_dog);

            context.location = location!();
            let ret_ty = make_value_type(&return_type.0, &return_type.1, None, context, lu_dog)?;

            let lambda = Lambda::new(Some(&_body), &ret_ty, lu_dog);
            let _ = ValueType::new_lambda(&lambda, lu_dog);

            let mut errors = Vec::new();
            let mut last_param_uuid: Option<usize> = None;
            for (position, ((param_name, name_span), (param_ty, param_span))) in
                params.iter().enumerate()
            {
                debug!("param name {}", param_name);
                debug!("param ty {}", param_ty);

                let param =
                    LambdaParameter::new(position as DwarfInteger, &lambda, None, None, lu_dog);

                debug!("param {:?}", param);

                let var = Variable::new_lambda_parameter(param_name.to_owned(), &param, lu_dog);
                debug!("var {:?}", var);
                // We need to introduce the values into the block, so that we don't
                // error out when parsing the statements.
                //
                context.location = location!();
                let param_ty = match make_value_type(param_ty, param_span, None, context, lu_dog) {
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
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                last_param_uuid = link_ƛ_parameter!(last_param_uuid, param, lu_dog);
            }

            let stmts: Vec<RefType<ParserStatement>> = stmts
                .iter()
                .map(|stmt| new_ref!(ParserStatement, stmt.0.clone()))
                .collect();

            let (block_ty, block_span) =
                inter_statements(&stmts, &body.1, &block, context, lu_dog)?;

            typecheck(
                (&ret_ty, &return_type.1),
                (&block_ty, &block_span),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Expression::new_lambda(&lambda, lu_dog);
            let value = XValue::new_expression(&block, &ret_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ret_ty))
        }
        //
        // LessThan: <
        //
        ParserExpression::LessThan(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_less_than(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // LessThanOrEqual
        //
        ParserExpression::LessThanOrEqual(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same,
            // 🚧 or at least compatible. Need to look into rust rules.
            // 🚧 We also need to check that the types implement PartialEq, and whatever else...
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_less_than_or_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

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
                update_span_value(&span, &value, location!());

                Ok(((expr, span), ty))
            } else {
                let mut elements = elements.iter();
                // I'm going to get the type of the first element, and then check
                // that each subsequent element is the same type.
                let element = elements.next().unwrap();
                let span1 = &element.1;
                let ((first, first_span), first_ty) = inter_expression(
                    &new_ref!(ParserExpression, element.0.to_owned()),
                    &element.1,
                    block,
                    context,
                    lu_dog,
                )?;

                let list = List::new(&first_ty, lu_dog);
                let element = ListElement::new(0, &first, None, lu_dog);
                let expr = Expression::new_list_element(&element, lu_dog);
                let value = XValue::new_expression(block, &first_ty, &expr, lu_dog);
                s_write!(first_span).x_value = Some(s_read!(value).id);
                let list_expr = ListExpression::new(Some(&element), lu_dog);

                let mut last_element_uuid: Option<usize> = Some(s_read!(element).id);
                let mut position = 1;
                for element in elements {
                    let ((elt, elt_span), elt_ty) = inter_expression(
                        &new_ref!(ParserExpression, element.0.to_owned()),
                        &element.1,
                        block,
                        context,
                        lu_dog,
                    )?;

                    typecheck(
                        (&first_ty, span1),
                        (&elt_ty, &element.1),
                        location!(),
                        context,
                        lu_dog,
                    )?;

                    let element = ListElement::new(position, &elt, None, lu_dog);
                    position += 1;

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
        ParserExpression::LocalVariable(ref name) => {
            // We need to return an expression and a type.
            debug!("local variable name {}", name);
            // We look for a value in the current block. We need to clone them
            // to be able to modify lu_dog below.
            //
            // So, multiple let statements will result in multiple values. We only
            // need one -- and it needs to be the right one...
            // To expound, there are likely to be multiple values in this block,
            // and we need to find the one that matches the variable name.

            // Blocks may be nested, so we collect all of the values up the chain.
            let mut values = Vec::new();

            let mut parent = Some(block.clone());
            while let Some(block) = parent {
                let mut foo = s_read!(block).r33_x_value(lu_dog);
                values.append(&mut foo);
                parent = s_read!(block).r93_block(lu_dog).pop();
            }

            debug!("values: {values:?}");
            // Now search for a value that's a Variable, and see if the access matches
            // the variable.
            let mut expr_type_tuples = values
                .iter()
                .filter_map(|value| {
                    let value = s_read!(value);
                    match value.subtype {
                        XValueEnum::Expression(ref _expr) => {
                            // What's going on is that there are a bunch of values in the block.
                            // We are iterating over them all, and we are bound to find some that
                            // aren't variable expressions even though we are parsing a LocalVariable.
                            // Remember these are all of the values -- not just the ones that have
                            // something to do with finding ourselves here.

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
                                        let ty = value.r24_value_type(lu_dog)[0].clone();

                                        let lhs_ty =
                                            PrintableValueType(&ty, context, lu_dog);

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
                                        update_span_value(&span, &value, location!());

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
            // "yes", which I guess is correct, but the type is `()`. So that's weird.
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
            } else if let Some(ref id) = lu_dog.exhume_function_id_by_name(name) {
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

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                debug!("LocalVariable result ({expr:#?}, {ty:#?})");
                Ok(((expr, span), ty))
            } else {
                debug!("variable not found");
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(&expr, lu_dog);
                let ty = ValueType::new_unknown(lu_dog);
                e_warn!("Unknown type for variable {name}");

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                Ok(((expr, span), ty))
            }
        }
        //
        // Match
        //
        // The idea here is to take the pattern and see if it matches the scrutinee.
        // Of course here we are just setting things up for the interpreter. We
        // have a `Match` type in the model that we need to populate. It however
        // only contains a string field. So the trick is to compile something into
        // that string that can later be consumed by the interpreter.  The scrutinee
        // is an expression, so it's storage should be easy. In rust it can be
        // a place or value expression, i.e., lvalue, rvalue. In the former rust
        // uses a temporary variable when processing a match.
        //
        // Let's start with a path pattern. Something like Option::None. In this
        // case we just need to check the value of the scrutinee and see if it
        // matches. So we need to turn that into something that we can evaluate.
        // For this code:
        // ```
        // if let Option::None = x { none(); }
        // ```
        // we would generate this dwarf:
        // ```
        // match x {
        //     Option::None => { none(); }
        //     _ => {}
        // }
        // ```
        // Which generates what? What can be interpreted?
        // ```
        // if x == Option::None { none(); }
        // ```
        //
        // I forget that we need to do
        // the typechecking here as well. That implies that we need to turn the
        // pattern into a type.
        //
        // So let's look at another example.
        // `if let Option::Some(y) = x { some(y); }`
        // In this case we need a test and then we need to create a variable and assign
        // the value of the match.
        // ```
        //  match x {
        //     Option::Some(y) => { some(y); }
        //     _ => {}
        // }
        // ```
        // This one seems more difficult.
        // ```
        // if chacha::typeof(x) == Option::Some { let y = x; some(y); }
        // ```
        //
        // 🚧 We need to check all of the types of the patterns and ensure that
        // they are compatible.
        ParserExpression::Match(scrutinee, patterns) => {
            debug!("scrutinee: {scrutinee:?}");

            let s_span = &scrutinee.1;
            let (scrutinee, scrutinee_ty) = inter_expression(
                &new_ref!(ParserExpression, scrutinee.0.to_owned()),
                &scrutinee.1,
                block,
                context,
                lu_dog,
            )?;

            let xmatch = XMatch::new(Uuid::new_v4(), &scrutinee.0, lu_dog);

            let mut first = true;
            let mut match_ty = ValueType::new_unknown(lu_dog);
            for ((pattern, match_expr), ref span) in patterns {
                debug!("pattern: {pattern:?}");
                let pattern_expr: ParserExpression = pattern.to_owned().into();
                let (pattern_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, pattern_expr),
                    span,
                    block,
                    context,
                    lu_dog,
                )?;

                typecheck(
                    (&scrutinee_ty, s_span),
                    (&ty, span),
                    location!(),
                    context,
                    lu_dog,
                )?;

                let (expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, match_expr.to_owned()),
                    span,
                    block,
                    context,
                    lu_dog,
                )?;

                if first {
                    first = false;
                    match_ty = ty;
                } else {
                    typecheck((&match_ty, span), (&ty, span), location!(), context, lu_dog)?;
                }

                let _pat = AssocPat::new(&expr.0, &pattern_expr.0, &xmatch, lu_dog);
            }

            let expr = Expression::new_x_match(&xmatch, lu_dog);

            let value = XValue::new_expression(block, &match_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), match_ty))
        }
        //
        // MethodCall
        //
        ParserExpression::MethodCall(instance, (ref method, meth_span), args) => {
            debug!("MethodCall Enter: instance: {instance:?}, method: `{method}`");

            let (instance, instance_ty) = inter_expression(
                &new_ref!(ParserExpression, instance.0.to_owned()),
                &instance.1,
                block,
                context,
                lu_dog,
            )?;

            debug!("MethodCall instance: {instance:?}, type: {instance_ty:?}");

            let ret_ty = match s_read!(instance_ty).subtype {
                ValueTypeEnum::WoogStruct(id) => {
                    let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
                    let x = lookup_woog_struct_method_return_type(
                        &s_read!(woog_struct).name,
                        method,
                        context.sarzak,
                        lu_dog,
                    );

                    #[allow(clippy::let_and_return)]
                    x
                }
                ValueTypeEnum::Ty(id) => {
                    let ty = context.sarzak.exhume_ty(&id).unwrap();
                    let ty = ty.read().unwrap();
                    if let Ty::SString(_) = &*ty {
                        match method.as_str() {
                            LEN => {
                                let ty = Ty::new_integer(context.sarzak);
                                ValueType::new_ty(&ty, lu_dog)
                            }
                            FORMAT => {
                                let ty = Ty::new_s_string(context.sarzak);
                                ValueType::new_ty(&ty, lu_dog)
                            }
                            _ => {
                                return Err(vec![DwarfError::NoSuchMethod {
                                    method: method.to_owned(),
                                    file: context.file_name.to_owned(),
                                    span: meth_span.to_owned(),
                                    location: location!(),
                                }])
                            }
                        }
                    } else {
                        ValueType::new_unknown(lu_dog)
                    }
                }
                ValueTypeEnum::XFuture(_) => match method.as_str() {
                    JOIN => ValueType::new_unknown(lu_dog),
                    _ => {
                        return Err(vec![DwarfError::NoSuchMethod {
                            method: method.to_owned(),
                            file: context.file_name.to_owned(),
                            span: meth_span.to_owned(),
                            location: location!(),
                        }])
                    }
                },
                _ => ValueType::new_unknown(lu_dog),
            };

            let meth = MethodCall::new(method.to_owned(), lu_dog);
            let call = Call::new_method_call(true, None, Some(&instance.0), &meth, lu_dog);
            let expr = Expression::new_call(&call, lu_dog);

            let value = XValue::new_expression(block, &instance_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            let mut last_arg_uuid: Option<usize> = None;

            // Self
            // This is the self parameter
            // Self -- I can never seem to find this.
            let this = Argument::new(0, &instance.0, &call, None, lu_dog);
            last_arg_uuid = link_argument!(last_arg_uuid, this, lu_dog);
            s_write!(call).argument = Some(s_read!(this).id);

            // Note the position.
            let mut position = 1;
            for arg in args {
                let (arg_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, arg.0.to_owned()),
                    &arg.1,
                    block,
                    context,
                    lu_dog,
                )?;
                let value = XValue::new_expression(block, &ty, &arg_expr.0, lu_dog);
                let _span = LuDogSpan::new(
                    arg.1.end as i64,
                    arg.1.start as i64,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );
                let arg = Argument::new(position, &arg_expr.0, &call, None, lu_dog);
                position += 1;

                last_arg_uuid = link_argument!(last_arg_uuid, arg, lu_dog);
            }

            debug!(
                "{} return type {}",
                Colour::Red.dimmed().italic().paint("MethodCall"),
                PrintableValueType(&ret_ty, context, lu_dog).to_string()
            );

            Ok(((expr, span), ret_ty))
        }
        //
        // Negation
        //
        ParserExpression::Negation(expr) => {
            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                &expr.1,
                block,
                context,
                lu_dog,
            )?;
            let negation = Unary::new_negation(lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &negation, lu_dog);
            let expr = Expression::new_operator(&operator, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // NotEquals
        //
        ParserExpression::NotEquals(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_not_equal(lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(&ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Print
        //
        ParserExpression::Print(expr) => {
            let (expr, _ty) = inter_expression(
                &new_ref!(ParserExpression, expr.0.to_owned()),
                &expr.1,
                block,
                context,
                lu_dog,
            )?;
            let ty = ValueType::new_empty(lu_dog);
            let print = XPrint::new(&expr.0, lu_dog);
            let expr = Expression::new_x_print(&print, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Multiplication
        //
        ParserExpression::Multiplication(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We also need to check that the type supports multiplication.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Binary::new_multiplication(lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Or
        //
        ParserExpression::Or(ref lhs_p, ref rhs_p) => {
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs_p.0.to_owned()),
                &lhs_p.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                lu_dog,
            )?;

            if let ValueTypeEnum::Ty(ref id) = s_read!(lhs_ty).subtype {
                let ty = context.sarzak.exhume_ty(id).unwrap();
                matches!(&*ty.read().unwrap(), Ty::Boolean(_));
            } else {
                let lhs = PrintableValueType(&lhs_ty, context, lu_dog);
                return Err(vec![DwarfError::TypeMismatch {
                    found: lhs.to_string(),
                    expected: "bool".to_string(),
                    file: context.file_name.to_owned(),
                    found_span: lhs_p.1.to_owned(),
                    expected_span: rhs_p.1.to_owned(),
                    location: location!(),
                }]);
            }

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = BooleanOperator::new_or(lu_dog);
            let expr = Binary::new_boolean_operator(&expr, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(&expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Path In Expression
        //
        // This looks like it's comping up as an enum constructor, not sure if
        // it will come up with structs. Seems like it would/could.
        // ParserExpression::PathInExpression(path) => {
        //     debug!("PathInExpression {:?}", path);

        //     // Lookup the type
        //     let root = path.first().unwrap();
        //     if let Type::UserType((root, span)) = root {
        //         if let Some(root) = lu_dog.exhume_enumeration_id_by_name(root) {
        //             // I'll be stupid here and assume that the next element is the last.
        //             let last = path.last().unwrap();
        //         } else {
        //             Err(vec![DwarfError::EnumNotFound {
        //                 name: root.to_owned(),
        //                 span: span.to_owned(),
        //             }])
        //         }
        //     } else {
        //         panic!("Things fell apart -- entropy happens.");
        //     }
        // }
        //
        // Path In Expression
        //
        // This looks like it's comping up as an enum constructor, not sure if
        // it will come up with structs. Seems like it would/could.
        // ParserExpression::PathInExpression(path) => {
        //     debug!("PathInExpression {:?}", path);

        //     // Lookup the type
        //     let root = path.first().unwrap();
        //     if let Type::UserType((root, span)) = root {
        //         if let Some(root) = lu_dog.exhume_enumeration_id_by_name(root) {
        //             // I'll be stupid here and assume that the next element is the last.
        //             let last = path.last().unwrap();
        //         } else {
        //             Err(vec![DwarfError::EnumNotFound {
        //                 name: root.to_owned(),
        //                 span: span.to_owned(),
        //             }])
        //         }
        //     } else {
        //         panic!("Things fell apart -- entropy happens.");
        //     }
        // }
        //
        // Unit enumeration
        //
        ParserExpression::UnitEnum(enum_path, (field_name, field_span)) => {
            debug!("UnitEnum {:?}, Field {field_name}", enum_path);
            let (path, path_span) =
                if let (ParserExpression::PathInExpression(path), span) = enum_path.as_ref() {
                    (path, span)
                } else {
                    panic!(
                    "I don't think that we should ever see anything other than a path here: {:?}",
                    enum_path
                );
                };

            debug!("path {path:?}");

            // 🚧 Looks like we are validating each element of the path, but int, in Option::<int> isn't a
            // UserType, so I'm confused.
            // I think that if you look, it's actually coming in as Option<int>, which can be a UserType.
            let full_enum_name = path.iter().map(|p| {
                if let Type::UserType((obj, _), generics) = p {
                    let mut name = obj.de_sanitize().to_owned();
                    let generics = generics.iter().map(|g| {
                        g.0.to_string()
                    }).collect::<Vec<_>>().join(", ");
                    if !generics.is_empty() {
                        name.push_str("<");
                        name.push_str(&generics);
                        name.push_str(">");
                    }
                    name
                } else {
                    panic!("I don't think that we should ever see anything other than a user type here: {:?}", p);
                }
            }).collect::<Vec<_>>().join("");

            let enum_root = if let Some(root) = full_enum_name.split('<').next() {
                root
            } else {
                full_enum_name.as_str()
            };

            debug!("enum_name {:?}", full_enum_name);
            // dbg!(&enum_root, &full_enum_name, &enum_path);
            let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
            let mut elts = path
                .iter()
                .inspect(|ty| {
                    debug!("ty {:?}", ty);
                })
                .map(|ty| {
                    if let Type::UserType((name, _), _generics) = ty {
                        PathElement::new(name.to_owned(), None, &x_path, lu_dog)
                    } else {
                        unreachable!()
                    }
                })
                .collect::<Vec<RefType<PathElement>>>();
            elts.push(PathElement::new(
                field_name.to_owned(),
                None,
                &x_path,
                lu_dog,
            ));

            let first = Some(elts[0].clone());
            let _last = elts
                .into_iter()
                .fold(Option::<RefType<PathElement>>::None, |prev, elt| {
                    if let Some(prev) = prev {
                        let elt = s_read!(elt);
                        s_write!(prev).next = Some(elt.id);
                    }
                    Some(elt)
                });

            if let Some(first) = first {
                let first = s_read!(first).id;
                s_write!(x_path).first = Some(first);
            }

            if let Some(woog_enum_id) = lu_dog.exhume_enumeration_id_by_name(enum_root) {
                let woog_enum_id = if enum_root != full_enum_name {
                    if let Some(id) = lu_dog.exhume_enumeration_id_by_name(&full_enum_name) {
                        id
                    } else {
                        let (new_enum, _) =
                            create_generic_enum(&full_enum_name, &enum_path.0, lu_dog);
                        let x = s_read!(new_enum).id;
                        x
                    }
                } else {
                    woog_enum_id
                };

                let woog_enum = lu_dog.exhume_enumeration(&woog_enum_id).unwrap();

                let data_struct = DataStructure::new_enumeration(&woog_enum, lu_dog);

                let foo = s_read!(woog_enum).r88_enum_field(lu_dog);
                let field = foo.iter().find(|field| {
                    let field = s_read!(field);
                    field.name == field_name
                });

                if let Some(_field) = field {
                    let ty = lu_dog
                        .iter_value_type()
                        .inspect(|ty| {
                            debug!("ty {:?}", ty);
                        })
                        .find(|ty| {
                            if let ValueTypeEnum::Enumeration(id) = s_read!(ty).subtype {
                                id == woog_enum_id
                            } else {
                                false
                            }
                        })
                        .unwrap();

                    // let expr = Expression::new_enum_field(field, lu_dog);
                    let struct_expr =
                        StructExpression::new(Uuid::new_v4(), &data_struct, &x_path, lu_dog);
                    let expr = Expression::new_struct_expression(&struct_expr, lu_dog);
                    debug!("expression {expr:?}");

                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                    update_span_value(&span, &value, location!());

                    Ok(((expr, span), ty))
                } else {
                    Err(vec![DwarfError::NoSuchField {
                        name: full_enum_name.to_owned(),
                        name_span: path_span.to_owned(),
                        field: field_name.to_owned(),
                        file: context.file_name.to_owned(),
                        span: field_span.to_owned(),
                    }])
                }
            } else {
                Err(vec![DwarfError::EnumNotFound {
                    name: full_enum_name.to_owned(),
                    file: context.file_name.to_owned(),
                    span: path_span.to_owned(),
                }])
            }
        }
        //
        // Range
        //
        ParserExpression::Range(start, end) => {
            let (start, start_ty) = inter_expression(
                &new_ref!(ParserExpression, start.0.to_owned()),
                &start.1,
                block,
                context,
                lu_dog,
            )?;
            let (end, _end_ty) = inter_expression(
                &new_ref!(ParserExpression, end.0.to_owned()),
                &end.1,
                block,
                context,
                lu_dog,
            )?;

            // 🚧 Typecheck the start and end types to make sure that they are
            // both ints.

            let range = RangeExpression::new_full(Some(&start.0), Some(&end.0), lu_dog);

            let expr = Expression::new_range_expression(&range, lu_dog);
            let value = XValue::new_expression(block, &start_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ValueType::new_range(lu_dog)))
        }
        //
        // Return
        //
        ParserExpression::Return(expr) => {
            let (expr, ty) = if let Some(expr) = expr {
                inter_expression(
                    &new_ref!(ParserExpression, expr.0.to_owned()),
                    &expr.1,
                    block,
                    context,
                    lu_dog,
                )?
            } else {
                // 🚧 Once we have tuples, I'd prefer to return the empty tuple.
                let expr = Expression::new_block(
                    &Block::new(false, Uuid::new_v4(), None, None, lu_dog),
                    lu_dog,
                );
                let ty = ValueType::new_empty(lu_dog);
                let value = XValue::new_expression(&block, &ty, &expr, lu_dog);
                // See # Span Bug
                lu_dog.inter_span(|id| {
                    let mut span = s_read!(span).clone();
                    span.x_value = Some(s_read!(value).id);
                    span.id = id;
                    new_ref!(LuDogSpan, span)
                });

                // update_span_value(&span, &value, location!());

                ((expr, span.clone()), ty)
            };

            let ret = XReturn::new(&expr.0, lu_dog);
            let expr = Expression::new_x_return(&ret, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // StaticMethodCall
        //
        ParserExpression::StaticMethodCall(ref path, (ref method, _), ref params) => {
            static_method_call::inter(path, method, span, params, block, context, lu_dog)
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
            let ty = ValueType::new_ty(&Ty::new_s_string(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Struct
        //
        ParserExpression::Struct(name, fields) => {
            let save_name = &name.0;
            let name_span = &name.1;
            let (root, name) = if let ParserExpression::LocalVariable(obj) = &name.0 {
                (obj.to_owned(), obj.to_owned())
            } else if let ParserExpression::PathInExpression(types) = &name.0 {
                let mut root = String::new();
                let mut name = String::new();

                for (i, ty) in types.iter().enumerate() {
                    if let Type::UserType((type_name, _), generics) = ty {
                        name.extend([type_name.as_str()]);
                        if i != types.len() - 1 {
                            name.extend(["::"]);
                        }
                        if i == types.len() - 1 {
                            root = name.clone();
                        }
                        if !generics.is_empty() {
                            name.push_str("<");
                            for (i, (generic, _)) in generics.iter().enumerate() {
                                // if let Type::Generic((generic_name, _)) = generic {
                                name.extend([generic.to_string()]);

                                if i != generics.len() - 1 {
                                    name.extend([", "]);
                                }
                                // }
                            }
                            name.push_str(">");
                        }
                    } else {
                        return Err(vec![DwarfError::Internal {
                            description: format!(
                                "Expected a user type in struct expression, found {:?}",
                                ty
                            ),
                            location: location!(),
                        }]);
                    }
                }

                (root, name)
            } else {
                return Err(vec![DwarfError::Internal {
                    description: format!(
                        "Expected a local variable in struct expression, found {:?}",
                        name.0
                    ),
                    location: location!(),
                }]);
            };

            debug!("ParserExpression::Struct {}", name);

            // Here we don't de_sanitize the name, and we are looking it up in the
            // dwarf model.
            let id = match lu_dog.exhume_woog_struct_id_by_name(&root) {
                Some(id) => id,
                None => {
                    return Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        file: context.file_name.to_owned(),
                        span: name_span.to_owned(),
                        location: location!(),
                    }]);
                }
            };

            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
            let struct_fields = s_read!(woog_struct).r7_field(lu_dog);

            let data_struct = DataStructure::new_woog_struct(&woog_struct, lu_dog);

            // 🚧 Fix this.
            let x_path = XPath::new(Uuid::new_v4(), None, lu_dog);
            let struct_expr = StructExpression::new(Uuid::new_v4(), &data_struct, &x_path, lu_dog);

            let mut generic_substitutions = HashMap::default();

            for (field_name, field_expr) in fields {
                let field_expr_span = field_expr.1.to_owned();
                let (field_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, field_expr.0.to_owned()),
                    &field_expr_span,
                    block,
                    context,
                    lu_dog,
                )?;

                let pvt = PrintableValueType(&ty, context, lu_dog).to_string();

                debug!("field `{name:?}` is of type `{pvt}`, expr: {field_expr:?}");

                if let Some(field) = struct_fields
                    .iter()
                    .find(|f| s_read!(f).name == field_name.0)
                {
                    let field_ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();
                    let naked_ty = s_read!(field_ty);

                    // dbg!(&field_ty);

                    // Primarily we are here to check the type of the field against
                    // the type of the expression. If only it were so easily done.
                    // The issue is generics. If the field is generic, then we need
                    // to initially use the type of the expression to fill in the
                    // type of the generic parameter. *After* that however, we
                    // need to use this new type to typecheck all subsequent uses
                    // of the pattern.
                    if let ValueTypeEnum::Generic(ref id) = naked_ty.subtype {
                        // OK. We are instantiating a generic. We need to create the new type
                        let generic = lu_dog.exhume_generic(id).unwrap();
                        generic_substitutions.insert(s_read!(generic).name.to_owned(), ty.clone());
                    } else {
                        // We only need the type check if the type of the field in the struct
                        // is not generic. Otherwise we are explicitly defining the type above.
                        //
                        // This is about to get complicated. If the field is generic,
                        // then we need to a) fill in the type value of the generic,
                        // based on either i) the type of a previous field, or ii)
                        // the type of this field.
                        typecheck(
                            (&field_ty, &field_name.1),
                            (&ty, &field_expr_span),
                            location!(),
                            context,
                            lu_dog,
                        )?;
                    }
                } else {
                    return Err(vec![DwarfError::NoSuchField {
                        name: name.to_string(),
                        name_span: name_span.to_owned(),
                        field: field_name.0.to_owned(),
                        file: context.file_name.to_owned(),
                        span: field_name.1.to_owned(),
                    }]);
                }

                let nfe = NamedFieldExpression::new(field_name.0.to_owned(), lu_dog);
                let field = FieldExpression::new_named_field_expression(
                    &field_expr.0,
                    &struct_expr,
                    &nfe,
                    lu_dog,
                );

                let expr = Expression::new_field_expression(&field, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                // # Span Bug
                // This is exceptional, at least so for. What's happening is that
                // the span is already pointing at a value. We've been clobbering
                // it successfully until the following case:
                // ```
                // // Does not work
                // Foo { bar: Bar::new()}
                // // Works
                // Foo { bar: Uuid::new()}
                // ```
                // So what do we do? Well I tried not overwriting, and lot's of
                // stuff broke. And really, it's two different values pointing at
                // the same span. So I just cloned it and inserted it.
                //
                // The thing that bothers me is where else might this be happening?
                if s_read!(field_expr.1).x_value.is_none() {
                    s_write!(field_expr.1).x_value = Some(s_read!(value).id);
                } else {
                    lu_dog.inter_span(|id| {
                        let mut span = s_read!(field_expr.1).clone();
                        span.x_value = Some(s_read!(value).id);
                        span.id = id;
                        new_ref!(LuDogSpan, span)
                    });
                }
            }

            if !generic_substitutions.is_empty() {
                create_generic_struct(
                    &woog_struct,
                    &generic_substitutions,
                    context,
                    context.sarzak,
                    lu_dog,
                );
            }

            // I love that the type of the thing is the same as the thing itself.
            let expr = Expression::new_struct_expression(&struct_expr, lu_dog);
            let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Subtraction
        //
        ParserExpression::Subtraction(ref lhs, ref rhs) => {
            debug!("Subtraction");
            let (lhs, lhs_ty) = inter_expression(
                &new_ref!(ParserExpression, lhs.0.to_owned()),
                &lhs.1,
                block,
                context,
                lu_dog,
            )?;
            let (rhs, _rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                &rhs.1,
                block,
                context,
                lu_dog,
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
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        道 => {
            let source = &s_read!(context.source).source;
            let span = s_read!(span).start as usize..s_read!(span).end as usize;
            Err(vec![DwarfError::NoImplementation {
                missing: format!("inter_expression: {:?}", 道),
                code: source[span.clone()].to_owned(),
                file: context.file_name.to_owned(),
                span,
            }])
        }
    }
}

fn inter_module(name: &str, context: &mut Context, lu_dog: &mut LuDogStore) -> Result<()> {
    debug!("inter_module: {name}");

    let mut errors = Vec::new();

    let mut path = context.cwd.clone();
    path.push("sacrifice");
    path.set_file_name(name);
    path.set_extension(TAO_EXT);

    match fs::read_to_string(&path) {
        Ok(source_code) => {
            // parse, and extrude the dwarf file
            match parse_dwarf(path.to_str().unwrap(), &source_code) {
                Ok(ast) => {
                    // let old_cwd = context.cwd.clone();
                    // context.cwd = path.clone();
                    // Extrusion time
                    walk_tree(&ast, context, lu_dog)?;
                    // context.cwd = old_cwd;
                }
                Err(_) => {
                    return Ok(());
                }
            }
        }
        Err(e) => {
            errors.push(DwarfError::File {
                description: "Attempting to open import".to_owned(),
                path: path.to_owned(),
                source: e,
                location: location!(),
            });
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn inter_import(
    path: &[Spanned<String>],
    _alias: &Option<(String, Range<usize>)>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    let mut errors = Vec::new();

    let path_root = path.iter().map(|p| p.0.to_owned()).collect::<Vec<_>>();
    let module = path_root.get(0).unwrap(); // This will have _something_.

    if module == "dwarf" {
        return Ok(());
    }
    // path_root.pop().expect("Path root not found");
    // let path_root = path_root.join("/");
    // let obj_name = path.last().unwrap();
    // let (has_alias, alias) = if let Some((alias, _)) = alias {
    //     (true, alias.to_owned())
    // } else {
    //     (false, "".to_owned())
    // };

    let mut path = context.dwarf_home.clone();
    path.push(EXTENSION_DIR);
    path.push(module);
    path.push(SRC_DIR);
    let dir = path.clone();

    path.push(LIB_TAO);

    match fs::read_to_string(&path) {
        Ok(source_code) => {
            // parse, and extrude the dwarf file
            match parse_dwarf(path.to_str().unwrap(), &source_code) {
                Ok(ast) => {
                    let old_cwd = context.cwd.clone();
                    context.cwd = dir;
                    // Extrusion time
                    walk_tree(&ast, context, lu_dog)?;
                    context.cwd = old_cwd;
                }
                Err(_) => {
                    return Ok(());
                }
            }
        }
        Err(e) => {
            errors.push(DwarfError::File {
                description: format!("Attempting to open import: {module}"),
                path: path.to_owned(),
                source: e,
                location: location!(),
            });
        }
    }

    // let import = Import::new(
    //     alias,
    //     has_alias,
    //     obj_name.0.to_owned(),
    //     path_root,
    //     None,
    //     lu_dog,
    // );
    // debug!("import {import:?}");

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn inter_implementation(
    name: &str,
    attributes: &AttributeMap,
    funcs: &[Item],
    span: &Span,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    let name = name.de_sanitize();
    debug!("inter_implementation: {name}");

    let (impl_ty, implementation) = if let Some(store_vec) = attributes.get(STORE) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = store_vec.get(0) {
            if let Some(model_vec) = attributes.get(MODEL) {
                if let Some((_, ref value)) = model_vec.get(0) {
                    let model_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("store.model: {model_name}");
                    let store = lu_dog
                        .iter_z_object_store()
                        .find(|store| s_read!(store).domain == model_name);
                    let (ty, store) = if let Some(store) = store {
                        let ty = lu_dog
                            .iter_value_type()
                            .find(|ty| {
                                let ty = s_read!(ty);
                                if let ValueTypeEnum::ZObjectStore(ref store_id) = ty.subtype {
                                    store_id == &s_read!(store).id
                                } else {
                                    false
                                }
                            })
                            .unwrap();
                        (ty, Some(store))
                    } else {
                        // 🚧 This is a hack to get the http client plugin working.
                        // I think that we need to use a different attribute name.
                        context.location = location!();
                        let ty = make_value_type(
                            &Type::UserType((name.to_owned(), 0..0), vec![]),
                            span,
                            None,
                            context,
                            lu_dog,
                        )?;
                        (ty, None)
                    };

                    let implementation = ImplementationBlock::new(None, store.as_ref(), lu_dog);
                    let _ = WoogItem::new_implementation_block(
                        &context.source,
                        &implementation,
                        lu_dog,
                    );

                    (Some(ty), Some(implementation))
                } else {
                    unreachable!();
                }
            } else {
                return Err(vec![DwarfError::Generic {
                    description: "No model specified".to_owned(),
                }]);
            }
        } else {
            unreachable!();
        }
    } else {
        context.location = location!();
        let impl_ty = make_value_type(
            &Type::UserType((name.to_owned(), 0..0), vec![]),
            span,
            None,
            context,
            lu_dog,
        )?;

        let id = if let Some(id) = lu_dog.exhume_woog_struct_id_by_name(name) {
            id
        } else {
            return Err(vec![DwarfError::ObjectNameNotFound {
                name: name.to_owned(),
                file: context.file_name.to_owned(),
                span: span.to_owned(),
                location: location!(),
            }]);
        };

        let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();

        let implementation = ImplementationBlock::new(Some(&woog_struct), None, lu_dog);
        let _ = WoogItem::new_implementation_block(&context.source, &implementation, lu_dog);

        (Some(impl_ty), Some(implementation))
    };

    let mut errors = Vec::new();

    debug!("inter_implementation {}", name);

    for func in funcs {
        match func {
            Item {
                item:
                    (
                        InnerItem::Function(
                            ref func_ty,
                            ref name,
                            ref params,
                            ref return_type,
                            ref stmts,
                        ),
                        span,
                    ),
                attributes,
            } => {
                match inter_func(
                    func_ty,
                    &name.0,
                    attributes,
                    params,
                    return_type,
                    stmts.as_ref(),
                    implementation.as_ref(),
                    impl_ty.as_ref(),
                    span,
                    context,
                    lu_dog,
                ) {
                    Ok(_) => (),
                    Err(mut err) => errors.append(&mut err),
                }
            }
            Item {
                item: (_, span),
                attributes: _,
            } => {
                return Err(vec![DwarfError::ImplementationBlock {
                    file: context.file_name.to_owned(),
                    span: span.clone(),
                }])
            }
        }
    }
    // }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[allow(dead_code)]
fn exorcise_generic_enum(
    name: &str,
    _attributes: &AttributeMap,
    _variants: &[(Spanned<String>, Option<EnumField>)],
    generics: &Option<Generics>,
    _context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("exorcise_generic_enum {name}");

    match generics {
        Some(_) => {
            debug!("erasing {}", &name);
            let woog_enum_id = lu_dog.exhume_enumeration_id_by_name(name).unwrap();
            let woog_enum = lu_dog.exhume_enumeration(&woog_enum_id).unwrap();
            for field in s_read!(woog_enum).r88_enum_field(lu_dog) {
                match s_read!(field).subtype {
                    EnumFieldEnum::Unit(ref _id) => {
                        // lu_dog.exorcise_unit(id);
                    }
                    EnumFieldEnum::StructField(ref _id) => {
                        // lu_dog.exorcise_struct_field(id);
                    }
                    EnumFieldEnum::TupleField(ref id) => {
                        let field = lu_dog.exhume_tuple_field(id).unwrap();
                        let ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();
                        let ty = s_read!(ty);
                        if let ValueTypeEnum::Generic(_) = ty.subtype {
                            // lu_dog.exorcise_tuple_field(id);
                        }
                    }
                }
                lu_dog.exorcise_enum_field(&s_read!(field).id);
            }

            lu_dog.exorcise_enumeration(&woog_enum_id);
        }
        None => {}
    };

    Ok(())
}

fn inter_enum(
    name: &str,
    _attributes: &AttributeMap,
    variants: &[(Spanned<String>, Option<EnumField>)],
    enum_generics: &HashMap<String, Type>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_enum {name}");

    let woog_enum = Enumeration::new(name.to_owned(), None, lu_dog);
    context.dirty.push(Dirty::Enum(woog_enum.clone()));
    let _ = ValueType::new_enumeration(&woog_enum, lu_dog);

    for (number, ((field_name, span), field)) in variants.iter().enumerate() {
        match field {
            Some(EnumField::Struct(ref fields)) => {
                // We create a struct in the store here so that it's available for construction
                // in the struct expression code.
                // Note that the name of the struct includes the name of the enum, with a path
                // separator. This is cheap. I really need to think about how paths and imports
                // and the like are going to work. The model will need some updating methinks.
                let woog_struct =
                    WoogStruct::new(format!("{}::{}", name, field_name), None, None, lu_dog);
                context.dirty.push(Dirty::Struct(woog_struct.clone()));
                let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);
                LuDogSpan::new(
                    span.end as i64,
                    span.start as i64,
                    &context.source,
                    Some(&ty),
                    None,
                    lu_dog,
                );

                for ((name, _), (ty, ty_span), attrs) in fields {
                    context.location = location!();
                    let ty = make_value_type(ty, ty_span, None, context, lu_dog)?;
                    let _ = Field::new(name.to_owned(), &woog_struct, &ty, lu_dog);
                }
                let field = StructField::new(field_name.to_owned(), lu_dog);
                LuDogEnumField::new_struct_field(field_name.to_owned(), &woog_enum, &field, lu_dog);
            }
            Some(EnumField::Tuple(type_)) => {
                let ty = match type_ {
                    (Type::UserType((_, span), generics), _outer_span) if !generics.is_empty() => {
                        let mut first = true;
                        let mut first_generic = ValueType::new_empty(lu_dog);
                        let mut last_generic_uuid: Option<usize> = None;
                        for generic in generics {
                            let (generic, span) =
                                if let Type::UserType((name, span), _) = &generic.0 {
                                    (name, span)
                                } else {
                                    unreachable!();
                                };
                            let generic = Generic::new(generic.to_owned(), None, None, lu_dog);
                            let ty = ValueType::new_generic(&generic, lu_dog);
                            let span = LuDogSpan::new(
                                span.end as i64,
                                span.start as i64,
                                &context.source,
                                Some(&ty),
                                None,
                                lu_dog,
                            );

                            if first {
                                first = false;
                                first_generic = ty.clone()
                            }
                            last_generic_uuid = link_generic!(last_generic_uuid, generic, lu_dog);
                        }

                        first_generic
                    }
                    (Type::UserType((ty, span), generics), _) if generics.is_empty() => {
                        // Pass the user type to the lookup business if this isn't a generic parameter.
                        if let Some(generic) = enum_generics.get(ty) {
                            context.location = location!();
                            let ty = make_value_type(&generic, span, None, context, lu_dog)?;
                            LuDogSpan::new(
                                span.end as i64,
                                span.start as i64,
                                &context.source,
                                Some(&ty),
                                None,
                                lu_dog,
                            );

                            ty
                        } else {
                            context.location = location!();
                            let ty = make_value_type(&type_.0, span, None, context, lu_dog)?;
                            LuDogSpan::new(
                                span.end as i64,
                                span.start as i64,
                                &context.source,
                                Some(&ty),
                                None,
                                lu_dog,
                            );

                            ty
                        }
                    }
                    _ => {
                        context.location = location!();
                        let ty = make_value_type(&type_.0, span, None, context, lu_dog)?;
                        LuDogSpan::new(
                            span.end as i64,
                            span.start as i64,
                            &context.source,
                            Some(&ty),
                            None,
                            lu_dog,
                        );

                        ty
                    }
                };

                let field = TupleField::new(Uuid::new_v4(), &ty, lu_dog);
                LuDogEnumField::new_tuple_field(field_name.to_owned(), &woog_enum, &field, lu_dog);
            }
            _ => {
                let unit = Unit::new(number as DwarfInteger, lu_dog);
                let _ = LuDogEnumField::new_unit(field_name.to_owned(), &woog_enum, &unit, lu_dog);
            }
        }
    }

    Ok(())
}

#[allow(dead_code)]
fn exorcise_generic_struct(
    name: &str,
    _attributes: &AttributeMap,
    _fields: &[(Spanned<String>, Spanned<Type>)],
    generics: &Option<Generics>,
    _context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("exorcise_generic_struct {name}");

    match generics {
        Some(_) => {
            debug!("erasing {}", &name);
            let woog_struct_id = lu_dog.exhume_woog_struct_id_by_name(name).unwrap();
            let woog_struct = lu_dog.exhume_woog_struct(&woog_struct_id).unwrap();
            for field in s_read!(woog_struct).r7_field(lu_dog) {
                lu_dog.exorcise_field(&s_read!(field).id);
            }

            lu_dog.exorcise_woog_struct(&woog_struct_id);
        }
        None => {}
    };
    Ok(())
}

const FUNC: &str = "func";
const MODEL: &str = "model";
const OBJECT: &str = "object";
const PROXY: &str = "proxy";
const PLUGIN: &str = "plugin";
const STORE: &str = "store";
const TYPE: &str = "ty";

fn inter_struct(
    name: &str,
    _span: &Span,
    attributes: &AttributeMap,
    fields: &[(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: &HashMap<String, Type>,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("struct {name}");

    // If there is a proxy attribute then we'll use it's info to attach an object
    // from the store to this UDT.
    if let Some(proxy_vec) = attributes.get(PROXY) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
            // Get the store value
            if let Some(store_vec) = attributes.get(STORE) {
                if let Some((_, ref value)) = store_vec.get(0) {
                    let store_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.store: {store_name}");

                    if let Some(name_vec) = attributes.get(OBJECT) {
                        if let Some((_, ref value)) = name_vec.get(0) {
                            let proxy: String = value.try_into().map_err(|e| vec![e])?;
                            let proxy = proxy.de_sanitize();
                            debug!("proxy.object: {proxy}");
                            if let Some(model) = context.models.get(&store_name) {
                                if let Some(ref obj_id) = model.0.exhume_object_id_by_name(proxy) {
                                    let obj = model.0.exhume_object(obj_id).unwrap();
                                    let woog_struct = WoogStruct::new(
                                        proxy.to_owned(),
                                        None,
                                        Some(&*obj.read().unwrap()),
                                        lu_dog,
                                    );
                                    context.dirty.push(Dirty::Struct(woog_struct.clone()));
                                    let _ = WoogItem::new_woog_struct(
                                        &context.source,
                                        &woog_struct,
                                        lu_dog,
                                    );
                                    let _ty = ValueType::new_woog_struct(&woog_struct, lu_dog);
                                    // 🚧 We may want to consider putting a span in the attribute map.
                                    // LuDogSpan::new(
                                    //     span.end as i64,
                                    //     span.start as i64,
                                    //     &context.source,
                                    //     Some(&ty),
                                    //     None,
                                    //     lu_dog,
                                    // );

                                    // We are pushing these onto a stack of fields so that we can typecheck
                                    // them after all of the structs have been interred.
                                    context.struct_fields.push(StructFields {
                                        woog_struct,
                                        fields: fields.to_owned(),
                                        generics: generics.clone(),
                                        location: location!(),
                                    });

                                    debug!("found proxy object");

                                    Ok(())
                                } else {
                                    Err(vec![DwarfError::Generic {
                                        description: format!(
                                            "Object `{}` not found in store",
                                            proxy
                                        ),
                                    }])
                                }
                            } else {
                                Err(vec![DwarfError::Generic {
                                    description: format!(
                                        "Model `{}` not found in store",
                                        store_name
                                    ),
                                }])
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        Err(vec![DwarfError::Generic {
                            description: "No object specified".to_owned(),
                        }])
                    }
                } else {
                    unreachable!();
                }
            } else if let Some(ty_vec) = attributes.get(TYPE) {
                if let Some((_, ref value)) = ty_vec.get(0) {
                    let type_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("proxy.ty: {type_name}");
                }
                Ok(())
            } else {
                Err(vec![DwarfError::Generic {
                    description: "No store specified".to_owned(),
                }])
            }
        } else {
            unreachable!();
        }

        // Below we are interring as an ObjectStore, according to it's annotation.
    } else if let Some(store_vec) = attributes.get(STORE) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = store_vec.get(0) {
            if let Some(model_vec) = attributes.get(MODEL) {
                if let Some((_, ref value)) = model_vec.get(0) {
                    let model_name: String = value.try_into().map_err(|e| vec![e])?;
                    debug!("store.model: {model_name}");

                    // Load the model.
                    let mut path = context.cwd.clone();
                    path.pop();
                    path.push(MODEL_DIR);
                    path.push("this is annoying");
                    path.set_file_name(&model_name);
                    path.set_extension(JSON_EXT);

                    let domain = DomainBuilder::new()
                        .cuckoo_model(path)
                        .map_err(|e| {
                            vec![DwarfError::Generic {
                                description: e.to_string(),
                            }]
                        })?
                        .build_v2()
                        .map_err(|e| {
                            vec![DwarfError::Generic {
                                description: e.to_string(),
                            }]
                        })?;

                    context
                        .models
                        .insert(domain.name().to_owned(), (domain.sarzak().clone(), None));

                    // 🚧 Really should check to see if it's already there.
                    let store = ZObjectStore::new(model_name, name.to_owned(), lu_dog);
                    context.dirty.push(Dirty::Store(s_read!(store).id));
                    let _ = ValueType::new_z_object_store(&store, lu_dog);

                    Ok(())
                } else {
                    unreachable!();
                }
            } else {
                Err(vec![DwarfError::Generic {
                    description: "No model specified".to_owned(),
                }])
            }
        } else {
            unreachable!();
        }
    } else {
        // This is just a plain vanilla user defined type.
        let woog_struct = WoogStruct::new(name.to_owned(), None, None, lu_dog);
        context.dirty.push(Dirty::Struct(woog_struct.clone()));
        let _ = ValueType::new_woog_struct(&woog_struct, lu_dog);

        let mut first = true;
        let mut first_generic = None;
        let mut last_generic_uuid: Option<usize> = None;
        for (generic, _) in generics {
            let generic = StructGeneric::new(generic.to_owned(), None, &woog_struct, lu_dog);
            if first {
                first = false;
                first_generic = Some(s_read!(generic).id);
            }
            last_generic_uuid = link_struct_generic!(last_generic_uuid, generic, lu_dog);
        }

        s_write!(woog_struct).first_generic = first_generic;

        context.struct_fields.push(StructFields {
            woog_struct,
            fields: fields.to_owned(),
            generics: generics.clone(),
            location: location!(),
        });

        Ok(())
    }
}

fn inter_struct_fields(
    woog_struct: RefType<WoogStruct>,
    fields: &[(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: &HashMap<String, Type>,
    location: Location,
    context: &mut Context,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    let mut errors = Vec::new();
    for ((name, _), (type_, span), attrs) in fields {
        let name = name.de_sanitize();

        debug!("field {name}");

        let type_str = type_.to_string();
        let ty = if let Some(_definition_type) = generics.get(&type_str) {
            let g = Generic::new(type_str, None, None, lu_dog);
            let ty = ValueType::new_generic(&g, lu_dog);
            LuDogSpan::new(
                span.end as i64,
                span.start as i64,
                &context.source,
                Some(&ty),
                None,
                lu_dog,
            );

            ty
        } else if let Some(proxy_vec) = attrs.get(PROXY) {
            if let Some((_, InnerAttribute::Attribute(ref attributes))) = proxy_vec.get(0) {
                // Get the plugin value
                if let Some(plugin_vec) = attributes.get(PLUGIN) {
                    if let Some((_, ref value)) = plugin_vec.get(0) {
                        let plugin_name: String = value.try_into().map_err(|e| vec![e])?;
                        debug!("proxy.plugin: {plugin_name}");
                        if let Type::UserType(tok, generics) = type_ {
                            let ty_name = tok.0.de_sanitize();
                            if ty_name == "Plugin" {
                                let plugin = Plugin::new(plugin_name, lu_dog);
                                let ty = ValueType::new_plugin(&plugin, lu_dog);
                                LuDogSpan::new(
                                    span.end as i64,
                                    span.start as i64,
                                    &context.source,
                                    Some(&ty),
                                    None,
                                    lu_dog,
                                );

                                ty
                            } else {
                                return Err(vec![DwarfError::Generic {
                                    description: format!("Expected `Plugin`, found `{ty_name}`.",),
                                }]);
                            }
                        } else {
                            return Err(vec![DwarfError::Generic {
                                description: format!("Expected `Plugin`, found `{type_}`.",),
                            }]);
                        }
                    } else {
                        unreachable!();
                    }
                } else {
                    return Err(vec![DwarfError::Generic {
                        description: "Expected `plugin` attribute".to_owned(),
                    }]);
                }
            } else {
                unreachable!();
            }
        } else {
            context.location = location;
            match make_value_type(type_, span, None, context, lu_dog) {
                Ok(ty) => ty,
                Err(mut err) => {
                    errors.append(&mut err);
                    continue;
                }
            }
        };

        //     Type::UserType(tok, generics) => {
        // let name = tok.0.de_sanitize();

        let _field = Field::new(name.to_owned(), &woog_struct, &ty, lu_dog);
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Get a Lu-Dog ValueType from a Dwarf Type
///
/// This is starting to look suspicious, at least for the Fn type. I'm basically
/// interring a half baked lambda into the store. I could fully bake it by interring
/// the parameters too, but then what's the point of the definition? Well, for one,
/// to inter the body.
///
/// Of course they are two different things. Without a body it's a type. With a
/// body it's an expression. I guess really I need to create a fully baked type
/// here because otherwise it'll never get done. So this is really a type factory.
///
/// It's actually good that we have the implementation sort of separate. We can
/// type check the signature against this type. But I think we need to cache it
/// somehow. Well, it is stored as the type of something, say a parameter. So, we
/// can look it up that way. Probably when, for instance, we type check a function
/// before execution.
pub(crate) fn make_value_type(
    type_: &Type,
    span: &Span,
    enclosing_type: Option<&RefType<ValueType>>,
    context: &Context,
    lu_dog: &mut LuDogStore,
) -> Result<RefType<ValueType>> {
    let sarzak = context.sarzak;

    debug!("make_value_type {type_:?}");

    match type_ {
        Type::Boolean => {
            let ty = Ty::new_boolean(sarzak);
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::Empty => Ok(ValueType::new_empty(lu_dog)),
        Type::Float => {
            let ty = Ty::new_float(sarzak);
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::Fn(ref params, ref return_type) => {
            let return_type =
                make_value_type(&return_type.0, span, enclosing_type, context, lu_dog)?;
            let lambda = Lambda::new(None, &return_type, lu_dog);

            let mut last_param_uuid: Option<usize> = None;
            for (position, (param_ty, param_span)) in params.iter().enumerate() {
                let param_ty = make_value_type(param_ty, param_span, None, context, lu_dog)?;
                debug!("param_ty {:?}", param_ty);

                let param = LambdaParameter::new(
                    position as DwarfInteger,
                    &lambda,
                    None,
                    Some(&param_ty),
                    lu_dog,
                );
                debug!("param {:?}", param);

                last_param_uuid = link_ƛ_parameter!(last_param_uuid, param, lu_dog);
            }

            let lambda = ValueType::new_lambda(&lambda, lu_dog);
            Ok(lambda)
        }
        Type::Generic((t, _)) => {
            let ty = Generic::new(t.to_owned(), None, None, lu_dog);
            Ok(ValueType::new_generic(&ty, lu_dog))
        }
        Type::Integer => {
            let ty = Ty::new_integer(sarzak);
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::List(ref type_) => {
            let inner_type = make_value_type(&type_.0, &type_.1, enclosing_type, context, lu_dog)?;
            let list = List::new(&inner_type, lu_dog);
            Ok(ValueType::new_list(&list, lu_dog))
        }
        Type::Self_ => match enclosing_type {
            Some(ty) => Ok(ty.clone()),
            None => Err(vec![DwarfError::BadSelf {
                file: context.file_name.to_owned(),
                span: span.clone(),
                location: context.location,
            }]),
        },
        Type::String => {
            let ty = Ty::new_s_string(sarzak);
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        Type::UserType(tok, generics) => {
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
                unreachable!();
                Ok(ValueType::new_import(&import, lu_dog))
            } else if name == "Future" {
                let inner_type =
                    make_value_type(&generics[0].0, span, enclosing_type, context, lu_dog)?;
                let future = XFuture::new(&inner_type, lu_dog);

                Ok(ValueType::new_x_future(&future, lu_dog))
            // } else if name == "Plugin" {
            //     let inner_type =
            //         make_value_type(&generics[0].0, span, enclosing_type, context, lu_dog)?;
            //     let future = XFuture::new(&inner_type, lu_dog);

            //     Ok(ValueType::new_x_future(&future, lu_dog))
            } else if name == "Self" {
                match enclosing_type {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(vec![DwarfError::BadSelf {
                        file: context.file_name.to_owned(),
                        span: tok.1.clone(),
                        location: context.location,
                    }]),
                }
            } else if name == "String" {
                Ok(ValueType::new_ty(&Ty::new_s_string(sarzak), lu_dog))
            } else if name == UUID_TYPE {
                Ok(ValueType::new_ty(&Ty::new_s_uuid(sarzak), lu_dog))
            } else {
                // 🚧 HashMapFix
                for model in context.models.values() {
                    // Look for the Object in the model domains first.
                    if let Some(ty) = model.0.iter_ty().find(|ty| match &*ty.read().unwrap() {
                        Ty::Object(ref obj) => {
                            let obj = model.0.exhume_object(obj).unwrap();
                            // We are going to cheat a little bit here. Say we have an
                            // object called `Point`. We want to be able to also handle
                            // proxy objects for `Point`. Those are suffixed with "Proxy".
                            let obj = obj.read().unwrap().name.to_upper_camel_case();
                            obj == *name || name == format!("{}Proxy", obj) || obj == *name
                        }
                        _ => false,
                    }) {
                        return Ok(ValueType::new_ty(&ty, lu_dog));
                    }
                }

                if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(name) {
                    // Here is where we look for actual user defined types, as
                    // in types that are defined in dwarf source.
                    let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                    let ty = ValueType::new_woog_struct(&woog_struct, lu_dog);
                    LuDogSpan::new(
                        span.end as i64,
                        span.start as i64,
                        &context.source,
                        Some(&ty),
                        None,
                        lu_dog,
                    );

                    Ok(ty)
                } else if let Some(ref id) = lu_dog.exhume_enumeration_id_by_name(name) {
                    // Here too, but for enums.
                    let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
                    Ok(ValueType::new_enumeration(&woog_enum, lu_dog))
                    // }
                } else if let Some(ref id) = lu_dog.exhume_z_object_store_id_by_name(name) {
                    let store = lu_dog.exhume_z_object_store(id).unwrap();
                    Ok(ValueType::new_z_object_store(&store, lu_dog))
                } else if let Some(ty) = sarzak.iter_ty().find(|ty| match &*ty.read().unwrap() {
                    // Unlikely to have to reach back this far.
                    Ty::Object(ref obj) => {
                        let obj = sarzak.exhume_object(obj).unwrap();
                        let obj = obj.read().unwrap().name.to_upper_camel_case();
                        obj == *name || name == format!("{}Proxy", obj)
                    }
                    _ => false,
                }) {
                    Ok(ValueType::new_ty(&ty, lu_dog))
                } else {
                    Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        file: context.file_name.to_owned(),
                        span: span.to_owned(),
                        location: context.location,
                    }])
                }
            }
        }
        Type::Uuid => {
            let ty = Ty::new_s_uuid(sarzak);
            Ok(ValueType::new_ty(&ty, lu_dog))
        }
        道 => todo!("get_value_type missing implementation for {:?}", 道),
    }
}

pub(crate) fn lookup_woog_struct_method_return_type(
    type_name: &str,
    method: &str,
    sarzak: &SarzakStore,
    lu_dog: &mut LuDogStore,
) -> RefType<ValueType> {
    // Look up the type in lu_dog structs.
    if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(type_name) {
        let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
        let ty = if let Some(impl_) = s_read!(woog_struct).r8c_implementation_block(lu_dog).pop() {
            let impl_ = s_read!(impl_);

            let funcs = impl_.r9_function(lu_dog);
            funcs.iter().find(|f| s_read!(f).name == *method).map(|f| {
                let ret_ty = s_read!(f).return_type;
                lu_dog.exhume_value_type(&ret_ty).unwrap()
            })
        } else {
            debug!("ParserExpression type not found");
            e_warn!("Unknown type for variable {method}");
            Some(ValueType::new_unknown(lu_dog))
        };
        debug!("ParserExpression found type: {ty:?}");
        if let Some(ty) = ty {
            ty
        } else {
            e_warn!("Unknown type for variable {method}");
            ValueType::new_unknown(lu_dog)
        }
    } else if type_name == CHACHA {
        match method {
            "args" => {
                let ty = Ty::new_s_string(sarzak);
                // 🚧 Ideally we'd cache this when we startup.
                let ty = lu_dog
                    .iter_value_type()
                    .find(|t| s_read!(t).subtype == ValueTypeEnum::Ty(ty.read().unwrap().id()))
                    .unwrap();
                let list = List::new(&ty, lu_dog);
                ValueType::new_list(&list, lu_dog)
            }
            _ => {
                e_warn!("ParserExpression type not found");
                ValueType::new_unknown(lu_dog)
            }
        }
    } else if type_name == UUID_TYPE && method == FN_NEW {
        ValueType::new_ty(&Ty::new_s_uuid(sarzak), lu_dog)
    } else {
        e_warn!("ParserExpression type not found");
        ValueType::new_unknown(lu_dog)
    }
}

pub(crate) trait DeSanitize {
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

pub(crate) fn de_sanitize(string: &str) -> Option<&str> {
    match string {
        "False Literal" => Some("False"),
        "FalseLiteral" => Some("False"),
        "SString" => Some("String"),
        "SUuid" => Some("Uuid"),
        "True Literal" => Some("True"),
        "TrueLiteral" => Some("True"),
        "Ty" => Some("Type"),
        "WoogOption" => Some("Option"),
        "WoogStruct" => Some("Struct"),
        "XSuper" => Some("Super"),
        "XSuperProxy" => Some("SuperProxy"),
        "XBox" => Some("Box"),
        "XBoxProxy" => Some("BoxProxy"),
        "XIf" => Some("If"),
        "XMacro" => Some("Macro"),
        "XMatch" => Some("Match"),
        "XPrint" => Some("Print"),
        "XReturn" => Some("Return"),
        "XValue" => Some("Value"),
        "ZObjectStore" => Some("ObjectStore"),
        "ZSome" => Some("Some"),
        "ZNone" => Some("None"),
        _ => None,
    }
}

pub(super) fn typecheck(
    lhs: (&RefType<ValueType>, &Span),
    rhs: (&RefType<ValueType>, &Span),
    location: Location,
    context: &Context,
    lu_dog: &LuDogStore,
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
        // Promote unknown to the other type.
        (ValueTypeEnum::Unknown(_), _) => Ok(()),
        (_, ValueTypeEnum::Unknown(_)) => Ok(()),
        (ValueTypeEnum::Ty(a), ValueTypeEnum::Ty(b)) => {
            let a = context.sarzak.exhume_ty(a).unwrap();
            let b = context.sarzak.exhume_ty(b).unwrap();
            let a = a.read().unwrap();
            let b = b.read().unwrap();
            match (&*a, &*b) {
                (Ty::Integer(_), Ty::Integer(_)) => Ok(()),
                (Ty::Float(_), Ty::Float(_)) => Ok(()),
                (Ty::Boolean(_), Ty::Boolean(_)) => Ok(()),
                (Ty::SString(_), Ty::SString(_)) => Ok(()),
                (Ty::SUuid(_), Ty::SUuid(_)) => Ok(()),
                (a, b) => {
                    // dbg!(PrintableValueType(lhs, context, lu_dog).to_string());
                    // dbg!(PrintableValueType(rhs, context, lu_dog).to_string());
                    if a == b {
                        Ok(())
                    } else {
                        let a = PrintableValueType(lhs, context, lu_dog);
                        let b = PrintableValueType(rhs, context, lu_dog);

                        Err(vec![DwarfError::TypeMismatch {
                            expected: a.to_string(),
                            found: b.to_string(),
                            file: context.file_name.to_owned(),
                            expected_span: lhs_span.to_owned(),
                            found_span: rhs_span.to_owned(),
                            location,
                        }])
                    }
                }
            }
        }
        // (ValueTypeEnum::Unknown(_), _) => Ok(()),
        // 🚧 This is a terrible hack. It's very temporary. Related to range_type_bug.
        (_, ValueTypeEnum::Range(_)) => Ok(()),
        (ValueTypeEnum::Char(_), ValueTypeEnum::Ty(id)) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::SString(_) => Ok(()),
                _ => {
                    // dbg!(PrintableValueType(lhs, context, lu_dog).to_string());
                    // dbg!(PrintableValueType(rhs, context, lu_dog).to_string());
                    let lhs = PrintableValueType(lhs, context, lu_dog);
                    let rhs = PrintableValueType(rhs, context, lu_dog);

                    Err(vec![DwarfError::TypeMismatch {
                        expected: lhs.to_string(),
                        found: rhs.to_string(),
                        file: context.file_name.to_owned(),
                        expected_span: lhs_span.to_owned(),
                        found_span: rhs_span.to_owned(),
                        location,
                    }])
                }
            }
        }
        (lhs_t, rhs_t) => {
            // dbg!(PrintableValueType(lhs, context, lu_dog).to_string());
            // dbg!(PrintableValueType(rhs, context, lu_dog).to_string());
            if lhs_t == rhs_t {
                Ok(())
            } else {
                let lhs = PrintableValueType(lhs, context, lu_dog);
                let rhs = PrintableValueType(rhs, context, lu_dog);

                Err(vec![DwarfError::TypeMismatch {
                    expected: lhs.to_string(),
                    found: rhs.to_string(),
                    file: context.file_name.to_owned(),
                    expected_span: lhs_span.to_owned(),
                    found_span: rhs_span.to_owned(),
                    location,
                }])
            }
        }
    }
}

pub(crate) fn create_generic_struct(
    woog_struct: &RefType<WoogStruct>,
    substitutions: &HashMap<String, RefType<ValueType>>,
    context: &mut Context,
    sarzak: &SarzakStore,
    lu_dog: &mut LuDogStore,
) -> RefType<WoogStruct> {
    let mut name = s_read!(woog_struct).name.to_owned();
    name.push_str("<");
    let first = s_read!(woog_struct).r102_struct_generic(lu_dog)[0].clone();
    name.push_str(&s_read!(first).name);
    let mut id = s_read!(first).next;
    while let Some(next_id) = id {
        let next = lu_dog.exhume_struct_generic(&next_id).unwrap();
        let next = s_read!(next);
        id = next.next;

        // dbg!(&next.name, &substitutions);

        let ty = substitutions.get(&next.name).unwrap();
        let ty = PrintableValueType(&ty, context, lu_dog).to_string();

        name.extend([", ", &ty]);
    }
    name.push('>');

    let mut obj = s_read!(woog_struct).r4_object(sarzak);
    let obj = if !obj.is_empty() {
        let obj = obj.pop().unwrap();
        let obj = obj.read().unwrap().clone();
        Some(obj)
    } else {
        None
    };

    let new_struct = WoogStruct::new(name.to_owned(), None, obj.as_ref(), lu_dog);
    context.dirty.push(Dirty::Struct(new_struct.clone()));
    let _ = ValueType::new_woog_struct(&new_struct, lu_dog);
    for field in s_read!(woog_struct).r7_field(lu_dog) {
        let field = s_read!(field);
        let ty = &field.r5_value_type(lu_dog)[0];
        let _ = Field::new(field.name.to_owned(), &new_struct, ty, lu_dog);
    }

    new_struct
}

// Note that the name is expected to contain the generic component.
pub(crate) fn create_generic_enum(
    enum_name: &str,
    enum_path: &ParserExpression,
    lu_dog: &mut LuDogStore,
) -> (RefType<Enumeration>, RefType<ValueType>) {
    debug!("interring generic enum {enum_name}");
    let new_enum = Enumeration::new(enum_name.to_owned(), None, lu_dog);
    let ty = ValueType::new_enumeration(&new_enum, lu_dog);

    let name_without_generics = enum_name.split('<').collect::<Vec<_>>()[0];

    debug!("name_without_generics {:?}", name_without_generics);
    let id = lu_dog
        .exhume_enumeration_id_by_name(name_without_generics)
        .unwrap();
    let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
    for field in s_read!(woog_enum).r88_enum_field(lu_dog) {
        let field = s_read!(field);
        match field.subtype {
            EnumFieldEnum::Unit(ref id) => {
                let orig = lu_dog.exhume_unit(id).unwrap();
                let new = Unit::new(s_read!(orig).x_value, lu_dog);
                let _ = LuDogEnumField::new_unit(field.name.to_owned(), &new_enum, &new, lu_dog);
            }
            EnumFieldEnum::StructField(ref id) => {
                let orig = lu_dog.exhume_struct_field(id).unwrap();
                let new = StructField::new(s_read!(orig).name.to_owned(), lu_dog);
                let _ = LuDogEnumField::new_struct_field(
                    field.name.to_owned(),
                    &new_enum,
                    &new,
                    lu_dog,
                );
            }
            EnumFieldEnum::TupleField(ref id) => {
                // Note that we are borrowing whatever expression may exist on
                // the original, non-generic tuple field.
                let orig = lu_dog.exhume_tuple_field(id).unwrap();
                let new = TupleField::new(
                    Uuid::new_v4(),
                    &s_read!(orig).r86_value_type(lu_dog)[0],
                    lu_dog,
                );
                let _ =
                    LuDogEnumField::new_tuple_field(field.name.to_owned(), &new_enum, &new, lu_dog);
            }
        }
    }

    (new_enum, ty)
}

pub(crate) fn update_span_value(
    span: &RefType<LuDogSpan>,
    value: &RefType<XValue>,
    location: Location,
) {
    debug!(
        "{}:{}:{} -- {value:?}",
        location.file, location.line, location.column
    );
    s_write!(span).x_value = Some(s_read!(value).id);
    debug!("span: {:?}", s_read!(span));
}

// fn update_span_type(span: &mut RefType<LuDogSpan>, value_type: &RefType<ValueType>) {
// s_write!(span).value_type = Some(s_read!(value_type).id);
// }
