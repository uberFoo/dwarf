use std::{env, fs, ops::Range, path::PathBuf};

use ansi_term::Colour;
use heck::ToUpperCamelCase;
use log;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    dwarf::{
        error::{DwarfError, Result},
        items::{
            enuum::{self, create_generic_enum},
            func, strukt,
        },
        parse_dwarf, AttributeMap, BlockType, DwarfInteger, EnumField,
        Expression as ParserExpression, Generics, InnerAttribute, InnerItem, Item,
        PrintableValueType, Spanned, Statement as ParserStatement, Type, WrappedValueType,
    },
    keywords::{ARGS, CHACHA, FN_NEW, FQ_UUID_TYPE, UUID_TYPE},
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{
            AWait, Block, Body, BooleanOperator, Call, CharLiteral, DataStructure, EnumFieldEnum,
            Expression, ExpressionBit, ExpressionEnum, ExpressionStatement, Field, FieldExpression,
            ForLoop, FormatBit, FormatString, FuncGeneric, FunctionCall, ImplementationBlock,
            Import, Index, IntegerLiteral, Item as WoogItem, ItemStatement, Lambda,
            LambdaParameter, LetStatement, Literal, LocalVariable, NamedFieldExpression,
            Pattern as AssocPat, RangeExpression, Span as LuDogSpan, Statement, StringBit,
            StringLiteral, StructExpression, ValueType, ValueTypeEnum, Variable,
            VariableExpression, WoogStruct, XFuture, XIf, XMatch, XPath, XPrint, XValue,
            XValueEnum,
        },
        Argument, Binary, BooleanLiteral, Comparison, DwarfSourceFile, FieldAccess,
        FieldAccessTarget, FloatLiteral, List, ListElement, ListExpression, Operator,
        ResultStatement, Unary, VariableEnum, XReturn,
    },
    new_ref, s_read, s_write,
    sarzak::{store::ObjectStore as SarzakStore, types::Ty},
    Context as InterContext, Dirty, ModelStore, NewRef, RefType, SarzakStorePtr, PATH_ROOT,
    PATH_SEP,
};

mod expression;
use expression::{addition, and, expr_as, method_call, static_method_call, unit_enum};

pub(super) const EXTENSION_DIR: &str = "extensions";
pub(super) const JSON_EXT: &str = "json";
pub(super) const LIB_DIR: &str = "lib";
pub(super) const LIB_TAO: &str = "lib.ore";
pub(super) const MODEL_DIR: &str = "models";
pub(super) const SRC_DIR: &str = "src";
pub(super) const TAO_EXT: &str = "ore";

macro_rules! link_format_bits {
    ($last:expr, $next:expr, $store:expr) => {{
        let next = s_read!($next);
        if let Some(last) = $last {
            let last = $store.exhume_format_bit(&last).unwrap().clone();
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

macro_rules! trace {
    ($($arg:tt)*) => {
        log::trace!(
            target: "extruder",
            "{}: {}\n  --> {}:{}:{}",
            Colour::Blue.dimmed().italic().paint(function!()),
            format_args!($($arg)*),
            file!(),
            line!(),
            column!()
        )
    };
}
pub(crate) use trace;

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

#[allow(unused_macros)]
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
#[allow(unused_imports)]
pub(crate) use error;

pub(super) type Span = Range<usize>;
pub(super) type ExprSpan = (RefType<Expression>, RefType<LuDogSpan>);

// These below are just to avoid cloning things.
struct ConveyFunc<'a> {
    a_sink: &'a BlockType,
    name: &'a str,
    attributes: &'a AttributeMap,
    span: &'a Span,
    params: &'a [(Spanned<String>, Spanned<Type>)],
    return_type: &'a Spanned<Type>,
    generics: Option<HashMap<String, Type>>,
    statements: Option<&'a Spanned<ParserExpression>>,
}

impl<'a> ConveyFunc<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        a_sink: &'a BlockType,
        name: &'a str,
        attributes: &'a AttributeMap,
        span: &'a Span,
        params: &'a [(Spanned<String>, Spanned<Type>)],
        return_type: &'a Spanned<Type>,
        generics: Option<HashMap<String, Type>>,
        statements: Option<&'a Spanned<ParserExpression>>,
    ) -> Self {
        Self {
            a_sink,
            name,
            attributes,
            span,
            params,
            return_type,
            generics,
            statements,
        }
    }
}

struct ConveyStruct<'a> {
    name: String,
    span: &'a Span,
    attributes: &'a AttributeMap,
    fields: &'a [(Spanned<String>, Spanned<Type>, AttributeMap)],
    generics: Option<HashMap<String, Type>>,
}

impl<'a> ConveyStruct<'a> {
    fn new(
        name: String,
        span: &'a Span,
        attributes: &'a AttributeMap,
        fields: &'a [(Spanned<String>, Spanned<Type>, AttributeMap)],
        generics: Option<HashMap<String, Type>>,
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
    name: &'a Spanned<String>,
    attributes: &'a AttributeMap,
    fields: &'a [(Spanned<String>, Option<EnumField>)],
    generics: Option<HashMap<String, Type>>,
}

impl<'a> ConveyEnum<'a> {
    fn new(
        name: &'a Spanned<String>,
        attributes: &'a AttributeMap,
        fields: &'a [(Spanned<String>, Option<EnumField>)],
        generics: Option<HashMap<String, Type>>,
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
    pub woog_struct: RefType<WoogStruct>,
    pub fields: Vec<(Spanned<String>, Spanned<Type>, AttributeMap)>,
    // I'd really like to keep this as a reference, rather than cloning it.
    pub generics: Option<HashMap<String, Type>>,
    pub location: Location,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub params: Vec<(String, RefType<ValueType>)>,
    pub return_type: RefType<ValueType>,
}

#[derive(Debug)]
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
    pub source_string: String,
    pub models: &'a mut ModelStore,
    pub sarzak: &'a SarzakStore,
    pub dwarf_home: &'a PathBuf,
    pub cwd: PathBuf,
    pub dirty: &'a mut Vec<Dirty>,
    pub file_name: &'a str,
    pub func_defs: HashMap<String, FunctionDefinition>,
    pub path: String,
    pub in_impl: String,
    pub scopes: &'a mut HashMap<String, String>,
    pub imports: &'a mut HashSet<PathBuf>,
}

impl<'a> Context<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        source: String,
        sarzak: &'a SarzakStore,
        file_name: &'a str,
        cwd: PathBuf,
        dwarf_home: &'a PathBuf,
        models: &'a mut ModelStore,
        dirty: &'a mut Vec<Dirty>,
        location: Location,
        lu_dog: &mut LuDogStore,
        path: String,
        scopes: &'a mut HashMap<String, String>,
        imports: &'a mut HashSet<PathBuf>,
    ) -> Self {
        Self {
            location,
            struct_fields: Vec::new(),
            source: DwarfSourceFile::new(source.clone(), lu_dog),
            source_string: source,
            models,
            sarzak,
            dwarf_home,
            cwd,
            dirty,
            file_name,
            func_defs: HashMap::default(),
            path,
            in_impl: "".to_owned(),
            scopes,
            imports,
        }
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
    file_name: String,
    source: Option<(String, &[Item])>,
    dwarf_home: &PathBuf,
    sarzak: &SarzakStore,
) -> Result<InterContext> {
    let mut lu_dog = LuDogStore::new();

    // We need to stuff all of the sarzak types into the store.
    ValueType::new_ty(true, &Ty::new_boolean(sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_float(sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_integer(sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_z_string(sarzak), &mut lu_dog);
    ValueType::new_ty(true, &Ty::new_z_uuid(sarzak), &mut lu_dog);
    // This one I'm not so sure about. It's here for initializing new lists.
    // I don't know why it's not being put into the store where used.
    ValueType::new_list(
        true,
        &List::new(&ValueType::new_empty(true, &mut lu_dog), &mut lu_dog),
        &mut lu_dog,
    );

    let mut models = HashMap::default();
    let mut scopes = HashMap::default();
    let mut dirty = Vec::new();
    let mut stack = Vec::new();
    let mut imports = HashSet::default();

    if let Some((source, ast)) = source {
        let mut context = Context {
            location: location!(),
            struct_fields: Vec::new(),
            source: DwarfSourceFile::new(source.clone(), &mut lu_dog),
            source_string: source,
            models: &mut models,
            sarzak,
            dwarf_home,
            cwd: env::current_dir().unwrap(),
            dirty: &mut dirty,
            file_name: file_name.as_str(),
            func_defs: HashMap::default(),
            path: PATH_ROOT.to_string(),
            in_impl: "".to_owned(),
            scopes: &mut scopes,
            imports: &mut imports,
        };

        walk_tree(ast, &mut context, &mut stack, &mut lu_dog)?;
    };

    Ok(InterContext {
        source_path: file_name,
        lu_dog: new_ref!(LuDogStore, lu_dog),
        models,
        dirty,
        sarzak: new_ref!(SarzakStore, sarzak.clone()),
        scopes,
        imports,
    })
}

fn walk_tree(
    ast: &[Item],
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    let mut funcs = Vec::new();
    let mut implementations = Vec::new();
    let mut structs = Vec::new();
    let mut enums = Vec::new();

    // We need the structs before the impls. We also need function signatures.
    // So we walk the tree and cache what we find so that we may then inter
    // things in the order that we need.
    for item in ast {
        match item {
            Item {
                item:
                    (
                        InnerItem::Enum {
                            name,
                            fields,
                            generics,
                        },
                        _,
                    ),
                attributes,
            } => {
                let generics = if let Some((generics, _)) = generics {
                    Some(
                        generics
                            .iter()
                            .map(|(t, _)| match t {
                                generic @ Type::Generic((t, _)) => (t.to_owned(), generic.clone()),
                                oops => panic!("Unexpected type: {oops:?}"),
                            })
                            .collect(),
                    )
                } else {
                    None
                };
                enums.push(ConveyEnum::new(&name, attributes, fields, generics))
            }
            Item {
                item:
                    (
                        InnerItem::Function {
                            a_sink,
                            name,
                            params,
                            return_type,
                            generics,
                            statements,
                        },
                        span,
                    ),
                attributes,
            } => {
                let generics = if let Some((generics, _)) = generics {
                    Some(
                        generics
                            .iter()
                            .map(|(t, _)| match t {
                                generic @ Type::Generic((ref t, _)) => {
                                    (t.to_owned(), generic.clone())
                                }

                                fubared => {
                                    dbg!(fubared);
                                    unreachable!();
                                }
                            })
                            .collect(),
                    )
                } else {
                    None
                };

                funcs.push(ConveyFunc::new(
                    a_sink,
                    &name.0,
                    attributes,
                    span,
                    params,
                    return_type,
                    generics,
                    statements.as_ref(),
                ))
            }
            Item {
                item: (InnerItem::Implementation((name, _name_span), funcs), span),
                attributes,
            } => implementations.push(ConveyImpl::new(name, span, attributes, funcs)),
            // Imports can happen any time, I think.
            Item {
                item: (InnerItem::Import((path, _path_span), alias), _span),
                attributes: _,
            } => inter_import(path, alias, context, context_stack, lu_dog)?,
            Item {
                item: (InnerItem::Module((name, _name_span)), _span),
                attributes: _,
            } => inter_module(name, context, context_stack, lu_dog)?,
            Item {
                item: (InnerItem::Struct((name, _), fields, generics), span),
                attributes,
            } => {
                let generics = if let Some((generics, _)) = generics {
                    Some(
                        generics
                            .iter()
                            .map(|(t, _)| match t {
                                generic @ Type::Generic((ref t, _)) => {
                                    (t.to_owned(), generic.clone())
                                }
                                fubared => {
                                    dbg!(fubared);
                                    unreachable!();
                                }
                            })
                            .collect(),
                    )
                } else {
                    None
                };

                structs.push(ConveyStruct::new(
                    name.to_owned(),
                    span,
                    attributes,
                    fields,
                    generics,
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
        let _ = strukt::inter_struct(
            name,
            span,
            attributes,
            fields,
            generics.as_ref(),
            context,
            lu_dog,
        )
        .map_err(|mut e| {
            errors.append(&mut e);
        });
    }

    for ConveyEnum {
        name,
        attributes,
        fields,
        generics,
    } in &enums
    {
        debug!("Interring enum `{}` fields", name.0);
        let _ = enuum::inter_enum(
            name,
            attributes,
            fields,
            generics.as_ref(),
            context,
            context_stack,
            lu_dog,
        )
        .map_err(|mut e| {
            errors.append(&mut e);
        });
    }

    // This needs to be after the enums are interred.
    for _ in &structs {
        let params = context.struct_fields.drain(..).collect::<Vec<_>>();
        for StructFields {
            woog_struct,
            fields,
            generics,
            location,
        } in params
        {
            let _ = strukt::inter_struct_fields(
                woog_struct,
                &fields,
                generics.as_ref(),
                location,
                context,
                context_stack,
                lu_dog,
            )
            .map_err(|mut e| {
                errors.append(&mut e);
            });
        }
    }

    // Scan the function signatures for type information, which is stored in the
    // LuDog store.
    for ConveyFunc {
        a_sink: _,
        name,
        attributes: _,
        span: _,
        params,
        return_type,
        generics,
        statements: _,
    } in &funcs
    {
        let _ = func::parse_func_signature(
            name,
            params,
            generics.as_ref(),
            return_type,
            None,
            context,
            context_stack,
            lu_dog,
        )
        .map_err(|mut e| {
            errors.append(&mut e);
        });
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
        let _ = inter_implementation(
            name,
            attributes,
            funcs,
            span,
            context,
            context_stack,
            lu_dog,
        )
        .map_err(|mut e| {
            errors.append(&mut e);
        });
    }

    // Finally, inter the loose functions.
    for ConveyFunc {
        a_sink,
        name,
        attributes,
        span,
        params,
        return_type,
        generics,
        statements,
    } in funcs
    {
        debug!("Interring function `{}`", name);
        let _ = func::inter_func(
            a_sink,
            name,
            attributes,
            params,
            return_type,
            generics.as_ref(),
            statements,
            None,
            None,
            span,
            context,
            context_stack,
            lu_dog,
        )
        .map_err(|mut e| errors.append(&mut e));
    }

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
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
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
                context_stack,
                lu_dog,
            )?;

            let stmt = ExpressionStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_expression_statement(index, block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ValueType::new_empty(true, lu_dog)))
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
                            InnerItem::Function {
                                a_sink,
                                name,
                                params,
                                return_type,
                                statements,
                                generics,
                            },
                            span,
                        ),
                    attributes,
                } => {
                    let generics = if let Some((generics, _)) = generics {
                        Some(
                            generics
                                .iter()
                                .map(|(t, _)| match t {
                                    generic @ Type::Generic((ref t, _)) => {
                                        (t.to_owned(), generic.clone())
                                    }
                                    fubared => {
                                        dbg!(fubared);
                                        unreachable!();
                                    }
                                })
                                .collect(),
                        )
                    } else {
                        None
                    };

                    func::inter_func(
                        a_sink,
                        &name.0,
                        attributes,
                        params,
                        return_type,
                        generics.as_ref(),
                        statements.as_ref(),
                        None,
                        None,
                        span,
                        context,
                        context_stack,
                        lu_dog,
                    )?;
                    span
                }
                Item {
                    item: (InnerItem::Implementation((name, _name_span), funcs), span),
                    attributes,
                } => {
                    inter_implementation(
                        name,
                        attributes,
                        funcs,
                        span,
                        context,
                        context_stack,
                        lu_dog,
                    )?;
                    span
                }
                Item {
                    item: (InnerItem::Import((path, _path_span), alias), span),
                    attributes: _,
                } => {
                    inter_import(path, alias, context, context_stack, lu_dog)?;
                    span
                }
                Item {
                    item: (InnerItem::Struct((name, _span), fields, generics), outer_span),
                    attributes,
                } => {
                    let generics = if let Some((generics, _)) = generics {
                        Some(
                            generics
                                .iter()
                                .map(|(t, _)| match t {
                                    generic @ Type::Generic((ref t, _)) => {
                                        (t.to_owned(), generic.clone())
                                    }
                                    _ => unreachable!(),
                                })
                                .collect(),
                        )
                    } else {
                        None
                    };

                    strukt::inter_struct(
                        name,
                        outer_span,
                        attributes,
                        fields,
                        generics.as_ref(),
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
                        strukt::inter_struct_fields(
                            woog_struct,
                            &fields,
                            generics.as_ref(),
                            location,
                            context,
                            context_stack,
                            lu_dog,
                        )
                    })?;
                    outer_span
                }
                _ => unimplemented!(),
            };
            let _stmt = ItemStatement::new();
            let stmt = Statement::new_item_statement(index, block, None, lu_dog);
            Ok(((stmt, span.to_owned()), ValueType::new_empty(true, lu_dog)))
        }
        //
        // Let
        //
        ParserStatement::Let((var_name, var_span), var_type, (expr, expr_span)) => {
            // Setup the local variable that is the LHS of the statement.
            let (var, local) = {
                let local = LocalVariable::new(Uuid::new_v4(), lu_dog);
                (
                    Variable::new_local_variable(var_name.to_owned(), &local, lu_dog),
                    local,
                )
            };

            debug!("inter let {var:?}");

            // Now parse the RHS, which is an expression.
            let (expr, expr_ty) = inter_expression(
                &new_ref!(ParserExpression, expr.to_owned()),
                expr_span,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            debug!("inter let expr {expr:?}, ty {expr_ty:?}");

            let ty = if let Some((ty, span)) = var_type {
                let lhs_ty = ty.into_value_type(span, context, lu_dog)?;

                typecheck(
                    (&lhs_ty, span),
                    (&expr_ty, expr_span),
                    location!(),
                    context,
                    lu_dog,
                )?;
                lhs_ty
            } else {
                expr_ty
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

            Ok((
                (stmt, expr_span.to_owned()),
                ValueType::new_empty(true, lu_dog),
            ))
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
                context_stack,
                lu_dog,
            )?;
            let stmt = ResultStatement::new(&expr.0, lu_dog);
            let stmt = Statement::new_result_statement(index, block, None, &stmt, lu_dog);

            Ok(((stmt, span.to_owned()), ty))
        }
        道 => todo!("{:?}", 道),
    }
}

pub(super) fn inter_statements(
    statements: &[RefType<ParserStatement>],
    span: &Span,
    block: &RefType<Block>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<Spanned<RefType<ValueType>>> {
    let mut value_type = ValueType::new_empty(true, lu_dog);
    let mut span = span.to_owned();
    let mut errors = Vec::new();

    let mut last_stmt_uuid: Option<SarzakStorePtr> = None;
    let mut index = 0;
    for stmt in statements {
        let (stmt, ty) = match inter_statement(stmt, index, block, context, context_stack, lu_dog) {
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
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<(ExprSpan, RefType<ValueType>)> {
    debug!("expr {expr:?}, span {span:?}");
    // debug!("source {}", context.source_string[span.clone()].to_owned());

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
            addition::inter(lhs_p, rhs_p, span, block, context, context_stack, lu_dog)
        }
        ParserExpression::And(ref lhs_p, ref rhs_p) => {
            and::inter(lhs_p, rhs_p, span, block, context, context_stack, lu_dog)
        }
        ParserExpression::As(ref expr, ref ty) => {
            expr_as::inter(expr, ty, span, block, context, context_stack, lu_dog)
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Binary::new_assignment(true, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Await
        //
        ParserExpression::Await(ref expr_p) => {
            debug!("await: {expr_p:?}");

            let (expr, ty) = inter_expression(
                &new_ref!(ParserExpression, expr_p.0.to_owned()),
                &expr_p.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            if !matches!(s_read!(ty).subtype, ValueTypeEnum::XFuture(_)) {
                let ty = PrintableValueType(&ty, context, lu_dog);
                Err(vec![DwarfError::AwaitNotFuture {
                    file: context.file_name.to_owned(),
                    found: ty.to_string(),
                    span: expr_p.1.clone(),
                    program: context.source_string.to_owned(),
                }])
            } else {
                let future = match s_read!(ty).subtype {
                    ValueTypeEnum::XFuture(ref id) => lu_dog.exhume_x_future(id).unwrap(),
                    _ => unreachable!(),
                };
                let ty = s_read!(future).r2_value_type(lu_dog)[0].clone();
                let expr = AWait::new(&expr.0, lu_dog);
                let expr = Expression::new_a_wait(true, &expr, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let not = Unary::new_not(true, lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &not, lu_dog);
            let expr = Expression::new_operator(true, &operator, lu_dog);
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
                debug!("variable {var:?}");
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

            let expr = Expression::new_block(true, &block, lu_dog);
            let ty = inter_statements(
                &stmts_vec,
                &stmts_span,
                &block,
                context,
                context_stack,
                lu_dog,
            )?;
            let value = XValue::new_expression(&block, &ty.0, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            // If it's an async block then wrap it in a future.
            let ty = match a_sink {
                BlockType::Async => {
                    let span = ty.1;
                    let future = XFuture::new(&ty.0, lu_dog);
                    (ValueType::new_x_future(true, &future, lu_dog), span)
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
                BooleanLiteral::new_true_literal(true, lu_dog)
            } else {
                BooleanLiteral::new_false_literal(true, lu_dog)
            };
            let expr = Expression::new_literal(
                true,
                &Literal::new_boolean_literal(true, &literal, lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(true, &Ty::new_boolean(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // CharLiteral
        //
        ParserExpression::CharLiteral(literal) => {
            let literal = CharLiteral::new(literal, lu_dog);
            let expr = Expression::new_literal(
                true,
                &Literal::new_char_literal(true, &literal, lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_char(true, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Debug
        //
        ParserExpression::Debug => {
            let expr = Expression::new_x_debugger(true, lu_dog);
            let ty = ValueType::new_empty(true, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Binary::new_division(true, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Empty
        //
        ParserExpression::Empty => {
            let expr = Expression::new_empty_expression(true, lu_dog);
            let ty = ValueType::new_empty(true, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_equal(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

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
                context_stack,
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
                            let fat = FieldAccessTarget::new_field(true, &field, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(true, &expr, lu_dog);
                            let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                            update_span_value(&span, &value, location!());

                            Ok(((expr, span), ty))
                        } else if let Some(func) = func {
                            let fat = FieldAccessTarget::new_function(true, &func, lu_dog);
                            let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                            let expr = Expression::new_field_access(true, &expr, lu_dog);
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
                                program: context.source_string.to_owned(),
                            }])
                        }
                    } else {
                        Err(vec![DwarfError::StructFieldNotFound {
                            field: rhs.0.clone(),
                            file: context.file_name.to_owned(),
                            span: rhs.1.to_owned(),
                            location: location!(),
                            program: context.source_string.to_owned(),
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
                                    let fat = FieldAccessTarget::new_field(true, &field, lu_dog);
                                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                                    let expr = Expression::new_field_access(true, &expr, lu_dog);
                                    let ty = s_read!(field).r5_value_type(lu_dog)[0].clone();
                                    let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                                    update_span_value(&span, &value, location!());

                                    Ok(((expr, span), ty))
                                } else if let Some(func) = func {
                                    let fat = FieldAccessTarget::new_function(true, &func, lu_dog);
                                    let expr = FieldAccess::new(&lhs.0, &fat, &woog_struct, lu_dog);
                                    let expr = Expression::new_field_access(true, &expr, lu_dog);
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
                                        program: context.source_string.to_owned(),
                                    }])
                                }
                            } else {
                                Err(vec![DwarfError::StructFieldNotFound {
                                    field: rhs.0.clone(),
                                    file: context.file_name.to_owned(),
                                    span: rhs.1.to_owned(),
                                    location: location!(),
                                    program: context.source_string.to_owned(),
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
                    program: context.source_string.to_owned(),
                }]),
            }
        }
        //
        // FloatLiteral
        //
        ParserExpression::FloatLiteral(literal) => {
            let expr = Expression::new_literal(
                true,
                &Literal::new_float_literal(true, &FloatLiteral::new(literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(true, &Ty::new_float(context.sarzak), lu_dog);
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
                inter_expression(&collection, cspan, block, context, context_stack, lu_dog)?;

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
                    // 🚧 Of course rust does not work on chars. Doesn't mean I don't want to.
                    ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog)
                }
                ValueTypeEnum::Ty(ref id) => {
                    let ty = context.sarzak.exhume_ty(id).unwrap();
                    let ty = ty.read().unwrap();
                    match &*ty {
                        Ty::ZString(_) => ValueType::new_char(true, lu_dog),
                        _ => {
                            let ty =
                                PrintableValueType(&collection_ty, context, lu_dog).to_string();
                            return Err(vec![DwarfError::NotAList {
                                file: context.file_name.to_owned(),
                                span: cspan.to_owned(),
                                ty,
                                location: location!(),
                                program: context.source_string.to_owned(),
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
                        program: context.source_string.to_owned(),
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

            let (body, _body_ty) =
                inter_expression(&body, bspan, block, context, context_stack, lu_dog)?;

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
            let body = Expression::new_block(true, &body, lu_dog);

            let for_loop = ForLoop::new(iter.0.to_owned(), &body, &collection.0, lu_dog);
            let expr = Expression::new_for_loop(true, &for_loop, lu_dog);
            let ty = ValueType::new_empty(true, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // FormatString
        //
        ParserExpression::FormatString(bits) => {
            let format_string = FormatString::new(None, lu_dog);
            let literal = Literal::new_format_string(true, &format_string, lu_dog);
            let expr = Expression::new_literal(true, &literal, lu_dog);
            let ty = ValueType::new_ty(true, &Ty::new_z_string(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            let mut last_format_bit_uuid: Option<SarzakStorePtr> = None;
            for (bit, span) in bits {
                let span = LuDogSpan::new(
                    span.end as i64,
                    span.start as i64,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );

                let format_bit = match bit {
                    ParserExpression::Addition(lhs, rhs) => {
                        let range_span = s_read!(span).start as usize..s_read!(span).end as usize;
                        let (lhs, ty) = inter_expression(
                            &new_ref!(ParserExpression, lhs.0.to_owned()),
                            &range_span,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;
                        let (rhs, _ty) = inter_expression(
                            &new_ref!(ParserExpression, rhs.0.to_owned()),
                            &range_span,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;

                        let expr = Binary::new_addition(true, lu_dog);
                        let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
                        let expr = Expression::new_operator(true, &expr, lu_dog);
                        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                        update_span_value(&span, &value, location!());

                        let expr_bit = ExpressionBit::new(&expr, lu_dog);
                        FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog)
                    }
                    ParserExpression::Index(target_p, index_p) => {
                        debug!("index {target_p:?}, {index_p:?}");
                        let (target, target_ty) = inter_expression(
                            &new_ref!(ParserExpression, target_p.0.to_owned()),
                            &target_p.1,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;
                        debug!("target: {target:?}, ty: {target_ty:?}");
                        let (index, index_ty) = inter_expression(
                            &new_ref!(ParserExpression, index_p.0.to_owned()),
                            &index_p.1,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;

                        let expr = Index::new(&index.0, &target.0, lu_dog);
                        let expr = Expression::new_index(true, &expr, lu_dog);
                        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                        update_span_value(&span, &value, location!());

                        let expr_bit = ExpressionBit::new(&expr, lu_dog);
                        FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog)
                    }
                    ParserExpression::LocalVariable(name) => {
                        let expr = VariableExpression::new(name.to_owned(), lu_dog);

                        let expr = Expression::new_variable_expression(true, &expr, lu_dog);
                        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                        update_span_value(&span, &value, location!());

                        let expr_bit = ExpressionBit::new(&expr, lu_dog);
                        FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog)
                    }
                    ParserExpression::MethodCall(instance, (ref method, meth_span), args) => {
                        let ((mc, _), _) = method_call::inter(
                            instance,
                            method,
                            meth_span,
                            args,
                            span,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;

                        let expr_bit = ExpressionBit::new(&mc, lu_dog);
                        FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog)
                    }
                    ParserExpression::Multiplication(lhs, rhs) => {
                        let range_span = s_read!(span).start as usize..s_read!(span).end as usize;
                        let (lhs, ty) = inter_expression(
                            &new_ref!(ParserExpression, lhs.0.to_owned()),
                            &range_span,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;
                        let (rhs, _ty) = inter_expression(
                            &new_ref!(ParserExpression, rhs.0.to_owned()),
                            &range_span,
                            block,
                            context,
                            context_stack,
                            lu_dog,
                        )?;

                        let expr = Binary::new_multiplication(true, lu_dog);
                        let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
                        let expr = Expression::new_operator(true, &expr, lu_dog);
                        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                        update_span_value(&span, &value, location!());

                        let expr_bit = ExpressionBit::new(&expr, lu_dog);
                        FormatBit::new_expression_bit(&format_string, None, &expr_bit, lu_dog)
                    }
                    ParserExpression::StringLiteral(string) => {
                        let literal = StringLiteral::new(string.to_owned(), lu_dog);
                        let expr = Expression::new_literal(
                            true,
                            &Literal::new_string_literal(true, &literal, lu_dog),
                            lu_dog,
                        );
                        let ty = ValueType::new_ty(true, &Ty::new_z_string(context.sarzak), lu_dog);
                        let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                        update_span_value(&span, &value, location!());

                        let string_bit = StringBit::new(&literal, lu_dog);
                        FormatBit::new_string_bit(&format_string, None, &string_bit, lu_dog)
                    }
                    huh => panic!("Unexpected expression: {huh:?}"),
                };
                if last_format_bit_uuid.is_none() {
                    s_write!(format_string).first_format_bit = Some(s_read!(format_bit).id);
                }
                last_format_bit_uuid = link_format_bits!(last_format_bit_uuid, format_bit, lu_dog);
            }

            Ok(((expr, span), ty))
        }
        //
        // FunctionCall
        //
        ParserExpression::FunctionCall(func, args) => {
            debug!("func {func:?}");
            let fspan = &func.1;
            let func = &func.0;
            debug!("args {args:?}");

            let (func_expr, ret_ty) = inter_expression(
                &new_ref!(ParserExpression, func.to_owned()),
                fspan,
                block,
                context,
                context_stack,
                lu_dog,
            )?;
            debug!("func_expr {func_expr:?}");

            let ret_ty = if let ValueTypeEnum::Unknown(_) = s_read!(ret_ty).subtype {
                match func {
                    ParserExpression::LocalVariable(name) => {
                        if let Some(defn) = context.func_defs.get(name) {
                            defn.return_type.clone()
                        } else {
                            return Err(vec![DwarfError::MissingFunctionDefinition {
                                span: fspan.to_owned(),
                                file: context.file_name.to_owned(),
                                program: context.source_string.to_owned(),
                            }]);
                        }
                    }
                    _ => {
                        return Err(vec![DwarfError::MissingFunctionDefinition {
                            span: fspan.to_owned(),
                            file: context.file_name.to_owned(),
                            program: context.source_string.to_owned(),
                        }]);
                    }
                }
            } else {
                ret_ty.clone()
            };

            let name = match func {
                ParserExpression::LocalVariable(name) => name,
                _ => "not-a-local-variable",
            };

            let func_call = FunctionCall::new(name.to_owned(), lu_dog);
            let func_call =
                Call::new_function_call(true, None, Some(&func_expr.0), &func_call, lu_dog);
            let func = Expression::new_call(true, &func_call, lu_dog);
            let value = XValue::new_expression(block, &ret_ty, &func, lu_dog);
            update_span_value(&span, &value, location!());

            let mut last_arg_uuid: Option<SarzakStorePtr> = None;
            // Note that position makes each arg unique. I don't remember if
            // that is the explicit intention or not.
            for (position, arg) in args.iter().enumerate() {
                let (arg_expr, _ty) = inter_expression(
                    &new_ref!(ParserExpression, arg.0.to_owned()),
                    &arg.1,
                    block,
                    context,
                    context_stack,
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Comparison::new_greater_than(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Comparison::new_greater_than_or_equal(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

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
                context_stack,
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
                inter_expression(&conditional, cspan, block, context, context_stack, lu_dog)?;
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
                    let bty = ValueType::new_ty(true, &Ty::new_boolean(context.sarzak), lu_dog);
                    let bty = PrintableValueType(&bty, context, lu_dog);
                    let ty = PrintableValueType(&conditional_ty, context, lu_dog);
                    return Err(vec![DwarfError::TypeMismatch {
                        expected: bty.to_string(),
                        found: ty.to_string(),
                        file: context.file_name.to_owned(),
                        expected_span: cspan.to_owned(),
                        found_span: cspan.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
            } else {
                let bty = ValueType::new_ty(true, &Ty::new_boolean(context.sarzak), lu_dog);
                let bty = PrintableValueType(&bty, context, lu_dog);
                let ty = PrintableValueType(&conditional_ty, context, lu_dog);
                return Err(vec![DwarfError::TypeMismatch {
                    expected: bty.to_string(),
                    found: ty.to_string(),
                    file: context.file_name.to_owned(),
                    expected_span: cspan.to_owned(),
                    found_span: cspan.to_owned(),
                    location: location!(),
                    program: context.source_string.to_owned(),
                }]);
            }

            let tspan = &true_block.1;
            let true_block = new_ref!(ParserExpression, true_block.0.to_owned());
            let (true_block, true_ty) =
                inter_expression(&true_block, tspan, block, context, context_stack, lu_dog)?;
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
                    inter_expression(&false_block, fspan, block, context, context_stack, lu_dog)?;
                Some(false_block.0)
            } else {
                None
            };

            let if_expr = XIf::new(false_block.as_ref(), &conditional.0, &true_block, lu_dog);
            let expr = Expression::new_x_if(true, &if_expr, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            debug!("target: {target:?}, ty: {target_ty:?}");
            let (index, index_ty) = inter_expression(
                &new_ref!(ParserExpression, index_p.0.to_owned()),
                &index_p.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            let int_ty = ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog);

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
                if let Ty::ZString(_) = &*ty {
                    ValueType::new_char(true, lu_dog)
                } else {
                    let ty = PrintableValueType(&target_ty, context, lu_dog).to_string();
                    return Err(vec![DwarfError::NotAList {
                        file: context.file_name.to_owned(),
                        span: target_p.1.clone(),
                        ty,
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }
            } else {
                let ty = PrintableValueType(&target_ty, context, lu_dog).to_string();
                return Err(vec![DwarfError::NotAList {
                    file: context.file_name.to_owned(),
                    span: target_p.1.clone(),
                    ty,
                    location: location!(),
                    program: context.source_string.to_owned(),
                }]);
            };

            let index = Index::new(&index.0, &target.0, lu_dog);

            let expr = Expression::new_index(true, &index, lu_dog);
            let value = XValue::new_expression(block, &target_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), target_ty))
        }
        //
        // IntegerLiteral
        //
        ParserExpression::IntegerLiteral(literal) => {
            let expr = Expression::new_literal(
                true,
                &Literal::new_integer_literal(true, &IntegerLiteral::new(literal, lu_dog), lu_dog),
                lu_dog,
            );
            let ty = ValueType::new_ty(true, &Ty::new_integer(context.sarzak), lu_dog);
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
                unreachable!("{:?}", body.0);
            };
            let block = Block::new(a_sink, Uuid::new_v4(), Some(block), None, lu_dog);
            let _body = Body::new_block(a_sink, &block, lu_dog);

            context.location = location!();
            let ret_ty = make_value_type(
                &return_type.0,
                &return_type.1,
                None,
                context,
                context_stack,
                lu_dog,
            )?;

            let lambda = Lambda::new(Some(&_body), None, &ret_ty, lu_dog);
            let _ = ValueType::new_lambda(true, &lambda, lu_dog);

            let mut errors = Vec::new();
            let mut last_param_uuid: Option<SarzakStorePtr> = None;
            for (position, ((param_name, name_span), (param_ty, ty_span))) in
                params.iter().enumerate()
            {
                debug!("param name {}", param_name);
                debug!("param ty {}", param_ty);

                context.location = location!();
                let param_ty = match make_value_type(
                    param_ty,
                    ty_span,
                    None,
                    context,
                    context_stack,
                    lu_dog,
                ) {
                    Ok(ty) => ty,
                    Err(mut e) => {
                        errors.append(&mut e);
                        continue;
                    }
                };
                debug!("param_ty {:?}", param_ty);

                let param = LambdaParameter::new(
                    position as DwarfInteger,
                    &lambda,
                    None,
                    Some(&param_ty),
                    lu_dog,
                );

                debug!("param {:?}", param);

                if position == 0 {
                    // 🚧 This should be marked as unsafe, as there is no type
                    // check happening.
                    s_write!(lambda).first_param = Some(s_read!(param).id);
                }

                let var = Variable::new_lambda_parameter(param_name.to_owned(), &param, lu_dog);
                debug!("var {:?}", var);
                // We need to introduce the values into the block, so that we don't
                // error out when parsing the statements.
                //
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
                inter_statements(&stmts, &body.1, &block, context, context_stack, lu_dog)?;

            typecheck(
                (&ret_ty, &return_type.1),
                (&block_ty, &block_span),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Expression::new_lambda(true, &lambda, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Comparison::new_less_than(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Comparison::new_less_than_or_equal(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // List
        //
        ParserExpression::List(ref elements) => {
            debug!("list {:?}", elements);
            if elements.is_empty() {
                // 🚧 Darn -- more of this and no comment. I think it's replaced someplace.
                // This has something to do with creating empty lists. This will cause
                // an error if the list initialization is not typed.
                let generic = FuncGeneric::new("UBER_HACK".to_owned(), None, None, lu_dog);
                let list = List::new(&ValueType::new_func_generic(true, &generic, lu_dog), lu_dog);
                let expr = Expression::new_list_expression(
                    true,
                    &ListExpression::new(None, lu_dog),
                    lu_dog,
                );
                let ty = ValueType::new_list(true, &list, lu_dog);
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
                    context_stack,
                    lu_dog,
                )?;

                let list = List::new(&first_ty, lu_dog);
                let element = ListElement::new(0, &first, None, lu_dog);
                let expr = Expression::new_list_element(true, &element, lu_dog);
                let value = XValue::new_expression(block, &first_ty, &expr, lu_dog);
                // We need to clone the span because it's already been used
                // by the underlying value.
                LuDogSpan::new(
                    s_read!(first_span).end,
                    s_read!(first_span).start,
                    &context.source,
                    None,
                    Some(&value),
                    lu_dog,
                );

                let list_expr = ListExpression::new(Some(&element), lu_dog);

                let mut last_element_uuid: Option<SarzakStorePtr> = Some(s_read!(element).id);
                let mut position = 1;
                for element in elements {
                    let ((elt, elt_span), elt_ty) = inter_expression(
                        &new_ref!(ParserExpression, element.0.to_owned()),
                        &element.1,
                        block,
                        context,
                        context_stack,
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
                    let expr = Expression::new_list_element(true, &element, lu_dog);
                    let value = XValue::new_expression(block, &elt_ty, &expr, lu_dog);
                    // We need to clone the span because it's already been used
                    // by the underlying value.
                    LuDogSpan::new(
                        s_read!(elt_span).end,
                        s_read!(elt_span).start,
                        &context.source,
                        None,
                        Some(&value),
                        lu_dog,
                    );
                }

                let expr = Expression::new_list_expression(true, &list_expr, lu_dog);
                let ty = ValueType::new_list(true, &list, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                Ok(((expr, span), ty))
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

            // Now search for a value that's a Variable, and see if the access matches
            // the variable.
            let mut expr_type_tuples = values
                .iter()
                .filter_map(|value| {
                    let value = &*s_read!(value);
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

                                        let ty_str =
                                            PrintableValueType(&ty, context, lu_dog);
                                        debug!("{name}, {}, {value:?} ({ty:?})", ty_str.to_string());

                                        let expr = lu_dog
                                            .iter_variable_expression()
                                            .find(|expr| s_read!(expr).name == *name);

                                        let expr = if let Some(expr) = expr {
                                            s_read!(expr).r15_expression(lu_dog)[0].clone()
                                        } else {
                                            let expr =
                                                VariableExpression::new(name.to_owned(), lu_dog);
                                            debug!("created a new variable expression {:?}", expr);
                                             Expression::new_variable_expression(true, &expr, lu_dog)
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

            debug!("expr_type_tuples {:?}", expr_type_tuples);

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
                let expr = Expression::new_variable_expression(true, &expr, lu_dog);

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                debug!("LocalVariable result ({expr:#?}, {ty:#?})");
                Ok(((expr, span), ty))
            } else {
                debug!("variable not found: `{name}`");
                let expr = VariableExpression::new(name.to_owned(), lu_dog);
                let expr = Expression::new_variable_expression(true, &expr, lu_dog);
                let ty = ValueType::new_unknown(true, lu_dog);
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
                context_stack,
                lu_dog,
            )?;

            let xmatch = XMatch::new(Uuid::new_v4(), &scrutinee.0, lu_dog);

            let mut first = true;
            let mut match_ty = ValueType::new_unknown(true, lu_dog);
            for ((pattern, match_expr), ref span) in patterns {
                debug!("pattern: {pattern:?}");
                let pattern_expr: ParserExpression = pattern.to_owned().into();
                let (pattern_expr, ty) = inter_expression(
                    &new_ref!(ParserExpression, pattern_expr),
                    span,
                    block,
                    context,
                    context_stack,
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
                    context_stack,
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

            let expr = Expression::new_x_match(true, &xmatch, lu_dog);

            let value = XValue::new_expression(block, &match_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), match_ty))
        }
        //
        // MethodCall
        //
        ParserExpression::MethodCall(instance, (ref method, meth_span), args) => {
            method_call::inter(
                instance,
                method,
                meth_span,
                args,
                span,
                block,
                context,
                context_stack,
                lu_dog,
            )
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
                context_stack,
                lu_dog,
            )?;
            let negation = Unary::new_negation(true, lu_dog);
            let operator = Operator::new_unary(&expr.0, None, &negation, lu_dog);
            let expr = Expression::new_operator(true, &operator, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = Comparison::new_not_equal(true, lu_dog);
            let expr = Operator::new_comparison(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let ty = Ty::new_boolean(context.sarzak);
            let ty = ValueType::new_ty(true, &ty, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            let ty = ValueType::new_empty(true, lu_dog);
            let print = XPrint::new(&expr.0, lu_dog);
            let expr = Expression::new_x_print(true, &print, lu_dog);
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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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

            let expr = Binary::new_multiplication(true, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            let (rhs, rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs_p.0.to_owned()),
                &rhs_p.1,
                block,
                context,
                context_stack,
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
                    program: context.source_string.to_owned(),
                }]);
            }

            typecheck(
                (&lhs_ty, &lhs_p.1),
                (&rhs_ty, &rhs_p.1),
                location!(),
                context,
                lu_dog,
            )?;

            let expr = BooleanOperator::new_or(true, lu_dog);
            let expr = Binary::new_boolean_operator(true, &expr, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

            let value = XValue::new_expression(block, &lhs_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), lhs_ty))
        }
        //
        // Path In Expression
        //
        // This is what you get when you use turbo-fish syntax on a function call.
        ParserExpression::PathInExpression(path) => {
            debug!("PathInExpression {:?}", path);

            //     // Lookup the type
            let root = path.first().unwrap();
            if let (Type::UserType((name, ut_span), generic), _) = root {
                let expr = lu_dog
                    .iter_variable_expression()
                    .find(|expr| s_read!(expr).name == *name);

                let expr = if let Some(expr) = expr {
                    s_read!(expr).r15_expression(lu_dog)[0].clone()
                } else {
                    let expr = VariableExpression::new(name.to_owned(), lu_dog);
                    debug!("created a new variable expression {:?}", expr);
                    Expression::new_variable_expression(true, &expr, lu_dog)
                };

                context.location = location!();
                let ty =
                    make_value_type(&generic[0].0, ut_span, None, context, context_stack, lu_dog)?;

                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                update_span_value(&span, &value, location!());

                debug!("expr {expr:?}, value {value:?}, type {ty:?}, span {span:?}");

                Ok(((expr, span.clone()), ty))
            } else {
                panic!("Things fell apart -- entropy happens.");
            }
        }
        //
        // Unit enumeration
        //
        ParserExpression::UnitEnum(enum_path, (field_name, field_span)) => unit_enum::inter(
            &enum_path, field_name, field_span, span, block, context, lu_dog,
        ),
        //
        // Range
        //
        ParserExpression::Range(start, end) => {
            let (start, start_ty) = inter_expression(
                &new_ref!(ParserExpression, start.0.to_owned()),
                &start.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;
            let (end, _end_ty) = inter_expression(
                &new_ref!(ParserExpression, end.0.to_owned()),
                &end.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            // 🚧 Typecheck the start and end types to make sure that they are
            // both ints.

            let range = RangeExpression::new_full(Some(&start.0), Some(&end.0), lu_dog);

            let expr = Expression::new_range_expression(true, &range, lu_dog);
            let value = XValue::new_expression(block, &start_ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ValueType::new_range(true, lu_dog)))
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
                    context_stack,
                    lu_dog,
                )?
            } else {
                // 🚧 Once we have tuples, I'd prefer to return the empty tuple.
                let expr = Expression::new_block(
                    true,
                    &Block::new(false, Uuid::new_v4(), None, None, lu_dog),
                    lu_dog,
                );
                let ty = ValueType::new_empty(true, lu_dog);
                let value = XValue::new_expression(block, &ty, &expr, lu_dog);
                cfg_if::cfg_if! {
                if #[cfg(not(feature="debug"))] {
                    // See # Span Bug
                        lu_dog.inter_span(|id| {
                            let mut span = s_read!(span).clone();
                            span.x_value = Some(s_read!(value).id);
                            span.id = id;
                            new_ref!(LuDogSpan, span)
                        });
                        // update_span_value(&span, &value, location!());
                    } else {
                        let span = LuDogSpan::new(
                            s_read!(span).end,
                            s_read!(span).start,
                            &context.source,
                            None,
                            Some(&value),
                            lu_dog,
                        );
                    }
                }
                ((expr, span.clone()), ty)
            };

            let ret = XReturn::new(&expr.0, lu_dog);
            let expr = Expression::new_x_return(true, &ret, lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // StaticMethodCall
        //
        ParserExpression::StaticMethodCall(ref path, (ref method, _), ref params) => {
            static_method_call::inter(
                path,
                method,
                span,
                params,
                block,
                context,
                context_stack,
                lu_dog,
            )
        }
        //
        // StringLiteral
        //
        ParserExpression::StringLiteral(literal) => {
            debug!("literal {:?}", literal);
            let expr = Expression::new_literal(
                true,
                &Literal::new_string_literal(
                    true,
                    &StringLiteral::new(literal.to_owned(), lu_dog),
                    lu_dog,
                ),
                lu_dog,
            );
            let ty = ValueType::new_ty(true, &Ty::new_z_string(context.sarzak), lu_dog);
            let value = XValue::new_expression(block, &ty, &expr, lu_dog);
            update_span_value(&span, &value, location!());

            Ok(((expr, span), ty))
        }
        //
        // Struct
        //
        ParserExpression::Struct(name, fields) => {
            let name_span = &name.1;
            let (_path, base, name) = if let ParserExpression::LocalVariable(obj) = &name.0 {
                (PATH_ROOT.to_owned(), obj.to_owned(), obj.to_owned())
            } else if let ParserExpression::PathInExpression(types) = &name.0 {
                let mut path = String::new();
                let mut base = String::new();
                let mut name = String::new();

                for (i, ty) in types.iter().enumerate() {
                    if let Type::UserType((type_name, _), generics) = &ty.0 {
                        name.extend([type_name.as_str()]);
                        if i != types.len() - 1 {
                            path.extend([type_name.as_str()]);
                            path.extend(["::"]);
                            name.extend(["::"]);
                        }
                        if i == types.len() - 1 {
                            base = type_name.clone();
                        }
                        if !generics.is_empty() {
                            name.push('<');
                            for (i, (generic, _)) in generics.iter().enumerate() {
                                // if let Type::Generic((generic_name, _)) = generic {
                                name.extend([generic.to_string()]);

                                if i != generics.len() - 1 {
                                    name.extend([", "]);
                                }
                                // }
                            }
                            name.push('>');
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

                (path, base, name)
            } else {
                return Err(vec![DwarfError::Internal {
                    description: format!(
                        "Expected a local variable in struct expression, found {:?}",
                        name.0
                    ),
                    location: location!(),
                }]);
            };

            let base = if let Some(path) = context.scopes.get(&name) {
                path.to_owned() + name.as_str()
            } else {
                context.path.clone() + base.as_str()
            };

            debug!("ParserExpression::Struct {}", name);

            // 🚧 Base or name? This is black magic at this point.
            let id = match lu_dog.exhume_woog_struct_id_by_name(&base) {
                Some(id) => id,
                None => match lu_dog.exhume_woog_struct_id_by_name(&name) {
                    Some(id) => id,
                    None => {
                        return Err(vec![DwarfError::UnknownType {
                            ty: name.to_owned(),
                            file: context.file_name.to_owned(),
                            span: name_span.to_owned(),
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }]);
                    }
                },
            };

            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
            let struct_fields = s_read!(woog_struct).r7_field(lu_dog);

            let data_struct = DataStructure::new_woog_struct(true, &woog_struct, lu_dog);

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
                    context_stack,
                    lu_dog,
                )?;

                let pvt = PrintableValueType(&ty, context, lu_dog).to_string();

                debug!(
                    "field `{}` is of type `{pvt}`, expr: {field_expr:?}",
                    field_name.0
                );

                if let Some(field) = struct_fields
                    .iter()
                    .find(|f| s_read!(f).name == field_name.0)
                {
                    let field_ty = lu_dog.exhume_value_type(&s_read!(field).ty).unwrap();
                    let naked_ty = s_read!(field_ty);

                    // Primarily we are here to check the type of the field against
                    // the type of the expression. If only it were so easily done.
                    // The issue is generics. If the field is generic, then we need
                    // to initially use the type of the expression to fill in the
                    // type of the generic parameter. *After* that however, we
                    // need to use this new type to typecheck all subsequent uses
                    // of the pattern.
                    if let ValueTypeEnum::StructGeneric(ref id) = naked_ty.subtype {
                        // OK. We are instantiating a generic. We need to create the new type
                        let generic = lu_dog.exhume_struct_generic(id).unwrap();
                        generic_substitutions.insert(s_read!(generic).name.to_owned(), ty.clone());
                    } else {
                        // We only need the type check if the type of the field in the struct
                        // is not generic. Otherwise we are explicitly defining the type above.
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
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }]);
                }

                let nfe = NamedFieldExpression::new(field_name.0.to_owned(), lu_dog);
                let field = FieldExpression::new_named_field_expression(
                    &field_expr.0,
                    &struct_expr,
                    &nfe,
                    lu_dog,
                );

                let expr = Expression::new_field_expression(true, &field, lu_dog);
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
                    cfg_if::cfg_if! {
                        if #[cfg(not(feature="debug"))] {
                            lu_dog.inter_span(|id| {
                                let mut span = s_read!(field_expr.1).clone();
                                span.x_value = Some(s_read!(value).id);
                                span.id = id;
                                new_ref!(LuDogSpan, span)
                            });
                        } else {
                            let span = LuDogSpan::new(
                                s_read!(field_expr.1).end,
                                s_read!(field_expr.1).start,
                                &context.source,
                                None,
                                Some(&value),
                                lu_dog,
                            );
                        }
                    }
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
            let expr = Expression::new_struct_expression(true, &struct_expr, lu_dog);
            let ty = ValueType::new_woog_struct(true, &woog_struct, lu_dog);

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
                context_stack,
                lu_dog,
            )?;
            let (rhs, _rhs_ty) = inter_expression(
                &new_ref!(ParserExpression, rhs.0.to_owned()),
                &rhs.1,
                block,
                context,
                context_stack,
                lu_dog,
            )?;

            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧
            // 🚧                        THIS IS SUPER IMPORTANT!
            // 🚧
            // 🚧 We need to check the types of the LHS and RHS to make sure that they are the same.
            // 🚧 We also need to check that the type supports subtraction.
            // 🚧
            // 🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧🚧

            let expr = Binary::new_subtraction(true, lu_dog);
            let expr = Operator::new_binary(&lhs.0, Some(&rhs.0), &expr, lu_dog);
            let expr = Expression::new_operator(true, &expr, lu_dog);

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

fn inter_module(
    name: &str,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_module: {name}");

    let mut errors = Vec::new();

    let mut path = context.cwd.clone();
    path.push("sacrifice");
    path.set_file_name(name);
    path.set_extension(TAO_EXT);

    if !context.imports.insert(path.clone()) {
        return Ok(());
    }

    match fs::read_to_string(&path) {
        Ok(source_code) => {
            // parse, and extrude the dwarf file
            match parse_dwarf(path.to_str().unwrap(), &source_code) {
                Ok(ast) => {
                    let path_name = format!("{}", path.display());

                    let mut type_path = context.path.clone();
                    type_path += name;
                    type_path += PATH_SEP;

                    // let mut scopes = context.scopes.clone();
                    let mut scopes = HashMap::default();

                    let mut dirty = Vec::new();

                    let mut new_ctx = Context::new(
                        source_code,
                        context.sarzak,
                        &path_name,
                        context.cwd.clone(),
                        context.dwarf_home,
                        context.models,
                        &mut dirty,
                        location!(),
                        lu_dog,
                        type_path,
                        &mut scopes,
                        context.imports,
                    );

                    // Extrusion time
                    trace!("processing dwarf import");
                    walk_tree(&ast, &mut new_ctx, context_stack, lu_dog)?;
                    trace!("done processing dwarf import");

                    context.dirty.extend(dirty);
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
    import_path: &[Spanned<String>],
    alias: &Option<(String, Range<usize>)>,
    context: &mut Context,
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    debug!("inter_import: {import_path:?}");
    let mut errors = Vec::new();

    let mut path_root = import_path
        .iter()
        .map(|p| p.0.to_owned())
        .collect::<Vec<_>>();

    let ty = path_root.pop().unwrap();
    let module = path_root.first().unwrap(); // This will have _something_.

    // It looks like we are first trying to load an extension.
    let mut path = context.dwarf_home.clone();
    path.push(EXTENSION_DIR);
    path.push(module);
    path.push(SRC_DIR);
    let dir = path.clone();

    path.push(LIB_TAO);

    // And then here if the extension doesn't exist, we try the lib dir.
    let (dir, path) = if path.exists() {
        (dir, path)
    } else {
        let mut path = context.dwarf_home.clone();
        path.push(LIB_DIR);
        path.push(module);
        let dir = path.clone();

        path.push(LIB_TAO);
        (dir, path)
    };

    match fs::read_to_string(&path) {
        Ok(source_code) => {
            // parse, and extrude the dwarf file
            match parse_dwarf(path.to_str().unwrap(), &source_code) {
                Ok(ast) => {
                    let path = format!("{}", path.display());
                    let mut dirty = Vec::new();
                    // let mut scopes = context.scopes.clone();
                    let mut scopes = HashMap::default();

                    let mut new_ctx = Context::new(
                        source_code,
                        context.sarzak,
                        &path,
                        dir,
                        context.dwarf_home,
                        context.models,
                        &mut dirty,
                        location!(),
                        lu_dog,
                        format!("::{module}::"),
                        &mut scopes,
                        context.imports,
                    );

                    // Extrusion time
                    trace!("processing dwarf import");
                    walk_tree(&ast, &mut new_ctx, context_stack, lu_dog)?;
                    trace!("done processing dwarf import");

                    context.dirty.extend(dirty);
                    context.scopes.insert(
                        ty.clone(),
                        PATH_SEP.to_owned() + path_root.join(PATH_SEP).as_str() + PATH_SEP,
                    );
                }
                Err(_) => {
                    e_warn!("Failed to parse import: {path:?}");
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

    let (alias, has_alias) = match alias {
        Some((alias, _)) => (alias.to_owned(), true),
        None => ("".to_owned(), false),
    };

    let import = Import::new(
        alias,
        has_alias,
        ty,
        path.into_os_string().into_string().unwrap(),
        None,
        lu_dog,
    );
    debug!("import {import:?}");

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
    context_stack: &mut Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<()> {
    // 🚧 I'm not sure if I should look this up or force it.
    let name = context.path.clone() + name;

    debug!("inter_implementation: {name}");

    let (impl_ty, implementation) = if let Some(store_vec) = attributes.get(STORE) {
        if let Some((_, InnerAttribute::Attribute(ref attributes))) = store_vec.first() {
            if let Some(model_vec) = attributes.get(MODEL) {
                if let Some((_, ref value)) = model_vec.first() {
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
                            context_stack,
                            lu_dog,
                        )?;
                        (ty, None)
                    };

                    let implementation =
                        ImplementationBlock::new(None, None, store.as_ref(), lu_dog);
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
            context_stack,
            lu_dog,
        )?;

        context.in_impl = name.to_owned();

        let implementation = if let Some(id) = lu_dog.exhume_woog_struct_id_by_name(&name) {
            let woog_struct = lu_dog.exhume_woog_struct(&id).unwrap();
            ImplementationBlock::new(None, Some(&woog_struct), None, lu_dog)
        } else if let Some(id) = lu_dog.exhume_enumeration_id_by_name(&name) {
            // OMG this is ugly.
            // 🚧 Fix the model.
            let woog_enum = lu_dog.exhume_enumeration(&id).unwrap();
            ImplementationBlock::new(Some(&woog_enum), None, None, lu_dog)
        } else {
            return Err(vec![DwarfError::ObjectNameNotFound {
                name: name.to_owned(),
                file: context.file_name.to_owned(),
                span: span.to_owned(),
                location: location!(),
                program: context.source_string.to_owned(),
            }]);
        };

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
                        InnerItem::Function {
                            a_sink,
                            name,
                            params,
                            return_type,
                            generics,
                            statements,
                        },
                        span,
                    ),
                attributes,
            } => {
                let generics = if let Some((generics, _)) = generics {
                    Some(
                        generics
                            .iter()
                            .cloned()
                            .map(|(t, _)| match t {
                                Type::Generic((t, s)) => (t.to_owned(), Type::Generic((t, s))),
                                fubared => {
                                    dbg!(fubared);
                                    unreachable!();
                                }
                            })
                            .collect(),
                    )
                } else {
                    None
                };

                match func::inter_func(
                    a_sink,
                    &name.0,
                    attributes,
                    params,
                    return_type,
                    generics.as_ref(),
                    statements.as_ref(),
                    implementation.as_ref(),
                    impl_ty.as_ref(),
                    span,
                    context,
                    context_stack,
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
                    program: context.source_string.to_owned(),
                }])
            }
        }
    }

    context.in_impl = "".to_owned();

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
                        if let ValueTypeEnum::EnumGeneric(_) = ty.subtype {
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

pub(super) const FUNC: &str = "func";
pub(super) const MODEL: &str = "model";
pub(super) const OBJECT: &str = "object";
pub(super) const PROXY: &str = "proxy";
pub(super) const PLUGIN: &str = "plugin";
pub(super) const STORE: &str = "store";
pub(super) const TYPE: &str = "ty";

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
    context: &mut Context,
    context_stack: &Vec<(String, RefType<LuDogStore>)>,
    lu_dog: &mut LuDogStore,
) -> Result<RefType<ValueType>> {
    let sarzak = context.sarzak;

    debug!("make_value_type {type_:?}");

    match type_ {
        Type::Boolean => {
            let ty = Ty::new_boolean(sarzak);
            Ok(ValueType::new_ty(true, &ty, lu_dog))
        }
        Type::Char => Ok(ValueType::new_char(true, lu_dog)),
        Type::Empty => Ok(ValueType::new_empty(true, lu_dog)),
        Type::Float => {
            let ty = Ty::new_float(sarzak);
            Ok(ValueType::new_ty(true, &ty, lu_dog))
        }
        Type::Fn(ref params, ref return_type) => {
            let return_type = make_value_type(
                &return_type.0,
                span,
                enclosing_type,
                context,
                context_stack,
                lu_dog,
            )?;
            let lambda = Lambda::new(None, None, &return_type, lu_dog);

            let mut last_param_uuid: Option<SarzakStorePtr> = None;
            for (position, (param_ty, param_span)) in params.iter().enumerate() {
                let param_ty =
                    make_value_type(param_ty, param_span, None, context, context_stack, lu_dog)?;
                debug!("param_ty {:?}", param_ty);

                let param = LambdaParameter::new(
                    position as DwarfInteger,
                    &lambda,
                    None,
                    Some(&param_ty),
                    lu_dog,
                );
                debug!("param {:?}", param);

                if position == 0 {
                    // 🚧 This should be marked as unsafe, as there is no type
                    // check happening.
                    s_write!(lambda).first_param = Some(s_read!(param).id);
                }

                last_param_uuid = link_ƛ_parameter!(last_param_uuid, param, lu_dog);
            }

            let lambda = ValueType::new_lambda(true, &lambda, lu_dog);
            Ok(lambda)
        }
        Type::Generic((_, _)) => {
            panic!("don't call this function with a generic -- you wont't get anywhere.");
        }
        Type::Integer => {
            let ty = Ty::new_integer(sarzak);
            Ok(ValueType::new_ty(true, &ty, lu_dog))
        }
        Type::List(ref type_) => {
            let inner_type = make_value_type(
                &type_.0,
                &type_.1,
                enclosing_type,
                context,
                context_stack,
                lu_dog,
            )?;
            let list = List::new(&inner_type, lu_dog);
            Ok(ValueType::new_list(true, &list, lu_dog))
        }
        Type::Self_ => match enclosing_type {
            Some(ty) => Ok(ty.clone()),
            None => Err(vec![DwarfError::BadSelf {
                file: context.file_name.to_owned(),
                span: span.clone(),
                location: context.location,
                program: context.source_string.to_owned(),
            }]),
        },
        Type::String => {
            let ty = Ty::new_z_string(sarzak);
            Ok(ValueType::new_ty(true, &ty, lu_dog))
        }
        Type::UserType(tok, generics) => {
            let name = &tok.0;

            if name == "Future" {
                let inner_type = if let Type::Generic((name, span)) = &generics[0].0 {
                    let name = if let Some(path) = context.scopes.get(name) {
                        path.to_owned() + name.as_str()
                    } else {
                        context.path.clone() + name.as_str()
                    };

                    if let Some(ty) = lookup_user_defined_type(lu_dog, &name, span, context) {
                        ty
                    } else {
                        let name = if let Some(name) = name.strip_prefix(PATH_SEP) {
                            name.to_owned()
                        } else {
                            name.to_owned()
                        };
                        return Err(vec![DwarfError::UnknownType {
                            ty: name,
                            file: context.file_name.to_owned(),
                            span: span.to_owned(),
                            location: location!(),
                            program: context.source_string.to_owned(),
                        }]);
                    }
                } else {
                    make_value_type(
                        &generics[0].0,
                        span,
                        enclosing_type,
                        context,
                        context_stack,
                        lu_dog,
                    )?
                };
                let future = XFuture::new(&inner_type, lu_dog);

                Ok(ValueType::new_x_future(true, &future, lu_dog))
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
                        program: context.source_string.to_owned(),
                    }]),
                }
            } else if name == "String" {
                Ok(ValueType::new_ty(true, &Ty::new_z_string(sarzak), lu_dog))
            } else if name == UUID_TYPE || name == FQ_UUID_TYPE {
                Ok(ValueType::new_ty(true, &Ty::new_z_uuid(sarzak), lu_dog))
            } else {
                // 🚧 HashMapFix
                // for model in context.models.values() {
                //     // Look for the Object in the model domains first.
                //     if let Some(ty) = model.0.iter_ty().find(|ty| match &*ty.read().unwrap() {
                //         Ty::Object(ref obj) => {
                //             let obj = model.0.exhume_object(obj).unwrap();
                //             // We are going to cheat a little bit here. Say we have an
                //             // object called `Point`. We want to be able to also handle
                //             // proxy objects for `Point`. Those are suffixed with "Proxy".
                //             let obj = obj.read().unwrap().name.to_upper_camel_case();
                //             obj == *name || name == format!("{}Proxy", obj).as_str()
                //         }
                //         _ => false,
                //     }) {
                //         return Ok(ValueType::new_ty(true, &ty, lu_dog));
                //     }
                // }

                // This feels sort of dirty. Sometimes the name has leading `::`, and
                // sometimes it does not. We don't want it here because below we
                // concatenate the name and the path and the path has a trailing `::`.
                let fq_name = if let Some(name) = tok.0.split(PATH_SEP).last() {
                    name.to_owned()
                } else {
                    tok.0.clone()
                };

                let mut fq_name = if let Some(path) = context.scopes.get(&fq_name) {
                    path.to_owned() + fq_name.as_str()
                } else {
                    context.path.clone() + fq_name.as_str()
                };

                let name = fq_name.clone();

                if !generics.is_empty() {
                    fq_name.push('<');
                    for (i, (generic, _)) in generics.iter().enumerate() {
                        fq_name.extend([generic.to_string()]);

                        if i != generics.len() - 1 {
                            fq_name.extend([", "]);
                        }
                    }
                    fq_name.push('>');
                }

                if let Some(ty) = lookup_user_defined_type(lu_dog, &fq_name, span, context) {
                    // 🔥 I think that I need to look at the returned type and see if it has
                    // generics, and then match them up with the generics I have above. But
                    // then what happens? I can't really return a new type with the substitutions
                    // I don't think.
                    Ok(ty)
                } else if fq_name != name {
                    // 🚧  I don't trust this code -- it needs testing.
                    if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(&name) {
                        let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
                        let struct_fields = s_read!(woog_struct).r7_field(lu_dog);
                        let mut generic_substitutions = HashMap::default();
                        let mut i = 0;

                        for field in struct_fields {
                            let field = s_read!(field);
                            let field_ty = lu_dog.exhume_value_type(&field.ty).unwrap();
                            let field_ty = s_read!(field_ty);
                            if let ValueTypeEnum::StructGeneric(ref id) = field_ty.subtype {
                                let generic = lu_dog.exhume_struct_generic(id).unwrap();
                                generic_substitutions.insert(
                                    s_read!(generic).name.to_owned(),
                                    make_value_type(
                                        &generics[i].0,
                                        span,
                                        enclosing_type,
                                        context,
                                        context_stack,
                                        lu_dog,
                                    )?,
                                );
                                i += 1;
                            }
                        }
                        let generic = create_generic_struct(
                            &woog_struct,
                            &generic_substitutions,
                            context,
                            context.sarzak,
                            lu_dog,
                        );
                        Ok(ValueType::new_woog_struct(true, &generic, lu_dog))
                    } else {
                        Ok(
                            create_generic_enum(&fq_name, &name, span.to_owned(), context, lu_dog)?
                                .1,
                        )
                    }
                } else if let Some(ty) = lookup_user_defined_type(lu_dog, &name, span, context) {
                    Ok(ty)
                } else if let Some(ref id) = lu_dog.exhume_z_object_store_id_by_name(&name) {
                    let store = lu_dog.exhume_z_object_store(id).unwrap();
                    Ok(ValueType::new_z_object_store(true, &store, lu_dog))
                } else if let Some(ty) = sarzak.iter_ty().find(|ty| match &*ty.read().unwrap() {
                    // Unlikely to have to reach back this far.
                    Ty::Object(ref obj) => {
                        let obj = sarzak.exhume_object(obj).unwrap();
                        let obj = obj.read().unwrap().name.to_upper_camel_case();
                        obj == *name || name == format!("{}Proxy", obj).as_str()
                    }
                    _ => false,
                }) {
                    Ok(ValueType::new_ty(true, &ty, lu_dog))
                } else {
                    Err(vec![DwarfError::UnknownType {
                        ty: fq_name.strip_prefix(PATH_SEP).unwrap().to_owned(),
                        file: context.file_name.to_owned(),
                        span: span.to_owned(),
                        location: context.location,
                        program: context.source_string.to_owned(),
                    }])
                }
            }
        }
        Type::Uuid => {
            let ty = Ty::new_z_uuid(sarzak);
            Ok(ValueType::new_ty(true, &ty, lu_dog))
        }
        道 => todo!("get_value_type missing implementation for {:?}", 道),
    }
}

pub(crate) fn lookup_user_defined_type(
    lu_dog: &mut LuDogStore,
    name: &str,
    span: &Span,
    context: &Context,
) -> Option<RefType<ValueType>> {
    if let Some(ref id) = lu_dog.exhume_woog_struct_id_by_name(name) {
        // Here is where we look for actual user defined types, as
        // in types that are defined in dwarf source.
        let woog_struct = lu_dog.exhume_woog_struct(id).unwrap();
        let ty = ValueType::new_woog_struct(true, &woog_struct, lu_dog);
        LuDogSpan::new(
            span.end as i64,
            span.start as i64,
            &context.source,
            Some(&ty),
            None,
            lu_dog,
        );
        Some(ty)
    } else if let Some(ref id) = lu_dog.exhume_enumeration_id_by_name(name) {
        // Here too, but for enums.
        let woog_enum = lu_dog.exhume_enumeration(id).unwrap();
        Some(ValueType::new_enumeration(true, &woog_enum, lu_dog))
        // 🚧 Don't we need a span here, like above?
    } else {
        None
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
            debug!("type not found");
            e_warn!("Unknown type for variable {method}");
            Some(ValueType::new_unknown(true, lu_dog))
        };
        debug!("found type: {ty:?}");
        if let Some(ty) = ty {
            ty
        } else {
            e_warn!("Unknown type for variable {method}");
            ValueType::new_unknown(true, lu_dog)
        }
    } else if type_name == CHACHA {
        match method {
            ARGS => {
                let ty = Ty::new_z_string(sarzak);
                // 🚧 Ideally we'd cache this when we startup.
                let ty = lu_dog
                    .iter_value_type()
                    .find(|t| s_read!(t).subtype == ValueTypeEnum::Ty(ty.read().unwrap().id()))
                    .unwrap();
                let list = List::new(&ty, lu_dog);
                ValueType::new_list(true, &list, lu_dog)
            }
            _ => {
                e_warn!("ParserExpression type not found");
                ValueType::new_unknown(true, lu_dog)
            }
        }
    } else if (type_name == UUID_TYPE || type_name == FQ_UUID_TYPE) && method == FN_NEW {
        ValueType::new_ty(true, &Ty::new_z_uuid(sarzak), lu_dog)
    } else {
        e_warn!("ParserExpression type not found: {type_name}");
        ValueType::new_unknown(true, lu_dog)
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
        if #[cfg(any(feature = "single", feature = "single-vec", feature = "single-vec-tracy", feature="debug"))] {
            if std::rc::Rc::as_ptr(lhs) == std::rc::Rc::as_ptr(rhs) {
                return Ok(());
            }
        } else {
            if std::sync::Arc::as_ptr(lhs) == std::sync::Arc::as_ptr(rhs) {
                return Ok(());
            }
        }
    }

    // 🚧 The way that I'm doing this is unwieldy. I'm checking left and right, and
    // position matters, so I need it to be symmetric, without all the extra code.
    // Symmetry, right? Maybe a macro? We are also diving into sarzak in three
    // different places? Four?
    match (&s_read!(lhs).subtype, &s_read!(rhs).subtype) {
        // Promote unknown to the other type.
        (ValueTypeEnum::Unknown(_), _) => Ok(()),
        (_, ValueTypeEnum::Unknown(_)) => Ok(()),
        (ValueTypeEnum::XPlugin(a), ValueTypeEnum::XPlugin(b)) => {
            let a = lu_dog.exhume_x_plugin(a).unwrap();
            let b = lu_dog.exhume_x_plugin(b).unwrap();
            let a = s_read!(a);
            let b = s_read!(b);
            if a.name == b.name {
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
                    program: context.source_string.to_owned(),
                }])
            }
        }
        (ValueTypeEnum::FuncGeneric(_), _) => Ok(()),
        (_, ValueTypeEnum::FuncGeneric(_)) => Ok(()),
        (ValueTypeEnum::EnumGeneric(_g), _) => {
            // let g = lu_dog.exhume_enum_generic(g).unwrap();
            // let ty = s_read!(g).r99_value_type(lu_dog);
            // dbg!(&ty, "a");

            // if !ty.is_empty() {
            //     typecheck(
            //         (&ty[0], lhs_span),
            //         (rhs, rhs_span),
            //         location,
            //         context,
            //         lu_dog,
            //     )
            // } else {
            // 🚧 I'd really much prefer to do something clever here.
            // Why would we get here? If there is no value type for the generic?
            // When would that happen?
            Ok(())
            // }
        }
        (_, ValueTypeEnum::EnumGeneric(_g)) => {
            // let g = lu_dog.exhume_enum_generic(g).unwrap();
            // let ty = s_read!(g).r99_value_type(lu_dog);
            // dbg!(&ty, "b");
            // let a = PrintableValueType(lhs, context, lu_dog);
            // let b = PrintableValueType(rhs, context, lu_dog);

            // dbg!(a.to_string(), b.to_string());

            // if !ty.is_empty() {
            //     typecheck(
            //         (lhs, lhs_span),
            //         (&ty[0], rhs_span),
            //         location,
            //         context,
            //         lu_dog,
            //     )
            // } else {
            Ok(())
            // }
        }
        (ValueTypeEnum::StructGeneric(_g), _) => {
            // let g = lu_dog.exhume_generic(g).unwrap();
            // let ty = s_read!(g).r99_value_type(lu_dog);
            // dbg!(&ty, "a");

            // if !ty.is_empty() {
            //     typecheck(
            //         (&ty[0], lhs_span),
            //         (rhs, rhs_span),
            //         location,
            //         context,
            //         lu_dog,
            //     )
            // } else {
            // 🚧 I'd really much prefer to do something clever here.
            // Why would we get here? If there is no value type for the generic?
            // When would that happen?
            Ok(())
            // }
        }
        (_, ValueTypeEnum::StructGeneric(_g)) => {
            // let g = lu_dog.exhume_generic(g).unwrap();
            // let ty = s_read!(g).r99_value_type(lu_dog);
            // dbg!(&ty, "b");
            // let a = PrintableValueType(lhs, context, lu_dog);
            // let b = PrintableValueType(rhs, context, lu_dog);

            // dbg!(a.to_string(), b.to_string());

            // if !ty.is_empty() {
            //     typecheck(
            //         (lhs, lhs_span),
            //         (&ty[0], rhs_span),
            //         location,
            //         context,
            //         lu_dog,
            //     )
            // } else {
            Ok(())
            // }
        }
        (ValueTypeEnum::List(a), ValueTypeEnum::List(b)) => {
            let a = lu_dog.exhume_list(a).unwrap();
            let b = lu_dog.exhume_list(b).unwrap();
            let a = s_read!(a);
            let b = s_read!(b);
            let a = lu_dog.exhume_value_type(&a.ty).unwrap();
            let b = lu_dog.exhume_value_type(&b.ty).unwrap();
            typecheck((&a, lhs_span), (&b, rhs_span), location, context, lu_dog)
        }
        (ValueTypeEnum::Ty(a), ValueTypeEnum::Ty(b)) => {
            let a = context.sarzak.exhume_ty(a).unwrap();
            let b = context.sarzak.exhume_ty(b).unwrap();
            let a = a.read().unwrap();
            let b = b.read().unwrap();
            match (&*a, &*b) {
                (Ty::Integer(_), Ty::Integer(_)) => Ok(()),
                (Ty::Float(_), Ty::Float(_)) => Ok(()),
                (Ty::Boolean(_), Ty::Boolean(_)) => Ok(()),
                (Ty::ZString(_), Ty::ZString(_)) => Ok(()),
                (Ty::ZUuid(_), Ty::ZUuid(_)) => Ok(()),
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
                            program: context.source_string.to_owned(),
                        }])
                    }
                }
            }
        }
        // (ValueTypeEnum::Unknown(_), _) => Ok(()),
        // 🚧 Related to range_type_bug.
        (ValueTypeEnum::Ty(id), ValueTypeEnum::Range(_)) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::Integer(_) => Ok(()),
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
                        program: context.source_string.to_owned(),
                    }])
                }
            }
        }
        (ValueTypeEnum::Range(_), ValueTypeEnum::Ty(id)) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::Integer(_) => Ok(()),
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
                        program: context.source_string.to_owned(),
                    }])
                }
            }
        }
        (ValueTypeEnum::Char(_), ValueTypeEnum::Ty(id)) => {
            let ty = context.sarzak.exhume_ty(id).unwrap();
            let ty = ty.read().unwrap();
            match &*ty {
                Ty::ZString(_) => Ok(()),
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
                        program: context.source_string.to_owned(),
                    }])
                }
            }
        }
        (ValueTypeEnum::Enumeration(a), ValueTypeEnum::Enumeration(b)) => {
            let a = lu_dog.exhume_enumeration(a).unwrap();
            let b = lu_dog.exhume_enumeration(b).unwrap();
            let a = s_read!(a);
            let b = s_read!(b);

            let a_name = if let Some(next) = a.name.split('<').next() {
                next
            } else {
                &a.name
            };
            let b_name = if let Some(next) = b.name.split('<').next() {
                next
            } else {
                &b.name
            };

            if a_name == b_name {
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
                    program: context.source_string.to_owned(),
                }])
            }
        }
        (lhs_t, rhs_t) => {
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
                    program: context.source_string.to_owned(),
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
    let woog_struct = s_read!(woog_struct);
    let mut name = woog_struct.name.to_owned();
    name.push('<');
    let first = woog_struct.r102_struct_generic(lu_dog)[0].clone();
    name.push_str(&s_read!(first).name);
    let mut id = s_read!(first).next;
    while let Some(next_id) = id {
        let next = lu_dog.exhume_struct_generic(&next_id).unwrap();
        let next = s_read!(next);
        id = next.next;

        let ty = substitutions.get(&next.name).unwrap();
        let ty = PrintableValueType(ty, context, lu_dog).to_string();

        name.extend([", ", &ty]);
    }
    name.push('>');

    let mut obj = woog_struct.r4_object(sarzak);
    let obj = if !obj.is_empty() {
        let obj = obj.pop().unwrap();
        // Hey uber, don't change this.
        let obj = obj.read().unwrap().clone();
        Some(obj)
    } else {
        None
    };

    let new_struct = WoogStruct::new(
        name.to_owned(),
        context.path.clone(),
        None,
        obj.as_ref(),
        lu_dog,
    );
    context.dirty.push(Dirty::Struct(new_struct.clone()));
    let _ = ValueType::new_woog_struct(true, &new_struct, lu_dog);
    for field in woog_struct.r7_field(lu_dog) {
        let field = s_read!(field);
        let ty = &field.r5_value_type(lu_dog)[0];
        let _ = Field::new(field.name.to_owned(), &new_struct, ty, lu_dog);
    }

    new_struct
}

pub(crate) fn update_span_value(
    span: &RefType<LuDogSpan>,
    value: &RefType<XValue>,
    location: Location,
) {
    trace!(
        "update span {}:{}:{} -- {value:?}",
        location.file,
        location.line,
        location.column
    );
    s_write!(span).x_value = Some(s_read!(value).id);
    trace!("update span: {:?}", s_read!(span));
}
