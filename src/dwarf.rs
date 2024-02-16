//! The Dwarf Language
//!
//! 🚧 Something that bothers me is that we are parsing a string into a bunch
//! of data structures that own their string-based values. I'd much rather use
//! references to the original input. I suppose I didn't do it that way originally
//! to avoid the lifetime noise? I dunno. This is a big todo. Probably when I
//! get around to refactoring the parser, which is a big todo.
//!
use std::{fmt, ops, path::PathBuf};

use clap::Args;
use rustc_hash::FxHashMap as HashMap;
use sarzak::sarzak::types::Ty;
use serde::{Deserialize, Serialize};
use snafu::{location, Location};

use crate::{
    dwarf::items::enuum::create_generic_enum,
    lu_dog::{
        store::ObjectStore as LuDogStore, types::ValueType, Lambda, List, Span as LuDogSpan,
        XFuture,
    },
    s_read, RefType,
};

pub mod error;
pub mod extruder;
mod items;
pub mod parser;
mod pvt;

use error::{DwarfError, Result};
pub(crate) use pvt::PrintableValueType;

pub use extruder::{inter_statement, new_lu_dog, Context};
pub use parser::{parse_dwarf, parse_line};

pub type Generics = Spanned<Vec<Spanned<Type>>>;
pub type Span = ops::Range<usize>;
pub type Spanned<T> = (T, Span);

// These should eventually come from the domain.
pub type DwarfInteger = i64;
pub type DwarfFloat = f64;

#[derive(Args, Clone, Debug, Deserialize, Serialize)]
pub struct DwarfOptions {
    /// Dwarf Source File
    ///
    /// Path to the source file to compile.
    source: PathBuf,
    /// Model File
    ///
    /// Path to the model, corresponding to the source file, to build the
    /// Lu-Dog domain.
    model: PathBuf,
    /// Meta-Model File
    ///
    /// Path to the meta-model, sarzak.
    sarzak: PathBuf,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    As,
    Asm,
    Async,
    Await,
    Bool(bool),
    Debugger,
    Else,
    Empty,
    Enum,
    Float(String),
    Fn,
    For,
    FormatString(String),
    // Global,
    Ident(String),
    If,
    Impl,
    In,
    Integer(String),
    Let,
    Match,
    Mod,
    Op(String),
    Print,
    Punct(char),
    Return,
    Self_,
    SmallSelf,
    String(String),
    Struct,
    Type(Type),
    Use,
    Uuid,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::As => write!(f, "as"),
            Self::Asm => write!(f, "asm!"),
            Self::Async => write!(f, "async"),
            Self::Await => write!(f, "await"),
            Self::Bool(bool_) => write!(f, "{}", bool_),
            Self::Debugger => write!(f, "debugger"),
            Self::Else => write!(f, "else"),
            Self::Empty => write!(f, "()"),
            Self::Enum => write!(f, "enum"),
            Self::Float(num) => write!(f, "{}", num),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            Self::FormatString(str_) => write!(f, "{}", str_),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::In => write!(f, "in"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Let => write!(f, "let"),
            Self::Match => write!(f, "match"),
            Self::Mod => write!(f, "mod"),
            Self::Op(op) => write!(f, "{}", op),
            Self::Print => write!(f, "print"),
            Self::Punct(punct) => write!(f, "{}", punct),
            Self::Return => write!(f, "return"),
            Self::Self_ => write!(f, "Self"),
            Self::SmallSelf => write!(f, "self"),
            Self::String(str_) => write!(f, "{}", str_),
            Self::Struct => write!(f, "struct"),
            Self::Type(type_) => write!(f, "{}", type_),
            Self::Use => write!(f, "use"),
            Self::Uuid => write!(f, "Uuid"),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Empty,
    Float,
    Fn(Vec<Spanned<Self>>, Box<Spanned<Self>>),
    Generic(Spanned<String>),
    Integer,
    List(Box<Spanned<Self>>),
    Path(Vec<Spanned<Self>>),
    Self_,
    String,
    Unknown,
    /// User Type
    ///
    /// Almost everything with a name falls into this category.
    ///
    /// The first element is the name of the type, and the second is a list of
    /// generic types.
    UserType(Spanned<String>, Vec<Spanned<Self>>),
    Uuid,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Boolean => write!(f, "bool"),
            Self::Empty => write!(f, "()"),
            Self::Float => write!(f, "float"),
            Self::Fn(params, return_) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.0)?;
                }
                write!(f, ") -> {}", return_.0)
            }
            Self::Generic(name) => write!(f, "{}", name.0),
            Self::Integer => write!(f, "int"),
            Self::List(type_) => write!(f, "[{}]", type_.0),
            Self::Path(path) => {
                for (i, ty) in path.iter().enumerate() {
                    if i != 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", ty.0)?;
                }
                Ok(())
            }
            Self::Self_ => write!(f, "Self"),
            Self::String => write!(f, "string"),
            Self::Unknown => write!(f, "<unknown>"),
            Self::UserType(type_, inner) => {
                if !inner.is_empty() {
                    write!(f, "{}<", type_.0)?;
                    for (i, ty) in inner.iter().enumerate() {
                        if i != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", ty.0)?;
                    }
                    write!(f, ">")
                } else {
                    write!(f, "{}", type_.0)
                }
            }
            Self::Uuid => write!(f, "Uuid"),
        }
    }
}

impl Type {
    pub fn check_type(
        &self,
        span: &Span,
        context: &mut Context,
        store: &mut LuDogStore,
    ) -> Result<bool> {
        self.into_value_type(span, context, store).map(|_| true)
    }

    pub fn into_value_type(
        &self,
        span: &Span,
        context: &mut Context,
        store: &mut LuDogStore,
    ) -> Result<RefType<ValueType>> {
        let sarzak = context.sarzak;

        match self {
            Type::Boolean => {
                let ty = Ty::new_boolean(sarzak);
                Ok(ValueType::new_ty(true, &ty, store))
            }
            Type::Empty => Ok(ValueType::new_empty(true, store)),
            Type::Float => {
                let ty = Ty::new_float(sarzak);
                Ok(ValueType::new_ty(true, &ty, store))
            }
            Type::Fn(_params, return_) => {
                let return_ = return_.0.into_value_type(&return_.1, context, store)?;
                let ƛ = Lambda::new(None, None, &return_, store);
                Ok(ValueType::new_lambda(true, &ƛ, store))
            }
            Type::Generic(name) => {
                panic!("Generics need a next and a parent.");
            }
            Type::Integer => {
                let ty = Ty::new_integer(sarzak);
                Ok(ValueType::new_ty(true, &ty, store))
            }
            Type::List(type_) => {
                let ty = type_.0.into_value_type(&type_.1, context, store)?;
                let list = List::new(&ty, store);
                Ok(ValueType::new_list(true, &list, store))
            }
            Type::Path(_) => unimplemented!(),
            Type::Self_ => panic!("Self is deprecated."),
            Type::String => {
                let ty = Ty::new_z_string(sarzak);
                Ok(ValueType::new_ty(true, &ty, store))
            }
            Type::Unknown => Ok(ValueType::new_unknown(true, store)),
            Type::UserType(type_, generics) => {
                let base_type = &type_.0;
                let base_type = if let Some(path) = context.scopes.get(base_type) {
                    path.to_owned() + base_type.as_str()
                } else {
                    base_type.to_owned()
                };

                let mut name = base_type.clone();
                let generics_string = generics
                    .iter()
                    .map(|g| g.0.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                if !generics_string.is_empty() {
                    name.push('<');
                    name.push_str(&generics_string);
                    name.push('>');
                }

                let name = if let Some(path) = context.scopes.get(&name) {
                    path.to_owned() + name.as_str()
                } else {
                    name
                };

                // This is a special case for Uuid, which is a built-in type.
                // The parser just doesn't know that, and it's actually cleaner
                // and easier to handle it here.
                if name == "Uuid" {
                    return Ok(ValueType::new_ty(true, &Ty::new_z_uuid(sarzak), store));
                }

                log::debug!(target: "dwarf", "Type::UserType: {name}");

                if base_type == "Future" {
                    let inner_type = &generics[0];
                    let inner_type = inner_type
                        .0
                        .into_value_type(&inner_type.1, context, store)?;
                    let future = XFuture::new(&inner_type, store);
                    let future = ValueType::new_x_future(true, &future, store);
                    let _ = LuDogSpan::new(
                        span.end as DwarfInteger,
                        span.start as DwarfInteger,
                        &context.source,
                        Some(&future),
                        None,
                        store,
                    );
                    Ok(future)
                } else if let Some(obj_id) = store.exhume_woog_struct_id_by_name(&name) {
                    let woog_struct = store.exhume_woog_struct(&obj_id).unwrap();
                    let woog_struct = ValueType::new_woog_struct(true, &woog_struct, store);
                    let _ = LuDogSpan::new(
                        span.end as DwarfInteger,
                        span.start as DwarfInteger,
                        &context.source,
                        Some(&woog_struct),
                        None,
                        store,
                    );
                    Ok(woog_struct)
                } else if let Some(enum_id) = store.exhume_enumeration_id_by_name(&name) {
                    let enumeration = store.exhume_enumeration(&enum_id).unwrap();
                    let enumeration = ValueType::new_enumeration(true, &enumeration, store);
                    let _ = LuDogSpan::new(
                        span.end as DwarfInteger,
                        span.start as DwarfInteger,
                        &context.source,
                        Some(&enumeration),
                        None,
                        store,
                    );
                    Ok(enumeration)
                } else if let Some(obj_id) = sarzak.exhume_object_id_by_name(&name) {
                    // If it's not in one of the models, it must be in sarzak.
                    let ty = sarzak.exhume_ty(&obj_id).unwrap();
                    log::debug!(target: "dwarf", "into_value_type, UserType, ty: {ty:?}");
                    let ty = ValueType::new_ty(true, &ty, store);
                    let _ = LuDogSpan::new(
                        span.end as DwarfInteger,
                        span.start as DwarfInteger,
                        &context.source,
                        Some(&ty),
                        None,
                        store,
                    );
                    Ok(ty)
                } else if store.exhume_enumeration_id_by_name(&base_type).is_some() {
                    Ok(create_generic_enum(&name, &base_type, span.to_owned(), context, store)?.1)
                } else {
                    Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        file: context.file_name.to_owned(),
                        span: span.to_owned(),
                        location: location!(),
                        program: context.source_string.to_owned(),
                    }])
                }
            }
            Type::Uuid => {
                let ty = Ty::new_z_uuid(sarzak);
                Ok(ValueType::new_ty(true, &ty, store))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Empty,
    Expression(Spanned<Expression>),
    Item(Item),
    Let(Spanned<String>, Option<Spanned<Type>>, Spanned<Expression>),
    Result(Spanned<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
/// Pattern Type
///
/// This is how we store parsed patterns.
pub enum Pattern {
    /// An identifier
    ///
    /// E.g., `foo`
    Identifier(Spanned<String>),
    /// A literal
    ///
    /// E.g., `420`
    Literal(Spanned<Expression>),
    /// A path pattern
    ///
    /// E.g., `Foo::Bar`
    PathPattern(Spanned<Vec<Type>>),
    /// A tuple struct
    ///
    /// E.g., `Foo(0, 1, 2)`
    TupleStruct(Box<Self>, Spanned<Vec<Spanned<Pattern>>>),
}

/// Turn a Pattern into an Expression
///
/// Deep magic happens here. We are writing code in here, more or less. We are
/// creating static method calls and local variables. Very cool stuff happening
/// here.
impl From<Pattern> for Expression {
    fn from(pattern: Pattern) -> Self {
        match pattern {
            // transmogrify an identifier into a local variable
            Pattern::Identifier((name, _span)) => Expression::LocalVariable(name),
            // 🚧 Need to do something about this.
            Pattern::Literal((literal, _span)) => match literal {
                Expression::BooleanLiteral(b) => Expression::BooleanLiteral(b),
                Expression::Empty => Expression::Empty,
                Expression::FloatLiteral(f) => Expression::FloatLiteral(f),
                Expression::IntegerLiteral(i) => Expression::IntegerLiteral(i),
                Expression::StringLiteral(s) => Expression::StringLiteral(s),
                _ => unreachable!(),
            },
            // Here we turn a path into a unit enum, which is really just a static
            // method call with storage.
            Pattern::PathPattern((path, span)) => {
                let mut path = path
                    .iter()
                    .cloned()
                    .map(|ty| (ty, None))
                    .collect::<Vec<_>>();
                let field = path.pop().unwrap();
                // 🚧 I'm not sure how this would play with generics. I don't
                // think that it's allowed.
                let field = if let Type::UserType(field, _generics) = field.0 {
                    field
                } else {
                    unreachable!();
                };

                Expression::UnitEnum(
                    Box::new((Expression::PathInExpression(path), span.to_owned())),
                    field,
                )
            }
            Pattern::TupleStruct(path, (fields, _fields_span)) => {
                // Who doesn't love a little recursion?
                let path = if let Pattern::PathPattern(path) = *path.to_owned() {
                    path
                } else {
                    unreachable!()
                };

                let mut path = path
                    .0
                    .iter()
                    .cloned()
                    .map(|ty| (ty, None))
                    .collect::<Vec<_>>();
                let name = path.pop().unwrap();
                let name = if let Type::UserType(name, _generics) = name.0 {
                    name
                } else {
                    unreachable!();
                };

                let fields = fields
                    .into_iter()
                    .map(|f| (f.0.into(), f.1))
                    .collect::<Vec<_>>();

                Expression::StaticMethodCall(
                    Box::new(Expression::PathInExpression(path)),
                    name,
                    fields,
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct WrappedValueType(pub RefType<ValueType>);

impl PartialEq for WrappedValueType {
    fn eq(&self, other: &Self) -> bool {
        *s_read!(self.0) == *s_read!(other.0)
    }
}

impl Eq for WrappedValueType {}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Addition(Box<Spanned<Self>>, Box<Spanned<Self>>),
    And(Box<Spanned<Self>>, Box<Spanned<Self>>),
    As(Box<Spanned<Self>>, Spanned<Type>),
    Asm(Vec<Spanned<Self>>),
    /// Assignment Expression
    ///
    /// E.g.: `a = b`
    ///
    /// The first element is the left-hand side expression representing the storage
    /// and the second is the right-hand side, representing the value to be stored.
    Assignment(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Await(Box<Spanned<Self>>),
    Bang(Box<Spanned<Self>>),
    Block(
        BlockType,
        Vec<Spanned<Statement>>,
        /// A list of variable names to insert into the top of the block
        ///
        // This is for `for` loops.
        Vec<String>,
        /// The types of the above variables
        ///
        // This is for `for` loops.
        Vec<WrappedValueType>,
    ),
    BooleanLiteral(bool),
    Debug,
    Division(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Empty,
    Error,
    Equals(Box<Spanned<Self>>, Box<Spanned<Self>>),
    FieldAccess(Box<Spanned<Self>>, Spanned<String>),
    FloatLiteral(f64),
    For(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    FormatString(Vec<Spanned<Self>>),
    // The first element is the function being called, the second is the list of
    // arguments.
    FunctionCall(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    GreaterThan(Box<Spanned<Self>>, Box<Spanned<Self>>),
    GreaterThanOrEqual(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Group(Box<Spanned<Self>>),
    If(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Option<Box<Spanned<Self>>>,
    ),
    Index(Box<Spanned<Self>>, Box<Spanned<Self>>),
    IntegerLiteral(i64),
    Lambda(
        BlockType,
        Vec<(Spanned<String>, Spanned<Type>)>,
        Spanned<Type>,
        Box<Spanned<Self>>,
    ),
    LessThan(Box<Spanned<Self>>, Box<Spanned<Self>>),
    LessThanOrEqual(Box<Spanned<Self>>, Box<Spanned<Self>>),
    List(Vec<Spanned<Self>>),
    LocalVariable(String),
    Match(Box<Spanned<Self>>, Vec<Spanned<(Pattern, Self)>>),
    MethodCall(Box<Spanned<Self>>, Spanned<String>, Vec<Spanned<Self>>),
    Multiplication(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Negation(Box<Spanned<Self>>),
    None,
    NotEquals(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>),
    UnitEnum(Box<Spanned<Self>>, Spanned<String>),
    Print(Box<Spanned<Self>>),
    PathInExpression(Vec<(Type, Option<Generics>)>),
    Range(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Return(Option<Box<Spanned<Self>>>),
    Some(Box<Spanned<Self>>),
    /// Static Method Call
    ///
    /// E.g., `Foo::bar()`.
    ///
    StaticMethodCall(Box<Self>, Spanned<String>, Vec<Spanned<Self>>),
    /// String Literal
    ///
    StringLiteral(String),
    /// Structure Expression
    ///
    /// Struct Expression, Vec<Field Name, Field Value>
    Struct(Box<Spanned<Self>>, Vec<(Spanned<String>, Spanned<Self>)>),
    Subtraction(Box<Spanned<Self>>, Box<Spanned<Self>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Item {
    item: Spanned<InnerItem>,
    attributes: AttributeMap,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockType {
    Async,
    Sync,
}

/// The inner bits of the Item implementation
///
///
#[derive(Clone, Debug, PartialEq)]
pub enum InnerItem {
    /// Enumeration
    ///
    /// The first field is the enum name.
    /// The second field is a Vec of the enum fields.
    /// The first item is the name of the field, and the second is an optional field description, i.e., a tuple or struct.
    /// The final field are the generics attached to the enum.
    Enum {
        name: Spanned<String>,
        fields: Vec<(Spanned<String>, Option<EnumField>)>,
        generics: Option<Generics>,
    },
    /// A Function Definition
    ///
    /// async, name, Vec<(Parameter Name, Parameter Type)>, Return Type, Vec<Statement>
    Function {
        a_sink: BlockType,
        name: Spanned<String>,
        params: Vec<(Spanned<String>, Spanned<Type>)>,
        return_type: Spanned<Type>,
        generics: Option<Generics>,
        statements: Option<Spanned<Expression>>,
    },
    /// name, Vec<(Function Name, Function)>
    Implementation(Spanned<String>, Vec<Item>),
    /// path, Option<Alias>
    Import(Spanned<Vec<Spanned<String>>>, Option<Spanned<String>>),
    Module(Spanned<String>),
    /// name, Vec<(Field Name, Field Type)>
    Struct(
        Spanned<String>,
        Vec<(Spanned<String>, Spanned<Type>, AttributeMap)>,
        Option<Generics>,
    ),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumField {
    /// Enum Tuple Field
    ///
    /// This is supposed to be a Tuple, which would imply that I have a list of
    /// Types. However, I don't yet have tuples, and I don't want to fake it here.
    /// So for now, we are just supporting a single type.
    Tuple(Spanned<Type>),
    /// Enum Struct Field
    ///
    /// This is a list of (Field Name, Field Type, AttributeMap) pairs.
    Struct(Vec<(Spanned<String>, Spanned<Type>, AttributeMap)>),
}

pub type AttributeMap = HashMap<String, Vec<(Span, InnerAttribute)>>;

#[derive(Clone, Debug)]
pub struct Attribute {
    pub name: Spanned<String>,
    pub value: InnerAttribute,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InnerAttribute {
    Attribute(AttributeMap),
    Expression(Spanned<Expression>),
    None,
}

impl TryFrom<&InnerAttribute> for String {
    type Error = DwarfError;

    fn try_from(
        inner: &InnerAttribute,
    ) -> Result<Self, <String as TryFrom<&InnerAttribute>>::Error> {
        match inner {
            InnerAttribute::Expression((Expression::StringLiteral(s), _)) => Ok(s.to_string()),
            _ => Err(DwarfError::Generic {
                description: format!("Error converting InnerAttribute to String: {inner:?}."),
            }),
        }
    }
}

#[allow(unused)]
pub(crate) fn generic_to_string(generic: &Generics) -> Spanned<String> {
    let mut result = String::new();
    let mut first_time = true;

    result.push('<');
    for (name, _) in &generic.0 {
        if first_time {
            first_time = false;
        } else {
            result.push_str(", ");
        }
        result.push_str(&name.to_string());
    }
    result.push('>');
    (result, generic.1.clone())
}
