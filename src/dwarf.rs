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
use sarzak::sarzak::{store::ObjectStore as SarzakStore, types::Ty};
use serde::{Deserialize, Serialize};
use snafu::{location, Location};
use uuid::Uuid;

use crate::{
    lu_dog::{
        store::ObjectStore as LuDogStore,
        types::{ValueType, WoogOption},
        Lambda, List, Reference, WoogStruct,
    },
    s_read, NewRef, RefType,
};

pub mod error;
mod expression;
pub mod extruder;
pub mod parser;
mod pvt;

use error::{DwarfError, Result};
use pvt::PrintableValueType;

pub use extruder::{inter_statement, new_lu_dog, Context};
pub use parser::{parse_dwarf, parse_line};

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
    Bool(bool),
    Debugger,
    Else,
    Empty,
    Float(String),
    Fn,
    For,
    // Global,
    Ident(String),
    If,
    Impl,
    In,
    Integer(String),
    Let,
    None,
    Op(String),
    Option,
    Print,
    Punct(char),
    Return,
    Self_,
    SmallSelf,
    Some,
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
            Self::Bool(bool_) => write!(f, "{}", bool_),
            Self::Debugger => write!(f, "debugger"),
            Self::Else => write!(f, "else"),
            Self::Empty => write!(f, "()"),
            Self::Float(num) => write!(f, "{}", num),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            // Self::Global => write!(f, "global"),
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::In => write!(f, "in"),
            Self::Integer(num) => write!(f, "{}", num),
            Self::Let => write!(f, "let"),
            Self::None => write!(f, "None"),
            Self::Op(op) => write!(f, "{}", op),
            Self::Option => write!(f, "Option"),
            Self::Print => write!(f, "print"),
            Self::Punct(punct) => write!(f, "{}", punct),
            Self::Return => write!(f, "return"),
            Self::Self_ => write!(f, "Self"),
            Self::SmallSelf => write!(f, "self"),
            Self::Some => write!(f, "Some"),
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
    Integer,
    List(Box<Spanned<Self>>),
    Option(Box<Spanned<Self>>),
    Reference(Box<Spanned<Self>>),
    Self_,
    String,
    Unknown,
    UserType(Spanned<String>),
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
            Self::Integer => write!(f, "int"),
            Self::List(type_) => write!(f, "[{}]", type_.0),
            Self::Option(type_) => write!(f, "Option<{}>", type_.0),
            Self::Reference(type_) => write!(f, "&{}", type_.0),
            Self::Self_ => write!(f, "Self"),
            Self::String => write!(f, "string"),
            Self::Unknown => write!(f, "<unknown>"),
            Self::UserType(type_) => write!(f, "{}", type_.0),
            Self::Uuid => write!(f, "Uuid"),
        }
    }
}

impl Type {
    pub fn check_type(
        &self,
        span: &Span,
        store: &mut LuDogStore,
        models: &HashMap<String, SarzakStore>,
        sarzak: &SarzakStore,
    ) -> Result<bool> {
        self.into_value_type(span, store, models, sarzak)
            .map(|_| true)
    }

    pub fn into_value_type(
        &self,
        span: &Span,
        store: &mut LuDogStore,
        models: &HashMap<String, SarzakStore>,
        sarzak: &SarzakStore,
    ) -> Result<RefType<ValueType>> {
        match self {
            Type::Boolean => {
                let ty = Ty::new_boolean();
                Ok(ValueType::new_ty(&ty, store))
            }
            Type::Empty => Ok(ValueType::new_empty(store)),
            Type::Float => {
                let ty = Ty::new_float();
                Ok(ValueType::new_ty(&ty, store))
            }
            Type::Fn(_params, return_) => {
                let return_ = return_
                    .0
                    .into_value_type(&return_.1, store, models, sarzak)?;
                let ƛ = Lambda::new(None, &return_, store);
                Ok(ValueType::new_lambda(&ƛ, store))
            }
            Type::Integer => {
                let ty = Ty::new_integer();
                Ok(ValueType::new_ty(&ty, store))
            }
            Type::List(type_) => {
                let ty = type_.0.into_value_type(&type_.1, store, models, sarzak)?;
                let list = List::new(&ty, store);
                Ok(ValueType::new_list(&list, store))
            }
            Type::Option(type_) => {
                let ty = type_.0.into_value_type(&type_.1, store, models, sarzak)?;
                let option = WoogOption::new_z_none(&ty, store);
                Ok(ValueType::new_woog_option(&option, store))
            }
            Type::Reference(type_) => {
                let ty = type_.0.into_value_type(&type_.1, store, models, sarzak)?;
                let reference = Reference::new(Uuid::new_v4(), false, &ty, store);
                Ok(ValueType::new_reference(&reference, store))
            }
            Type::Self_ => panic!("Self is deprecated."),
            Type::String => {
                let ty = Ty::new_s_string();
                Ok(ValueType::new_ty(&ty, store))
            }
            Type::Unknown => Ok(ValueType::new_unknown(store)),
            Type::UserType(type_) => {
                let name = &type_.0;

                log::debug!(target: "dwarf", "Type::UserType: {name}");

                // 🚧 HashMapFix
                // for (_, model) in models {
                //     if let Some(obj_id) = model.exhume_object_id_by_name(name) {
                //         let woog_struct = store
                //             .iter_z_object_store()
                //             .find(|os| {
                //                 let wrapper = s_read!(os).object;
                //                 let wrapper = store.exhume_object_wrapper(&wrapper).unwrap();
                //                 let object = s_read!(wrapper).object;
                //                 object == obj_id
                //             })
                //             .map(|os| s_read!(os).r78_woog_struct(store)[0].clone());

                //         if let Some(woog_struct) = woog_struct {
                //             let woog_struct = s_read!(woog_struct);
                //             return Ok(ValueType::new_woog_struct(
                //                 &<RefType<WoogStruct> as NewRef<WoogStruct>>::new_ref(
                //                     woog_struct.to_owned(),
                //                 ),
                //                 store,
                //             ));
                //         } else {
                //             return Err(vec![DwarfError::UnknownType {
                //                 ty: name.to_owned(),
                //                 span: span.to_owned(),
                //                 location: location!(),
                //             }]);
                //         }
                //     } else {
                //         return Err(vec![DwarfError::UnknownType {
                //             ty: name.to_owned(),
                //             span: span.to_owned(),
                //             location: location!(),
                //         }]);
                //     }
                // }

                if let Some(obj_id) = store.exhume_woog_struct_id_by_name(name) {
                    let woog_struct = store.exhume_woog_struct(&obj_id).unwrap();
                    Ok(ValueType::new_woog_struct(&woog_struct, store))
                } else
                // If it's not in one of the models, it must be in sarzak.
                if let Some(obj_id) = sarzak.exhume_object_id_by_name(name) {
                    let ty = sarzak.exhume_ty(&obj_id).unwrap();
                    dbg!(&ty);
                    log::debug!(target: "dwarf", "into_value_type, UserType, ty: {ty:?}");
                    Ok(ValueType::new_ty(ty, store))
                } else {
                    dbg!("Unknown type");
                    log::error!(target: "dwarf", "Unknown type");
                    Err(vec![DwarfError::UnknownType {
                        ty: name.to_owned(),
                        span: span.to_owned(),
                        location: location!(),
                    }])
                }
            }
            Type::Uuid => {
                let ty = Ty::new_s_uuid();
                Ok(ValueType::new_ty(&ty, store))
            }
        }
    }
}

// impl From<(&Type, &mut LuDogStore, &SarzakStore)> for ValueType {
//     fn from((type_, store, model): (&Type, &mut LuDogStore, &SarzakStore)) -> Self {
//         match type_ {
//             Type::Boolean => {
//                 let ty = Ty::new_boolean();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Empty => ValueType::new_empty(),
//             Type::Float => {
//                 let ty = Ty::new_float();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Integer => {
//                 let ty = Ty::new_integer();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Option(type_) => {
//                 let ty = (&**type_, &store, &model).into();
//                 let option = WoogOption::new_none(&ty, store);
//                 ValueType::new_woog_option(&option, store)
//             }
//             Type::Self_(type_) => panic!("Self is deprecated."),
//             Type::String => {
//                 let ty = Ty::new_s_string();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::UserType(type_) => {
//                 let name = if let Token::Object(name) = &**type_ {
//                     name
//                 } else {
//                     panic!("Expected UserType to be Token::Object.")
//                 };
//                 let obj_id = model.exhume_object_id_by_name(&name).unwrap();
//                 let ty = model.exhume_ty(obj_id).unwrap();
//                 ValueType::new_ty(&ty, store)
//             }
//             Type::Uuid => {
//                 let ty = Ty::new_s_uuid();
//                 ValueType::new_ty(&ty, store)
//             }
//         }
//     }
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Empty,
    Expression(Spanned<Expression>),
    Item(Item),
    Let(Spanned<String>, Option<Spanned<Type>>, Spanned<Expression>),
    Result(Spanned<Expression>),
}

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
    Bang(Box<Spanned<Self>>),
    Block(Vec<Spanned<Statement>>),
    BooleanLiteral(bool),
    Debug,
    Division(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Empty,
    Error,
    Equals(Box<Spanned<Self>>, Box<Spanned<Self>>),
    FieldAccess(Box<Spanned<Self>>, Spanned<String>),
    FloatLiteral(f64),
    For(Spanned<String>, Box<Spanned<Self>>, Box<Spanned<Self>>),
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
        Vec<(Spanned<String>, Spanned<Type>)>,
        Spanned<Type>,
        Box<Spanned<Self>>,
    ),
    LessThan(Box<Spanned<Self>>, Box<Spanned<Self>>),
    LessThanOrEqual(Box<Spanned<Self>>, Box<Spanned<Self>>),
    List(Vec<Spanned<Self>>),
    LocalVariable(String),
    MethodCall(Box<Spanned<Self>>, Spanned<String>, Vec<Spanned<Self>>),
    Multiplication(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Negation(Box<Spanned<Self>>),
    None,
    NotEquals(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Or(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Print(Box<Spanned<Self>>),
    Range(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Return(Box<Spanned<Self>>),
    Some(Box<Spanned<Self>>),
    /// Static Method Call
    ///
    /// E.g., `Foo::bar()`.
    ///
    StaticMethodCall(Type, Spanned<String>, Vec<Spanned<Self>>),
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
pub enum InnerItem {
    /// A Function Definition
    ///
    /// name, Vec<(Parameter Name, Parameter Type)>, Return Type, Vec<Statement>
    Function(
        Spanned<String>,
        Vec<(Spanned<String>, Spanned<Type>)>,
        Spanned<Type>,
        Option<Spanned<Expression>>,
    ),
    /// name, Vec<(Function Name, Function)>
    Implementation(Spanned<String>, Vec<Item>),
    /// path, Option<Alias>
    Import(Spanned<Vec<Spanned<String>>>, Option<Spanned<String>>),
    /// name, Vec<(Field Name, Field Type)>
    Struct(Spanned<String>, Vec<(Spanned<String>, Spanned<Type>)>),
}

pub type AttributeMap = HashMap<String, Vec<(Span, InnerAttribute)>>;

#[derive(Clone, Debug, PartialEq)]
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
