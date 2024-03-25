use std::fmt;

use serde::{Deserialize, Serialize};

use crate::{chacha::value::Struct, lu_dog::ValueType, s_read, RefType};

/// The type of Enumeration Field
///
/// There are three types of enumeration fields: Unit, Struct, and Tuple.
///
/// The field descriptions refer to the following enumeration, `Foo`.
///
/// ```ignore
/// enum Foo {
///     One,
///     Bar(int),
///     Baz { qux: string },
/// }
/// ```
///
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Enum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    /// Struct Enumeration Field
    ///
    /// This type of field is for when it contains a struct, as `Baz` does above.
    /// That is to say, `Foo::Baz { qux: string }`. We store the final path element
    /// as a string, and the struct as a `RefType<UserStruct>`.
    Struct(RefType<Struct<T>>),
    /// Tuple Enumeration Field
    ///
    /// This type of field is for when it contains a tuple, as `Bar` does above.
    /// That is to say, `Foo::Bar(int)`.
    ///
    /// The type is stored as the first element of the tuple, and the path/type
    /// as a string in the second. The third element is the enum itself.
    Tuple((RefType<ValueType>, String), RefType<TupleEnum<T>>),
    /// Unit Enumeration Field
    ///
    /// This sort of enumeration is the simplest. In `Foo`, this refers to the
    /// field `One`: `Foo::One`. The final field in the path is stored as a
    /// string.
    ///
    /// The tuple is: (Type, TypeName, FieldValue)
    Unit(RefType<ValueType>, String, String),
}

impl<T> Enum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    pub fn get_type(&self) -> RefType<ValueType> {
        match self {
            Self::Unit(ty, _, _) => ty.clone(),
            Self::Struct(ut) => s_read!(ut).get_type().clone(),
            Self::Tuple((ty, _), _) => ty.clone(),
        }
    }
}

impl<T> PartialEq for Enum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit(_a, b, e), Self::Unit(_c, d, f)) => b == d && e == f,
            (Self::Struct(a), Self::Struct(b)) => *s_read!(a) == *s_read!(b),
            (Self::Tuple((a, _), c), Self::Tuple((b, _), d)) => {
                *s_read!(a) == *s_read!(b) && *s_read!(c) == *s_read!(d)
            }
            _ => false,
        }
    }
}

impl<T> Eq for Enum<T> where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default
{
}

impl<T> fmt::Display for Enum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Unit(_, t, s) => write!(f, "{t}::{s}",),
            Self::Struct(s) => write!(f, "{}", s_read!(s)),
            Self::Tuple((_, t), e) => write!(f, "{t}::{}", s_read!(e)),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TupleEnum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display,
{
    pub(crate) variant: String,
    pub(crate) value: RefType<T>,
}

impl<T> PartialEq for TupleEnum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display,
{
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant && s_read!(self.value).eq(&s_read!(other.value))
    }
}

impl<T> Eq for TupleEnum<T> where T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display {}

impl<T> TupleEnum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display,
{
    pub fn new<S: AsRef<str>>(variant_name: S, value: RefType<T>) -> Self {
        Self {
            variant: variant_name.as_ref().to_owned(),
            value,
        }
    }

    pub fn value(&self) -> RefType<T> {
        self.value.clone()
    }

    pub fn variant(&self) -> &str {
        &self.variant
    }
}

impl<T> fmt::Display for TupleEnum<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", self.variant(), s_read!(self.value))
    }
}
