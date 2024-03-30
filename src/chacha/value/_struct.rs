use std::fmt;

use rustc_hash::FxHashMap as HashMap;
use serde::{Deserialize, Serialize};

use crate::{lu_dog::ValueType, s_read, RefType, PATH_SEP};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Struct<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    type_name: String,
    type_: RefType<ValueType>,
    attrs: StructAttributes<T>,
}

impl<T> PartialEq for Struct<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    fn eq(&self, other: &Self) -> bool {
        s_read!(self.type_).eq(&s_read!(other.type_)) && self.attrs.eq(&other.attrs)
    }
}

impl<T> Eq for Struct<T> where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default
{
}

impl<T> Struct<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    pub fn new<S: AsRef<str>>(type_name: S, type_: &RefType<ValueType>) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
            type_: type_.clone(),
            attrs: StructAttributes::default(),
        }
    }

    /// Create a field for the user type
    ///
    /// This is called during type definition, from a declaration in a source file.
    pub fn define_field<S: AsRef<str>>(&mut self, name: S, value: T) {
        self.attrs.0.insert(name.as_ref().to_owned(), value);
    }

    pub fn get_field_value<S: AsRef<str>>(&self, name: S) -> Option<&T> {
        self.attrs.0.get(name.as_ref())
    }

    pub fn set_field_value<S: AsRef<str>>(&mut self, name: S, value: T) -> Option<T> {
        self.attrs.0.insert(name.as_ref().to_owned(), value)
    }

    pub fn get_type(&self) -> &RefType<ValueType> {
        &self.type_
    }

    pub fn type_name(&self) -> &str {
        &self.type_name
    }
}

impl<T> fmt::Display for Struct<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut attrs = self.attrs.0.iter().collect::<Vec<_>>();
        attrs.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

        let name = if let Some(name) = self.type_name.strip_prefix(PATH_SEP) {
            name
        } else {
            &self.type_name
        };

        let mut out = f.debug_struct(name);
        for (k, v) in attrs {
            out.field(k, &format_args!("{v}"));
        }

        out.finish()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct StructAttributes<T>(HashMap<String, T>)
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default;

impl<T> PartialEq for StructAttributes<T>
where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default,
{
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        for (k, v) in self.0.iter() {
            if !other.0.contains_key(k) {
                return false;
            }

            let ov = other.0.get(k).unwrap();

            if !v.eq(&ov) {
                return false;
            }
        }

        true
    }
}

impl<T> Eq for StructAttributes<T> where
    T: Clone + std::fmt::Debug + PartialEq + std::fmt::Display + std::default::Default
{
}
