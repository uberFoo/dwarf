use abi_stable::{std_types::RString, StableAbi};

use std::{
    error::Error as ErrorTrait,
    fmt::{self, Display},
};

#[repr(u8)]
#[derive(Debug, StableAbi)]
pub enum Error {
    Plugin(RString),
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Plugin(e) => Display::fmt(e, f),
        }
    }
}

impl ErrorTrait for Error {}
