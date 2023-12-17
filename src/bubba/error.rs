use snafu::{prelude::*, Backtrace, Location};

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);
pub(super) type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug, Snafu)]
enum BubbaError {}
