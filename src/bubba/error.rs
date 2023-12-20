use ansi_term::Colour;
use snafu::{prelude::*, Backtrace, Location};

#[derive(Debug, Snafu)]
pub struct Error(BubbaError);
pub type Result<T, E = Error> = std::result::Result<T, E>;

const ERR_CLR: Colour = Colour::Red;
const OK_CLR: Colour = Colour::Green;
const POP_CLR: Colour = Colour::Yellow;
const OTH_CLR: Colour = Colour::Cyan;

#[derive(Debug, Snafu)]
pub(crate) enum BubbaError {
    #[snafu(display("\n{}: vm panic: {source}", ERR_CLR.bold().paint("error")))]
    VmPanic { source: Box<dyn std::error::Error> },
}
