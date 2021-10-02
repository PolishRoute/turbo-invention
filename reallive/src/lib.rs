#![feature(iter_zip)]

pub use crate::archive::read_archive;
pub use crate::parse::{Element, Expr, CallMeta, Operator, MemoryBank};
pub use crate::gameini::parse_line;

mod parse;
mod archive;
mod keys;
mod gameini;

type MyError = Box<dyn std::error::Error>;
