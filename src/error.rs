use crate::loc::LocDisplay;
use crate::assembler::InputLine;

use std::{fmt, error};

pub type Result<T> = std::result::Result<T, Error>;

pub mod prelude {
    #[allow(unused_imports)]
    pub use super::{
        Error,
        Result,
        ErrorKind::{self, *}
    };
}

pub enum ErrorKind {
    UnknownMnemonic,
    EmptyLabel,
    UnknownDirective,
    InvalidNumber,
    MacroInvalidHeader,
    MacroArgCount,
    MacroRecursion,
    UnknownParam,
    ExpectedSectionName,
    FileNotFound,
    ExpectedCommaOrEnd,
    EmptyIncludePath,
    MacroUnclosed,
    HandlingInstruction,
    ParsingMacro,
    ExpandingMacro,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownMnemonic => write!(f, "unknown mnemonic"),
            Self::EmptyLabel => write!(f, "empty label"),
            Self::UnknownDirective => write!(f, "unknown directive"),
            Self::InvalidNumber => write!(f, "invalid number"),
            Self::MacroInvalidHeader => write!(f, "invalid macro header"),
            Self::MacroArgCount => write!(f, "macro argument count mismatch"),
            Self::MacroRecursion => write!(f, "macro recursion limit exceeded"),
            Self::UnknownParam => write!(f, "unknown macro parameter"),
            Self::ExpectedSectionName => write!(f, "expected section name"),
            Self::FileNotFound => write!(f, "file not found"),
            Self::ExpectedCommaOrEnd => write!(f, "expected comma or end of arguments"),
            Self::EmptyIncludePath => write!(f, "empty include path"),
            Self::MacroUnclosed => write!(f, "unclosed macro"),
            Self::HandlingInstruction => write!(f, "error handling instruction"),
            Self::ParsingMacro => write!(f, "error parsing macro"),
            Self::ExpandingMacro => write!(f, "error expanding macro"),
        }
    }
}

pub struct Error {
    pub loc: LocDisplay,
    pub kind: ErrorKind,
    pub src: InputLine,
    pub msg: Option<Box<str>>
}

impl error::Error for Error {}

impl fmt::Debug for Error {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { loc, src, msg, kind } = self;

        if let Some(msg) = msg {
            write!(f, "{loc}: {src:?}: {kind}: {msg}")
        } else {
            write!(f, "{loc}: {src:?}: {kind}")
        }
    }
}

