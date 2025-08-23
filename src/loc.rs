use std::{fmt, rc::Rc};

use crate::fm::{FileId, FileManager};

#[derive(Copy, Clone)]
pub struct Loc(pub Option<FileId>, pub u32);

impl Loc {
    #[allow(unused)]
    #[inline(always)]
    pub const fn file_id(&self) -> &Option<FileId> { &self.0 }

    #[inline(always)]
    #[doc(alias = "row")]
    pub const fn line_number(&self) -> u32 { self.1 }

    #[inline(always)]
    pub fn display(&self, fm: &FileManager) -> LocDisplay {
        let Self(file_id, line_number) = *self;

        let file_path = file_id.map(|file_id| {
            fm.get_file_path_rc_unchecked(file_id)
        }).unwrap_or("<virtual>".into());

        LocDisplay { file_path, line_number }
    }
}

pub struct LocDisplay {
    pub file_path: Rc<str>,
    pub line_number: u32
}

impl fmt::Display for LocDisplay {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { file_path, line_number } = self;
        write!(f, "{file_path}:{line_number}")
    }
}
