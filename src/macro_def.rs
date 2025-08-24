use crate::loc::Loc;

use std::rc::Rc;

pub enum MacroKind {
    Object(Rc<str>),
    Function {
        params: Rc<[Box<str>]>,
        body: Rc<str>,
    }
}

pub struct MacroDef {
    pub kind: MacroKind,
    pub src_loc: Loc
}
