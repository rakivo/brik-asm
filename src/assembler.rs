use crate::encoder::Encoder;
use crate::error::prelude::*;
use crate::mnemonic::Mnemonic;
use crate::loc::{Loc, LocDisplay};
use crate::fm::{BrikFile, FileId, FileManager};
use crate::parse::{
    take_hex,
    take_signed,
    take_string,
    split_at_space,
    skip_whitespace,
    first_token_and_rest
};

use std::fmt;
use std::rc::Rc;
use std::borrow::Cow;
use std::ptr::NonNull;
use std::{str, slice};
use std::collections::HashMap;

use brik::object::write::{
    Object,
    SectionId,
    SymbolKind,
    SectionKind,
    SymbolScope,
    SymbolSection,
    StandardSegment,
};

use memchr::Memchr;
use num_traits::Num;
use anyhow::{bail, Context};

const MACRO_RECURSION_LIMIT: usize = 64;

const STANDARD_LIBRARY_FILE_PATH: &str = "std";

const STANDARD_LIBRARY: &str = include_str!("../std.s");

pub struct Sections {
    text   : SectionId,
    data   : SectionId,
    rodata : SectionId,
    bss    : SectionId,
}

struct MacroDef {
    params: Rc<[Box<str>]>,
    body: Rc<str>,
    src_loc: Loc
}

#[derive(Copy, Clone)]
pub struct InputLine {
    content: NonNull<u8>,
    len: u32,
    src_loc: Loc
}

impl fmt::Debug for InputLine {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{self}\"")
    }
}

impl fmt::Display for InputLine {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl InputLine {
    #[inline(always)]
    pub const fn empty_file(loc: Loc) -> Self {
        Self {
            content: NonNull::dangling(),
            len: 0,
            src_loc: loc
        }
    }

    #[inline(always)]
    pub const fn empty(line_number: u32) -> Self {
        Self {
            content: NonNull::dangling(),
            len: 0,
            src_loc: Loc(None, line_number)
        }
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        unsafe {
            let s = slice::from_raw_parts(self.content.as_ptr(), self.len as _);
            str::from_utf8_unchecked(s)
        }
    }
}

enum InputSource {
    File {
        loc: Loc,
        content: NonNull<u8>,
        len: usize,
        pos: usize,
    },
    Virtual {
        content: Box<str>,
        pos: usize,
        line_number: u32,
    },
    MacroExpansion {
        content: Box<str>,
        pos: usize,
        line_number: u32,
        original_loc: Loc
    },
}

impl InputSource {
    #[inline(always)]
    fn virtual_from_str(s: &str) -> Self {
        Self::Virtual {
            content: s.to_owned().into_boxed_str(),
            pos: 0,
            line_number: 1
        }
    }

    #[inline(always)]
    const fn file_from_str(s: &str, file_id: FileId) -> Self {
        Self::File {
            content: unsafe {
                NonNull::new(s.as_ptr() as _).unwrap_unchecked()
            },

            len: s.len(),
            pos: 0,
            loc: Loc(Some(file_id), 1)
        }
    }

    #[inline(always)]
    const fn original_line_number(&self) -> u32 {
        match self {
            Self::File { loc, .. } => loc.line_number(),

            Self::MacroExpansion { original_loc: loc, .. } => loc.line_number(),

            Self::Virtual { line_number, .. } => *line_number
        }
    }

    // Returns (line, should_advance)
    fn peek_line(&self) -> Option<(InputLine, bool)> {
        match self {
            Self::File { loc, content, len, pos } => {
                if *pos >= *len {
                    return Some(
                        (InputLine::empty_file(*loc), false)
                    )
                }

                let slice = unsafe {
                    slice::from_raw_parts(content.as_ptr(), *len)
                };

                let rest = &slice[*pos..];

                let end = memchr::memchr(b'\n', rest)
                    .map(|i| *pos + i)
                    .unwrap_or(*len);

                let line = unsafe {
                    str::from_utf8_unchecked(&slice[*pos..end])
                };

                let line = InputLine {
                    content: NonNull::new(line.as_ptr() as _).unwrap(),
                    len: line.len() as _,
                    src_loc: *loc
                };

                Some((line, true))
            }

            Self::Virtual { content, pos, .. } |
            Self::MacroExpansion { content, pos, .. } => {
                let original_line = self.original_line_number();

                if *pos >= content.len() {
                    return Some(
                        (InputLine::empty(original_line), false)
                    )
                }

                let rem = &content[*pos..];
                let end = memchr::memchr(b'\n', rem.as_bytes())
                    .unwrap_or(rem.len());

                let line = &rem[..end];

                let line = InputLine {
                    content: NonNull::new(line.as_ptr() as _).unwrap(),
                    len: line.len() as _,
                    src_loc: Loc(None, original_line)
                };

                Some((line, true))
            }
        }
    }

    #[inline]
    fn advance(&mut self) {
        #[inline(always)]
        fn maybe_advance(pos: &mut usize, ln: &mut u32, content: &[u8]) {
            if let Some(nl_pos) = memchr::memchr(b'\n', &content[*pos..]) {
                *pos += nl_pos + 1;
            } else {
                *pos = content.len();
            }

            *ln += 1;
        }

        match self {
            Self::File { content, len, pos, loc } => {
                if *pos >= *len { return }

                let slice = unsafe {
                    slice::from_raw_parts(content.as_ptr(), *len)
                };

                maybe_advance(pos, &mut loc.1, slice);
            }

            Self::Virtual { content, pos, line_number } |
            Self::MacroExpansion { content, pos, line_number, .. } => {
                if *pos >= content.len() { return }

                maybe_advance(pos, line_number, content.as_bytes());
            }
        }
    }
}

pub struct Assembler<'a> {
    enc: Encoder<'a>,
    sections: Sections,

    curr_line: InputLine,

    file_manager: FileManager,

    macros: HashMap<Box<str>, MacroDef, wyhash::WyHasherBuilder>,
    input_stack: Vec<InputSource>,
}

#[allow(dead_code)]
impl<'a> Assembler<'a> {
    #[inline]
    pub fn new(asm: brik::asm::Assembler<'a>, file_path: &str) -> anyhow::Result<Self> {
        let mut file_manager = FileManager::default();

        let (file_id, bytes) = BrikFile::new(file_path)
            .and_then(|file| file_manager.read_file(file))
            .with_context(|| format!("reading input file: {file_path}"))?;

        let str = unsafe {
            str::from_utf8_unchecked(bytes)
        };

        let input_stack = vec![
            InputSource::file_from_str(str, file_id)
        ];

        let mut enc = Encoder::new(asm, file_id);

        let asm = Self {
            input_stack,
            file_manager,
            macros: HashMap::default(),
            curr_line: InputLine::empty_file(Loc(
                Some(file_id),
                1
            )),
            sections: Sections {
                rodata : enc.add_rodata_section(),
                text   : enc.add_text_section(),
                data   : enc.add_data_section(),
                bss    : enc.add_bss_section(),
            },
            enc,
        };

        Ok(asm)
    }

    #[inline(always)]
    fn loc_display(&self) -> LocDisplay {
        self.enc.loc.display(&self.file_manager)
    }

    #[inline]
    fn get_next_line(&mut self) -> Option<InputLine> {
        loop {
            let last = self.input_stack.last_mut()?;

            let (line, should_advance) = last
                .peek_line()
                .unwrap_or((InputLine::empty(self.enc.loc.1), false));

            if !should_advance {
                // source exhausted, pop it
                _ = self.input_stack.pop();
                continue
            }

            last.advance();

            return Some(line)
        }
    }

    #[inline(always)]
    fn append_virtual(&mut self, virt: &str) {
        self.input_stack.push(InputSource::virtual_from_str(virt));
    }

    #[inline]
    fn append_input_file(&mut self, file_path: &str) -> Result<()> {
        let Ok((file_id, bytes)) = BrikFile::new(file_path)
            .and_then(|file| self.file_manager.read_file(file))
        else {
            return Err(self.error_file_not_found())
        };

        let str = unsafe {
            str::from_utf8_unchecked(bytes)
        };

        self.input_stack.push(InputSource::file_from_str(str, file_id));

        Ok(())
    }

    pub fn assemble(mut self) -> anyhow::Result<Object<'a>> {
        self.enc.position_at_end(self.sections.text);

        let err = loop {
            let Some(input_line) = self.get_next_line() else {
                break None
            };

            self.curr_line = input_line;
            self.enc.loc = self.curr_line.src_loc;

            let line = unsafe {
                // SAFETY: this loop is the only place
                // in the program that mutates .curr_line
                let self_ptr: *const Self = &self;
                (*self_ptr).curr_line.as_str()
            };

            let line = line.strip_suffix('\n').unwrap_or(line);
            let line = line.trim_start();

            if line.is_empty() { continue }

            let line_bytes = line.as_bytes();
            let first_byte = *unsafe { line_bytes.get_unchecked(0) };

            if first_byte == b';' { continue }

            let line = match line.find(';') {
                Some(pos) => line[..pos].trim_end(),
                None => line
            };

            if line.is_empty() { continue }

            if line_bytes[line_bytes.len() - 1] == b':' {
                let lbl = &line[..line.len() - 1].trim();

                if lbl.is_empty() {
                    break Some(self.error_empty_label())
                }

                if let Some(sym_id) = self.enc.symbol_id(lbl.as_bytes()) {
                    let curr_offset = self.enc.curr_offset();
                    let curr_section = self.enc.expect_curr_section();
                    self.enc.edit_sym(sym_id, |s| {
                        s.section = SymbolSection::Section(curr_section);
                        s.value = curr_offset;
                    });
                    self.enc.mark_sym_defined(sym_id);
                }

                self.enc.place_or_add_label_here(
                    lbl,
                    SymbolKind::Text,
                    SymbolScope::Compilation
                );

                continue
            }

            if first_byte == b'.' {
                if let Err(e) = self.handle_directive(line) {
                    break Some(e)
                }

                continue
            }

            if line_bytes.starts_with(b"macro ") {
                if let Err(e) = self.handle_macro_definition(line) {
                    break Some(e)
                }

                continue
            }

            let (first_word, rest) = split_at_space(line);
            if self.macros.contains_key(first_word) {
                if let Err(e) = self.call_macro(first_word, rest) {
                    break Some(e)
                }

                continue
            }

            let (mn, rest) = split_at_space(line);
            let Some(m) = Mnemonic::try_from_str(mn) else {
                break Some(self.error_unknown_mnemonic())
            };

            if let Err(e) = self.enc.encode_inst(m, rest) {
                break Some(self.error_handling_instruction_with_msg(
                    e.to_string()
                ))
            }
        };

        if let Some(err) = err {
            bail!(err.to_string())
        }

        let file_path = self.loc_display().file_path;

        self.enc
            .finish(&self.file_manager)
            .map_err(|e| anyhow::anyhow!(e.to_string()))
            .with_context(|| format!{
                "generating object file for {file_path}",
            })
    }

    fn handle_directive(&mut self, line: &str) -> Result<()> {
        let line_bytes = line.as_bytes();

        // find space position once
        let space_pos = line_bytes
            .iter()
            .position(|&b| b == b' ')
            .unwrap_or(line.len());

        let directive = &line_bytes[1..space_pos];
        let rest = if space_pos < line.len() {
            line[space_pos + 1..].trim_start()
        } else {
            ""
        };

        let next = || {
            if rest.is_empty() {
                return Err(self.error_expected_section_name())
            }

            Ok(rest)
        };

        match directive {
            b"text"   => self.enc.position_at_end(self.sections.text),
            b"data"   => self.enc.position_at_end(self.sections.data),
            b"rodata" => self.enc.position_at_end(self.sections.rodata),
            b"bss"    => self.enc.position_at_end(self.sections.bss),

            b"section" => {
                let name = next()?;
                match name.as_bytes() {
                    b"text"   => self.enc.position_at_end(self.sections.text),
                    b"data"   => self.enc.position_at_end(self.sections.data),
                    b"rodata" => self.enc.position_at_end(self.sections.rodata),
                    b"bss"    => self.enc.position_at_end(self.sections.bss),
                    other      => _ = self.enc.add_section_at_end(
                        StandardSegment::Data,
                        other,
                        SectionKind::Data
                    )
                }
            }

            b"extern" | b"extrn" => {
                let name = next()?;
                let (_, sym_id) = self.enc.edit_or_add_sym_and_edit_it(name, |s| {
                    s.section = SymbolSection::Undefined;
                    s.scope   = SymbolScope::Dynamic;
                    s.weak    = false;
                });
                self.enc.mark_sym_defined(sym_id);
                self.enc.intern_sym(name, sym_id);
            }

            b"global" | b"globl" => {
                let name = next()?;
                let lbl_id = self.enc.get_or_declare_label(
                    name,
                    SymbolKind::Text,
                    SymbolScope::Compilation
                );
                self.enc.make_label_global(lbl_id);
                // should we intern labels as well?
                // self.enc.intern_label(name, lbl_id);
            }

            b"space" => {
                let count = self.parse_i::<u64>(rest)?;
                self.enc.emit_zeroes(count as _);
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            b"ascii" => {
                let (str, _) = take_string(rest);
                let v = &str[1..str.len() - 1];
                self.enc.emit_string(v.to_owned());
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            b"asciiz" => {
                let (str, _) = take_string(rest);
                let v = &str[1..str.len() - 1];
                self.enc.emit_string(v.to_owned());
                self.enc.emit_byte(0);
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            b"byte" => {
                self.enc.align_to(1);
                self.encoding_list(rest, b' ', |asm, v| _ = asm.enc.emit_byte(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"hword" => {
                self.enc.align_to(2);
                self.encoding_list(rest, b' ', |asm, v| _ = asm.enc.emit_half(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"word" => {
                self.enc.align_to(4);
                self.encoding_list(rest, b' ', |asm, v| _ = asm.enc.emit_word(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"dword" => {
                self.enc.align_to(8);
                self.encoding_list(rest, b' ', |asm, v| _ = asm.enc.emit_dword(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"include" => {
                let rest = next()?;
                let (file_path, _) = take_string(rest);
                let file_path = &file_path[1..file_path.len()-1];

                if file_path.is_empty() {
                    return Err(self.error_empty_include_path())
                }

                if file_path == STANDARD_LIBRARY_FILE_PATH {
                    self.append_virtual(STANDARD_LIBRARY);
                } else {
                    self.append_input_file(file_path)?;
                }
            }

            _ => {
                let dir_str = str::from_utf8(directive)
                    .unwrap_or("<invalid utf8>");

                return Err(self.error_unknown_directive_with_msg(
                    format!("unknown directive .{dir_str}")
                ))
            }
        }

        Ok(())
    }

     fn handle_macro_definition(&mut self, line: &str) -> Result<()> {
        // "macro name [..param] {"
        let Some(header) = line
            .strip_prefix("macro ")
            .and_then(|s| s.strip_suffix("{"))
        else {
            return Err(self.error_macro_invalid_header())
        };

        let mut parts = header.split_whitespace();

        let Some(name) = parts.next() else {
            return Err(self.error_macro_invalid_header())
        };

        let params = parts
            .map(|s| s.strip_prefix('$').unwrap_or(s))
            .map(ToOwned::to_owned)
            .map(Into::into)
            .collect();

        // collect macro body until we find closing brace at same nesting level.
        let mut body = String::new();

        // start with 1 because we've already seen the opening brace in header.
        let mut brace_count = 1isize;

        while let Some(line) = self.get_next_line() {
            let b = line.as_str().as_bytes();
            let open  = bytecount::count(b, b'{') as isize;
            let close = bytecount::count(b, b'}') as isize;
            brace_count += open - close;

            if brace_count == 0 {
                break
            }

            body.push_str(line.as_str());
            body.push('\n');
        }

        if brace_count != 0 {
            return Err(self.error_macro_unclosed_with_msg(
                format!("unclosed macro definition for '{name}'")
            ))
        }

        self.macros.insert(
            name.to_owned().into(),
            MacroDef {
                params,
                body: body.into(),
                src_loc: self.enc.loc,
            }
        );

        Ok(())
    }

    fn call_macro(&mut self, name: &str, args_str: &str) -> Result<()> {
        let macro_def = self.macros.get(name).unwrap();
        let (params, body) = (
            Rc::clone(&macro_def.params),
            Rc::clone(&macro_def.body)
        );

        let args = args_str.split_whitespace()
            .map(|arg| arg.strip_suffix(',').unwrap_or(arg))
            .collect::<Vec<_>>();

        if args.len() != params.len() {
            let e = self.error_macro_arg_count_with_msg(format!{
                "expected {a} arguments, got {b}\nnote: macro defined here: {loc}",
                a = params.len(),
                b = args.len(),
                loc = macro_def.src_loc.display(&self.file_manager)
            });
            return Err(e)
        }

        let expanded = self.expand_macro_body(&body, &params, &args)?;

        self.input_stack.push(InputSource::MacroExpansion {
            content: expanded,
            pos: 0,
            line_number: 1,
            original_loc: self.enc.loc
        });

        Ok(())
    }

    #[inline]
    fn expand_macro_body(
        &mut self,
        body: &str,
        params: &[Box<str>],
        args: &[&str]
    ) -> Result<Box<str>> {
        let args_map = HashMap::from_iter(
            params
                .iter()
                .map(AsRef::as_ref)
                .zip(args.iter().map(AsRef::as_ref))
        );

        let mut out = String::with_capacity(body.len().max(128));

        self.expand_lines_into(&mut out, body, &args_map, 0)?;

        if out.ends_with('\n') { _ = out.pop() }

        Ok(out.into_boxed_str())
    }

    /// recursively expand lines. `args-map` maps local `$param` -> value (already substituted values).
    fn expand_lines_into(
        &self,
        out: &mut String,
        body: &str,
        args_map: &HashMap<&str, &str>,
        depth: usize,
    ) -> Result<()> {
        if depth > MACRO_RECURSION_LIMIT {
            let e = self.error_macro_recursion_with_msg(format!{
                "macro recursion limit ({MACRO_RECURSION_LIMIT}) exceeded"
            });
            return Err(e)
        }

        let mut byte_offset = 0;

        for nl in Memchr::new(b'\n', body.as_bytes()) {
            let line = &body[byte_offset..nl];
            byte_offset = nl + 1;

            let sub = self.substitute_params_in_line(line, args_map)?;
            let sub_str = sub.as_ref();

            let (first_tok, rest_opt) = first_token_and_rest(sub_str);

            // check if first token is a macro
            let Some(nested_def) = first_tok.and_then(|f| self.macros.get(f)) else {
                out.push_str(sub_str);
                out.push('\n');

                continue
            };

            let nested_args = match rest_opt {
                Some(rest) => rest
                    .split_whitespace()
                    .map(|arg| arg.strip_suffix(',').unwrap_or(arg))
                    .collect(),

                None => const { Vec::new() }
            };

            if nested_args.len() != nested_def.params.len() {
                let e = self.error_macro_arg_count_with_msg(format!{
                    "expected {a} arguments, got {b}\nnote: macro defined here: {loc}",
                    a = nested_def.params.len(),
                    b = nested_args.len(),
                    loc = nested_def.src_loc.display(&self.file_manager)
                });

                return Err(e)
            }

            let nested_args_map = HashMap::from_iter(
                nested_def.params
                    .iter()
                    .map(AsRef::as_ref)
                    .zip(nested_args.iter().map(AsRef::as_ref))
            );

            self.expand_lines_into(
                out,
                &nested_def.body,
                &nested_args_map,
                depth + 1
            )?;
        }

        Ok(())
    }

    /// replace $param occurrences in `line` using `repl` mapping.
    /// only accepts identifiers `[A-Za-z0-9_]` after `$` for the name.
    /// if a `$name` is not present in `sub_map`, it is left intact (keeps `$name`).
    fn substitute_params_in_line<'b>(
        &self,
        line: &'b str,
        sub_map: &HashMap<&str, &str>
    ) -> Result<Cow<'b, str>> {
        if bytecount::count(line.as_bytes(), b'$') == 0 {
            return Ok(line.into())
        }

        let bytes = line.as_bytes();
        let mut out = String::with_capacity(line.len());
        let mut i = 0;

        while i < bytes.len() {
            if bytes[i] == b'$' {
                // parse identifier after $
                i += 1;

                let start = i;
                while i < bytes.len() {
                    let b = bytes[i];
                    let is_ident = b.is_ascii_alphanumeric() || b == b'_';

                    if !is_ident { break }

                    i += 1;
                }

                let name = &line[start..i];
                if name.is_empty() {
                    out.push('$');
                } else if let Some(val) = sub_map.get(name) {
                    out.push_str(val);
                } else {
                    let e = self.error_unknown_param_with_msg(name);
                    return Err(e)
                }
            } else {
                out.push(bytes[i] as char);
                i += 1;
            }
        }

        Ok(out.into())
    }

    #[inline(always)]
    fn error_unknown_mnemonic(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownMnemonic,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_unknown_mnemonic_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownMnemonic,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // EmptyLabel errors
    #[inline(always)]
    fn error_empty_label(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::EmptyLabel,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_empty_label_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::EmptyLabel,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // UnknownDirective errors
    #[inline(always)]
    fn error_unknown_directive(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownDirective,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_unknown_directive_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownDirective,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // InvalidNumber errors
    #[inline(always)]
    fn error_invalid_number(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::InvalidNumber,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_invalid_number_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::InvalidNumber,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // MacroArgCount errors
    #[inline(always)]
    fn error_macro_arg_count(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroArgCount,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_macro_arg_count_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroArgCount,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // MacroRecursion errors
    #[inline(always)]
    fn error_macro_recursion(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroRecursion,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_macro_recursion_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroRecursion,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // UnknownParam errors
    #[inline(always)]
    fn error_unknown_param(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownParam,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_unknown_param_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::UnknownParam,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // ExpectedSectionName errors
    #[inline(always)]
    fn error_expected_section_name(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpectedSectionName,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_expected_section_name_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpectedSectionName,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    // FileNotFound errors
    #[inline(always)]
    fn error_file_not_found(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::FileNotFound,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_file_not_found_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::FileNotFound,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline(always)]
    fn error_expected_comma_or_end(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpectedCommaOrEnd,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_expected_comma_or_end_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpectedCommaOrEnd,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline(always)]
    fn error_empty_include_path(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::EmptyIncludePath,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_macro_invalid_header(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroInvalidHeader,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_macro_unclosed(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroUnclosed,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_macro_unclosed_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::MacroUnclosed,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline(always)]
    fn error_handling_instruction(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::HandlingInstruction,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_handling_instruction_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::HandlingInstruction,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline(always)]
    fn error_parsing_macro(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ParsingMacro,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_parsing_macro_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ParsingMacro,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline(always)]
    fn error_expanding_macro(&self) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpandingMacro,
            src: self.curr_line,
            msg: None,
        }
    }

    #[inline(always)]
    fn error_expanding_macro_with_msg(&self, msg: impl Into<Box<str>>) -> Error {
        Error {
            loc: self.loc_display(),
            kind: ErrorKind::ExpandingMacro,
            src: self.curr_line,
            msg: Some(msg.into()),
        }
    }

    #[inline]
    fn ensure_comma_or_end(&self, s: &str) -> Result<()> {
        let i = skip_whitespace(s);
        let bytes = s.as_bytes();
        if i >= bytes.len() || bytes[i] == b',' {
            Ok(())
        } else {
            Err(self.error_expected_comma_or_end_with_msg(
                format!("expected ',' or end, got: {}", &s[i..])
            ))
        }
    }

    #[inline]
    fn parse_i<T>(&self, s: &str) -> Result<T>
    where
        T: Num,
        <T as Num>::FromStrRadixErr: fmt::Display
    {
        let s = &s[skip_whitespace(s)..];
        let ((tok, rest), radix) = if let Some(rest) = s.strip_prefix("0x") {
            (take_hex(rest), 16)
        } else {
            (take_signed(s), 10)
        };
        self.ensure_comma_or_end(rest)?;
        T::from_str_radix(tok, radix).map_err(|e| {
            self.error_invalid_number_with_msg(format!(
                "couldn't parse {tok:?} to {ty:?}: {e}",
                ty = std::any::type_name::<T>()
            ))
        })
    }

    #[inline]
    fn encoding_list<T>(
        &mut self,
        input: &str,
        sepa: u8,
        mut f: impl FnMut(&mut Self, T)
    ) -> Result<()>
    where
        T: Num + str::FromStr,
        <T as Num>::FromStrRadixErr: fmt::Display,
        <T as str::FromStr>::Err: Send + Sync + fmt::Display + std::error::Error + 'static
    {
        let bytes = input.as_bytes();
        let mut byte_offset = 0;
        while byte_offset < bytes.len() {
            let end = memchr::memchr(sepa, &bytes[byte_offset..])
                .map(|pos| byte_offset + pos)
                .unwrap_or(bytes.len());
            let item = input[byte_offset..end].trim();
            if !item.is_empty() {
                let val = self.parse_i(item)?;
                f(self, val);
            }
            byte_offset = end + 1;
        }
        Ok(())
    }
}
