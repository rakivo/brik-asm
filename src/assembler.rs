use crate::encoder::Encoder;
use crate::mnemonic::Mnemonic;
use crate::parse::{
    parse_i,
    take_string,
    parsing_list,
    split_at_space,
    skip_whitespace,
    first_token_and_rest
};

use std::rc::Rc;
use std::path::Path;
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
use anyhow::{bail, Context};

const MACRO_RECURSION_LIMIT: usize = 64;

#[derive(Copy, Clone)]
pub struct Sections {
    pub text   : SectionId,
    pub data   : SectionId,
    pub rodata : SectionId,
    pub bss    : SectionId,
}

struct MacroDef {
    params: Rc<[Box<str>]>,
    body: Rc<str>,
    defined_at_line: u32,
    __pad: u32
}

struct InputLine {
    content: NonNull<u8>,
    len: u32,
    src_line_number: u32
}

impl InputLine {
    #[inline(always)]
    pub const fn empty(line_number: u32) -> Self {
        Self {
            content: NonNull::dangling(),
            len: 0,
            src_line_number: line_number
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
        content: NonNull<u8>,
        len: usize,
        pos: usize,
        line_number: u32
    },
    MacroExpansion {
        content: Box<str>,
        pos: usize,
        line_number: u32,
        original_line: u32
    },
}

impl InputSource {
    #[inline(always)]
    const fn file_from_str(s: &str) -> Self {
        Self::File {
            content: unsafe {
                NonNull::new(s.as_ptr() as _).unwrap_unchecked()
            },

            len: s.len(),
            pos: 0,
            line_number: 1
        }
    }

    // Returns (line, should_advance)
    fn peek_line(&self) -> Option<(InputLine, bool)> {
        match self {
            Self::File { content, len, pos, line_number } => {
                if *pos >= *len {
                    return Some(
                        (InputLine::empty(*line_number), false)
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
                    src_line_number: *line_number
                };

                Some((line, true))
            }

            Self::MacroExpansion { content, pos, original_line, .. } => {
                if *pos >= content.len() {
                    return Some(
                        (InputLine::empty(*original_line), false)
                    )
                }

                let rem = &content[*pos..];
                let end = memchr::memchr(b'\n', rem.as_bytes())
                    .unwrap_or(rem.len());

                let line = &rem[..end];

                let line = InputLine {
                    content: NonNull::new(line.as_ptr() as _).unwrap(),
                    len: line.len() as _,
                    src_line_number: *original_line
                };

                Some((line, true))
            }
        }
    }

    #[inline]
    fn advance(&mut self) {
        match self {
            Self::File { content, len, pos, line_number } => {
                if *pos >= *len { return; }

                unsafe {
                    let slice = slice::from_raw_parts(content.as_ptr(), *len);
                    if let Some(nl_pos) = memchr::memchr(b'\n', &slice[*pos..]) {
                        *pos += nl_pos + 1;
                    } else {
                        *pos = *len;
                    }
                    *line_number += 1;
                }
            }

            Self::MacroExpansion { content, pos, line_number, .. } => {
                if *pos >= content.len() { return; }

                if let Some(nl_pos) = memchr::memchr(b'\n', content[*pos..].as_bytes()) {
                    *pos += nl_pos + 1;
                } else {
                    *pos = content.len();
                }
                *line_number += 1;
            }
        }
    }
}

pub struct Assembler<'a> {
    enc: Encoder<'a>,
    sections: Sections,

    line_number: u32,

    macros: HashMap<String, MacroDef, wyhash::WyHasherBuilder>,
    input_stack: Vec<InputSource>,
}

impl<'a> Assembler<'a> {
    #[inline]
    pub fn new(mut enc: Encoder<'a>) -> Self {
        Self {
            sections: Sections {
                rodata : enc.add_rodata_section(),
                text   : enc.add_text_section(),
                data   : enc.add_data_section(),
                bss    : enc.add_bss_section(),
            },
            enc,
            line_number: u32::default(),
            macros: HashMap::default(),
            input_stack: Vec::default(),
        }
    }

    #[inline]
    fn get_next_line(&mut self) -> Option<InputLine> {
        loop {
            let last = self.input_stack.last_mut()?;

            let (line, should_advance) = last
                .peek_line()
                .unwrap_or((InputLine::empty(self.line_number), false));

            if !should_advance {
                // source exhausted, pop it
                _ = self.input_stack.pop();
                continue
            }

            // now advance the source
            last.advance();

            return Some(line)
        }
    }

    pub fn assemble_file(
        mut self,
        path: &Path,
        src: &str
    ) -> anyhow::Result<Object<'a>> {
        self.enc.position_at_end(self.sections.text);

        self.input_stack.push(InputSource::file_from_str(src));

        let path_display = path.display().to_string();

        while let Some(line) = self.get_next_line() {
            let line_number = line.src_line_number;
            self.line_number = line_number;

            let line = line.as_str();
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
                    bail!("empty label")
                }

                if let Some(sym_id) = self.enc.symbol_id(lbl.as_bytes()) {
                    let curr_offset = self.enc.curr_offset();
                    let curr_section = self.enc.expect_curr_section();
                    self.enc.edit_sym(sym_id, |s| {
                        s.section = SymbolSection::Section(curr_section);
                        s.value = curr_offset;
                    });
                }

                self.enc.place_or_add_label_here(
                    lbl,
                    SymbolKind::Text,
                    SymbolScope::Compilation
                );

                // should we intern labels as well?
                // self.enc.intern_label(lbl, lbl_id);

                continue
            }

            let fl_context = || format!{
                "{path_display}:{line_number}:",
            };

            if first_byte == b'.' {
                self.handle_directive(line)
                    .with_context(fl_context)?;

                continue
            }

            if line_bytes.starts_with(b"macro ") {
                self.handle_macro_definition(line)
                    .with_context(fl_context)?;

                continue
            }

            let (first_word, rest) = split_at_space(line);
            if self.macros.contains_key(first_word) {
                self.call_macro(first_word, rest)
                    .with_context(|| format!{
                        "expanding macro: '{first_word}'"
                    })?;

                continue
            }

            let (mn, rest) = split_at_space(line);
            let m = Mnemonic::try_from_str(mn)
                .ok_or_else(|| anyhow::anyhow!("unknown mnemonic: {mn}"))
                .with_context(fl_context)?;

            self.enc
                .encode_inst(m, rest)
                .with_context(fl_context)?;
        }

        self.enc
            .finish()
            .map_err(|e| anyhow::anyhow!(e.rendered))
            .with_context(|| format!{
                "generating object file for {path}",
                path = path.display()
            })
    }

    fn handle_directive(
        &mut self,
        line: &str
    ) -> anyhow::Result<()> {
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

        let get_name = || {
            if rest.is_empty() {
                bail!("expected section name")
            }

            Ok(rest)
        };

        match directive {
            b"text"   => self.enc.position_at_end(self.sections.text),
            b"data"   => self.enc.position_at_end(self.sections.data),
            b"rodata" => self.enc.position_at_end(self.sections.rodata),
            b"bss"    => self.enc.position_at_end(self.sections.bss),

            b"section" => {
                let name = get_name()?;
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
                let name = get_name()?;
                let (_, sym_id) = self.enc.edit_or_add_sym_and_edit_it(name, |s| {
                    s.section = SymbolSection::Undefined;
                    s.scope   = SymbolScope::Dynamic;
                    s.weak    = false;
                });
                self.enc.intern_sym(name, sym_id);
            }

            b"global" | b"globl" => {
                let name = get_name()?;
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
                let count = parse_i::<u64>(rest)?;
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
                parsing_list(rest, b' ', |v| _ = self.enc.emit_byte(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"hword" => {
                self.enc.align_to(2);
                parsing_list(rest, b' ', |v| _ = self.enc.emit_half(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"word" => {
                self.enc.align_to(4);
                parsing_list(rest, b' ', |v| _ = self.enc.emit_word(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"dword" => {
                self.enc.align_to(8);
                parsing_list(rest, b' ', |v| _ = self.enc.emit_dword(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            _ => {
                let dir_str = str::from_utf8(directive)
                    .unwrap_or("<invalid utf8>");

                bail!("unknown directive .{dir_str}")
            }
        }

        Ok(())
    }

     fn handle_macro_definition(&mut self, line: &str) -> anyhow::Result<()> {
        // "macro name [..param] {"
        let header = line
            .strip_prefix("macro ")
            .and_then(|s| s.strip_suffix("{"))
            .ok_or_else(|| anyhow::anyhow!("invalid macro header"))?;

        let mut parts = header.split_whitespace();

        let Some(name) = parts.next() else {
            bail!("macro name required");
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
            bail!("unclosed macro definition for '{name}'");
        }

        self.macros.insert(
            name.to_owned(),
            MacroDef {
                params,
                body: body.into(),
                defined_at_line: self.line_number,
                __pad: 0
            }
        );

        Ok(())
    }

    fn call_macro(&mut self, name: &str, args_str: &str) -> anyhow::Result<()> {
        let macro_def = self.macros.get(name).unwrap();
        let (params, body) = (
            Rc::clone(&macro_def.params),
            Rc::clone(&macro_def.body)
        );

        let args = args_str.split_whitespace()
            .map(|arg| arg.strip_suffix(',').unwrap_or(arg))
            .collect::<Vec<_>>();

        if args.len() != params.len() {
            let e0 = format!{
                "macro '{name}' expects {a} arguments, got {b}",
                a = params.len(),
                b = args.len()
            };

            let e1 = format!{
                "macro '{name}' defined here: {line}",
                line = macro_def.defined_at_line
            };

            bail!("{e0}\n{e1}")
        }

        let expanded = self.expand_macro_body(&body, &params, &args)?;

        self.input_stack.push(InputSource::MacroExpansion {
            content: expanded,
            pos: 0,
            line_number: 1,
            original_line: self.line_number,
        });

        Ok(())
    }

    #[inline]
    fn expand_macro_body(
        &mut self,
        body: &str,
        params: &[Box<str>],
        args: &[&str]
    ) -> anyhow::Result<Box<str>> {
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
    ) -> anyhow::Result<()> {
        if depth > MACRO_RECURSION_LIMIT {
            bail!("macro recursion limit ({MACRO_RECURSION_LIMIT}) exceeded");
        }

        let mut byte_offset = 0;

        for nl in Memchr::new(b'\n', body.as_bytes()) {
            let line = &body[byte_offset..nl];
            byte_offset = nl + 1;

            let sub = substitute_params_in_line(line, args_map)?;
            let sub_str = sub.as_ref();

            let (first_tok, rest_opt) = first_token_and_rest(sub_str);

            // check if first token is a macro
            let (Some(nested_def), Some(first_tok)) = (
                first_tok.and_then(|f| self.macros.get(f)),
                first_tok
            ) else {
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
                bail!{
                    "macro '{first_tok}' expects {a} arguments, got {b}",
                    a = nested_def.params.len(),
                    b = nested_args.len()
                }
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
}

/// replace $param occurrences in `line` using `repl` mapping.
/// only accepts identifiers `[A-Za-z0-9_]` after `$` for the name.
/// if a `$name` is not present in `sub_map`, it is left intact (keeps `$name`).
fn substitute_params_in_line<'a>(
    line: &'a str,
    sub_map: &HashMap<&str, &str>
) -> anyhow::Result<Cow<'a, str>> {
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
                bail!("unknown macro param: {name}")
            }
        } else {
            out.push(bytes[i] as char);
            i += 1;
        }
    }

    Ok(out.into())
}
