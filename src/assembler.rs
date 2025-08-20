use crate::encoder::Encoder;
use crate::mnemonic::Mnemonic;
use crate::parse::{
    parse_i,
    with_cs_list,
    take_string,
    split_at_space,
    skip_whitespace
};

use std::path::Path;

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
use anyhow::Context;

#[derive(Copy, Clone)]
pub struct Sections {
    pub text   : SectionId,
    pub data   : SectionId,
    pub rodata : SectionId,
    pub bss    : SectionId,
}

pub struct Assembler<'a> {
    enc: Encoder<'a>,
    sections: Sections,

    line_number: usize
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
            line_number: 0
        }
    }

    pub fn assemble_file(
        mut self,
        path: &Path,
        src: &str
    ) -> anyhow::Result<Object<'a>> {
        self.enc.position_at_end(self.sections.text);

        let mut byte_offset = 0;

        let path_display = path.display().to_string();

        for nl in Memchr::new(b'\n', src.as_bytes()) {
            let line = &src[byte_offset..nl];

            byte_offset = nl + 1;
            self.line_number += 1;

            let line = line.strip_suffix('\n').unwrap_or(line);
            let line = &line[skip_whitespace(line)..];

            if line.is_empty() {
                continue;
            }

            let line_bytes = line.as_bytes();
            let first_byte = *unsafe { line_bytes.get_unchecked(0) };

            if first_byte == b';' { continue }

            let line = match line.rfind(';') {
                Some(pos) => line[..pos].trim_end(),
                None => line
            };

            if line.is_empty() { continue }

            if line_bytes[line_bytes.len() - 1] == b':' {
                let lbl = &line[..line.len() - 1].trim();

                if lbl.is_empty() {
                    bail_at!(path.display(), self.line_number, "empty label")
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

            let fl_context = |asm: &Self| format!{
                "{path_display}:{l}:",
                l = asm.line_number
            };

            if first_byte == b'.' {
                self.handle_directive(path, line)
                    .with_context(|| fl_context(&self))?;

                continue
            }

            let (mn, rest) = split_at_space(line);
            let m = Mnemonic::try_from_str(mn)
                .ok_or_else(|| anyhow::anyhow!("unknown mnemonic: {mn}"))?;

            self.enc
                .encode_inst(m, rest)
                .with_context(|| fl_context(&self))?;
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
        path: &Path,
        line: &str
    ) -> anyhow::Result<()> {
        let line_bytes = line.as_bytes();
        let path_display = path.display();

        // Find space position once
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
                bail_at!{
                    path_display,
                    self.line_number,
                    "expected section name"
                }
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
                with_cs_list(rest, |v| _ = self.enc.emit_byte(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"hword" => {
                self.enc.align_to(2);
                with_cs_list(rest, |v| _ = self.enc.emit_half(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"word" => {
                self.enc.align_to(4);
                with_cs_list(rest, |v| _ = self.enc.emit_word(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            b"dword" => {
                self.enc.align_to(8);
                with_cs_list(rest, |v| _ = self.enc.emit_dword(v))?;
                self.enc.edit_curr_label_sym(|s| s.kind = SymbolKind::Data);
            }

            _ => {
                let dir_str = str::from_utf8(directive)
                    .unwrap_or("<invalid utf8>");

                bail_at!{
                    path_display,
                    self.line_number,
                    "unknown directive .{dir_str}"
                }
            }
        }

        Ok(())
    }
}
