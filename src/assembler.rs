use crate::encoder::Encoder;
use crate::mnemonic::Mnemonic;
use crate::parse::{
    parse_i,
    take_string,
    strip_comment,
    split_at_space,
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
        path: &'a Path,
        src: &'a str,
    ) -> anyhow::Result<Object<'a>> {
        self.enc.position_at_end(self.sections.text);

        let mut byte_offset = 0;

        for nl in Memchr::new(b'\n', src.as_bytes()) {
            let line = &src[byte_offset..nl];

            byte_offset = nl + 1;
            self.line_number += 1;

            let line = line.trim();

            let Some(&first_byte) = line.as_bytes().first() else {
                continue
            };

            if first_byte == b';' {
                continue
            }

            let line = strip_comment(line);

            if line.is_empty() { continue }

            if matches!(line.as_bytes().last(), Some(b':')) {
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

                continue
            }

            if first_byte == b'.' {
                self.handle_directive(path, line)?;
                continue
            }

            let (mn, rest) = split_at_space(line);
            let m = Mnemonic::try_from_str(mn)
                .ok_or_else(|| anyhow::anyhow!("unknown mnemonic: {mn}"))?;

            self.enc.encode_inst(m, rest)?;
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
        path: &'a Path,
        line: &'a str
    ) -> anyhow::Result<()> {
        let (dir, rest) = split_at_space(line);

        let get_name = || {
            let name = rest.trim();
            if name.is_empty() {
                bail_at!{
                    path.display(),
                    self.line_number,
                    "expected section name"
                }
            }

            Ok(name)
        };

        match dir.as_bytes() {
            b".text"   => self.enc.position_at_end(self.sections.text),
            b".data"   => self.enc.position_at_end(self.sections.data),
            b".rodata" => self.enc.position_at_end(self.sections.rodata),
            b".bss"    => self.enc.position_at_end(self.sections.bss),

            b".section" => {
                let name = get_name()?;
                match name.as_bytes() {
                    b".text"   => self.enc.position_at_end(self.sections.text),
                    b".data"   => self.enc.position_at_end(self.sections.data),
                    b".rodata" => self.enc.position_at_end(self.sections.rodata),
                    b".bss"    => self.enc.position_at_end(self.sections.bss),
                    other      => _ = self.enc.add_section_at_end(
                        StandardSegment::Data,
                        other,
                        SectionKind::Data
                    )
                }
            }

            b".extern" | b".extrn" => {
                let name = get_name()?;
                self.enc.add_symbol_extern(
                    name,
                    SymbolKind::Text,
                    SymbolScope::Dynamic
                );
            }

            b".global" | b".globl" => {
                let name = get_name()?;
                let lbl_id = self.enc.get_or_declare_label(
                    name,
                    SymbolKind::Text,
                    SymbolScope::Compilation
                );
                self.enc.make_label_global(lbl_id);
            }

            b".ascii" => {
                let (str, _) = take_string(rest);
                self.enc.emit_str(&str[1..]);
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            b".asciiz" => {
                let (str, _) = take_string(rest);
                self.enc.emit_str(&str[1..]);
                self.enc.emit_byte(0);
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            b".byte" => {
                let byte = parse_i::<u8>(rest)?;
                self.enc.emit_byte(byte);
                self.enc.edit_curr_label_sym(|s| {
                    s.kind = SymbolKind::Data;
                });
            }

            _ => bail_at!(path.display(), self.line_number, "unknown directive {dir}")
        }

        Ok(())
    }
}
