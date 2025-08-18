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
    SectionId,
    SymbolKind,
    SymbolScope,
    SectionKind,
    SymbolSection,
    StandardSegment,
};

#[derive(Copy, Clone)]
pub struct Sections {
    pub text   : SectionId,
    pub data   : SectionId,
    pub rodata : SectionId,
    pub bss    : SectionId,
}

pub fn assemble_file<'a>(
    path: &Path,
    src: &'a str,
    mut enc: Encoder<'a>,
    sect: &Sections,
) -> anyhow::Result<Encoder<'a>> {
    enc.position_at_end(sect.text);

    for (lineno, line) in src.split_inclusive('\n').enumerate() {
        let line = strip_comment(line);
        let line = line.trim();
        if line.is_empty() { continue }

        if let Some(lbl) = line.strip_suffix(':') {
            let name = lbl.trim();
            if name.is_empty() {
                bail_at!(path.display(), lineno, "empty label")
            }

            if let Some(sym_id) = enc.symbol_id(name.as_bytes()) {
                let curr_offset = enc.curr_offset();
                let curr_section = enc.expect_curr_section();
                enc.edit_sym(sym_id, |s| {
                    s.section = SymbolSection::Section(curr_section);
                    s.value = curr_offset;
                });
            }

            enc.place_or_add_label_here(
                name,
                SymbolKind::Text,
                SymbolScope::Compilation
            );

            continue
        }

        if matches!(line.as_bytes().first(), Some(b'.')) {
            handle_directive(path, lineno, line, &mut enc, sect)?;
            continue
        }

        let (mn, rest) = split_at_space(line);
        let m = Mnemonic::try_from_str(mn)
            .ok_or_else(|| anyhow::anyhow!("unknown mnemonic: {mn}"))?;

        enc.encode_inst(m, rest)?;
    }

    Ok(enc)
}

pub fn handle_directive<'a>(
    path: &Path,
    lineno: usize,
    line: &'a str,
    asm: &mut Encoder<'a>,
    sect: &Sections,
) -> anyhow::Result<()> {
    let (dir, rest) = split_at_space(line);

    let get_name = || {
        let name = rest.trim();
        if name.is_empty() {
            bail_at!(path.display(), lineno, "expected section name")
        }

        Ok(name)
    };

    match dir.as_bytes() {
        b".text"   => asm.position_at_end(sect.text),
        b".data"   => asm.position_at_end(sect.data),
        b".rodata" => asm.position_at_end(sect.rodata),
        b".bss"    => asm.position_at_end(sect.bss),

        b".section" => {
            let name = get_name()?;
            match name.as_bytes() {
                b".text"   => asm.position_at_end(sect.text),
                b".data"   => asm.position_at_end(sect.data),
                b".rodata" => asm.position_at_end(sect.rodata),
                b".bss"    => asm.position_at_end(sect.bss),
                other      => _ = asm.add_section_at_end(
                    StandardSegment::Data,
                    other,
                    SectionKind::Data
                )
            }
        }

        b".extern" | b".extrn" => {
            let name = get_name()?;
            asm.add_symbol_extern(
                name,
                SymbolKind::Text,
                SymbolScope::Dynamic
            );
        }

        b".global" | b".globl" => {
            let name = get_name()?;
            let lbl_id = asm.get_or_declare_label(
                name,
                SymbolKind::Text,
                SymbolScope::Compilation
            );
            asm.make_label_global(lbl_id);
        }

        b".ascii" => {
            let (str, _) = take_string(rest);
            asm.emit_str(&str[1..]);
            asm.edit_curr_label_sym(|s| {
                s.kind = SymbolKind::Data;
            });
        }

        b".asciiz" => {
            let (str, _) = take_string(rest);
            asm.emit_str(&str[1..]);
            asm.emit_byte(0);
            asm.edit_curr_label_sym(|s| {
                s.kind = SymbolKind::Data;
            });
        }

        b".byte" => {
            let byte = parse_i::<u8>(rest)?;
            asm.emit_byte(byte);
            asm.edit_curr_label_sym(|s| {
                s.kind = SymbolKind::Data;
            });
        }

        _ => bail_at!(path.display(), lineno, "unknown directive {dir}")
    }

    Ok(())
}
