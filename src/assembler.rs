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

        // label?
        if let Some(lbl) = line.strip_suffix(':') {
            let name = lbl.trim();
            if name.is_empty() {
                bail_at!(path.display(), lineno, "empty label")
            }

            if let Some(sym_id) = enc.symbol_id(name.as_bytes()) {
                let curr_offset = enc.curr_offset();
                let curr_section = enc.expect_curr_section();
                let sym = &mut enc.symbol_mut(sym_id);
                sym.section = SymbolSection::Section(curr_section);
                sym.value = curr_offset;

                // for now all symbols are global
                sym.scope = SymbolScope::Linkage;
            } else if let Some(lbl_id) = enc.get_label_id(name) {
                enc.place_label_here(lbl_id);
                let sym_id = enc.get_label(lbl_id).sym;
                // for now all symbols are global
                enc.symbol_mut(sym_id).scope = SymbolScope::Linkage;
            } else {
                enc.add_label_here(
                    name,
                    SymbolKind::Text,
                    SymbolScope::Linkage
                );
            }

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
    match dir.as_bytes() {
        b".text"   => asm.position_at_end(sect.text),
        b".data"   => asm.position_at_end(sect.data),
        b".rodata" => asm.position_at_end(sect.rodata),
        b".bss"    => asm.position_at_end(sect.bss),

        b".section" => {
            let name = rest.trim();
            if name.is_empty() {
                bail_at!(path.display(), lineno, "expected section name")
            }

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

        b".ascii" => {
            let (str, _) = take_string(rest);
            asm.emit_str(&str[1..]);
        }

        b".byte" => {
            let byte = parse_i::<u8>(rest)?;
            asm.emit_byte(byte);
        }

        _ => bail_at!(path.display(), lineno, &format!("unknown directive {dir}"))
    }

    Ok(())
}
