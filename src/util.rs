#[inline]
pub fn unescape_string_(escaped: &str, ret: &mut String) {
    let mut chars = escaped.chars().peekable();

    while let Some(c) = chars.next() {
        if c != '\\' {
            ret.push(c);
            continue
        }

        let Some(&next) = chars.peek() else {
            ret.push(c);
            continue
        };

        match next {
            'n'  => { ret.push('\n'); chars.next(); },
            't'  => { ret.push('\t'); chars.next(); },
            'r'  => { ret.push('\r'); chars.next(); },
            '\\' => { ret.push('\\'); chars.next(); },
            '"'  => { ret.push('"');  chars.next(); },
            '\'' => { ret.push('\''); chars.next(); },
            '0'  => { ret.push('\0'); chars.next(); },
            _ => ret.push(c)
        }
    }
}

#[inline]
pub fn unescape_string(escaped: &str) -> String {
    let mut ret = String::with_capacity(escaped.len());
    unescape_string_(escaped, &mut ret);
    ret
}

macro_rules! maybe_reloc {
    // rd + imm
    ($self:ident, $ctor:ident, rd=$rd:expr, imm=$imm:expr, kind=$kind:expr) => {{
        #[allow(unused_imports)]
        use brik::{
            rv32::I32::*,
            rv64::I64::*,
            asm::reloc::*
        };

        match $imm {
            Imm::Int(val) => {
                $self.emit_bytes($ctor { d: $rd, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes($ctor { d: $rd, im: 0 });
                let section = $self.expect_curr_section();
                $self.add_reloc(
                    section,
                    Reloc { offset, symbol: sym, addend, rtype: $kind },
                );
            }
        }
    }};

    // rd + rs1 + imm
    ($self:ident, $ctor:ident, rd=$rd:expr, rs1=$rs1:expr, imm=$imm:expr, kind=$kind:expr) => {{
        #[allow(unused_imports)]
        use brik::{
            rv32::I32::*,
            rv64::I64::*,
            asm::reloc::*
        };

        match $imm {
            Imm::Int(val) => {
                $self.emit_bytes($ctor { d: $rd, s: $rs1, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes($ctor { d: $rd, s: $rs1, im: 0 });
                let section = $self.expect_curr_section();
                $self.add_reloc(
                    section,
                    Reloc { offset, symbol: sym, addend, rtype: $kind },
                );
            }
        }
    }};

    // rs1 + rs2 + imm (branches)
    ($self:ident, $ctor:ident, rs1=$rs1:expr, rs2=$rs2:expr, imm=$imm:expr, kind=$kind:expr) => {{
        #[allow(unused_imports)]
        use brik::{
            rv32::I32::*,
            rv64::I64::*,
            asm::reloc::*
        };

        match $imm {
            Imm::Int(val) => {
                $self.emit_bytes($ctor { s1: $rs1, s2: $rs2, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes($ctor { s1: $rs1, s2: $rs2, im: 0 });
                let section = $self.expect_curr_section();
                $self.add_reloc(
                    section,
                    Reloc { offset, symbol: sym, addend, rtype: $kind },
                );
            }
        }
    }};
}
