macro_rules! bail_at {
    ($path:expr, $line:expr, $($err: tt)*) => {
        anyhow::bail!{
            "{f}:{l}: {e}",
            f = $path,
            l = $line,
            e = std::format_args!($($err)*)
        }
    };
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
                $self.emit_bytes(I32::$ctor { d: $rd, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes(I32::$ctor { d: $rd, im: 0 });
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
                $self.emit_bytes(I32::$ctor { d: $rd, s: $rs1, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes(I32::$ctor { d: $rd, s: $rs1, im: 0 });
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
                $self.emit_bytes(I32::$ctor { s1: $rs1, s2: $rs2, im: val as _ });
            }
            Imm::Sym { sym, addend } => {
                let offset = $self.curr_offset();
                $self.emit_bytes(I32::$ctor { s1: $rs1, s2: $rs2, im: 0 });
                let section = $self.expect_curr_section();
                $self.add_reloc(
                    section,
                    Reloc { offset, symbol: sym, addend, rtype: $kind },
                );
            }
        }
    }};
}
