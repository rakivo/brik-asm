use crate::mnemonic::Mnemonic::{self, *};
use crate::parse::{
    parse_i,
    parse_u8,
    parse_i16,
    parse_reg,
    take_ident,
    take_number,
    ensure_empty,
};

use std::ops::{Deref, DerefMut};

use brik::rv32::I32;
use brik::asm::Assembler;
use brik::asm::errors::FinishError;
use brik::object::{SymbolKind, SymbolScope};
use brik::object::write::{Object, SymbolId};

use anyhow::{bail, Result};

pub enum Imm {
    Int(i64), // can be any size
    Sym {
        sym: SymbolId,
        addend: i64
    }
}

#[repr(transparent)]
pub struct Encoder<'a>(pub(crate) Assembler<'a>);

impl<'a> Deref for Encoder<'a> {
    type Target = Assembler<'a>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Encoder<'_> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Encoder<'a> {
    #[inline(always)]
    pub fn finish(self) -> Result<Object<'a>, FinishError> {
        self.0.finish()
    }

    pub fn encode_inst(
        &mut self,
        m: Mnemonic,
        operands: &str
    ) -> Result<()> {
        match m {
            SD => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_sd(s1, s2, im);
            }
            LD => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_ld(d, s, im);
            }
            ADDI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                maybe_reloc!(self, ADDI, rd=rd, rs1=rs1, imm=imm, kind=RelocKind::PcrelLo12I);

            }
            EBREAK => {
                self.emit_bytes(I32::EBREAK {});
            }
            ECALL => {
                self.emit_bytes(I32::ECALL {});
            }
            ADD => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (rs2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_add(rd, rs1, rs2);
            }
            LUI => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                maybe_reloc!(self, LUI, rd=rd, imm=imm, kind=RelocKind::PcrelHi20);
            }
            LA => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                match imm {
                    Imm::Sym { sym, .. } => {
                        // TODO: pass addend here
                        self.emit_pcrel_load_addr(rd, sym);
                    }
                    Imm::Int(val) => {
                        let hi = (val + 0x800) >> 12;
                        let lo = val - (hi << 12);
                        self.emit_bytes(I32::LUI { d: rd, im: hi as i32 });
                        self.emit_bytes(I32::ADDI { d: rd, s: rd, im: lo as _ });
                    }
                }
            }
            AUIPC => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                maybe_reloc!(self, AUIPC, rd=rd, imm=imm, kind=RelocKind::PcrelHi20);
            }
            JAL => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                maybe_reloc!(self, JAL, rd=rd, imm=imm, kind=RelocKind::Jal);
            }
            JALR => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                match imm {
                    Imm::Int(val) => {
                        self.emit_bytes(I32::JALR { d: rd, s: rs1, im: val as _ });
                    }
                    Imm::Sym { .. } => bail!("jalr sym not supported directly")
                }
            }
            ANDI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                match imm {
                    Imm::Int(val) => {
                        self.emit_bytes(I32::ANDI { d: rd, s: rs1, im: val as _ });
                    }
                    Imm::Sym { .. } => bail!("jalr sym not supported directly")
                }
            }
            ORI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                match imm {
                    Imm::Int(val) => {
                        self.emit_bytes(I32::ORI { d: rd, s: rs1, im: val as _ });
                    }
                    Imm::Sym { .. } => bail!("jalr sym not supported directly")
                }
            }
            XORI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest)?;
                match imm {
                    Imm::Int(val) => {
                        self.emit_bytes(I32::XORI { d: rd, s: rs1, im: val as _ });
                    }
                    Imm::Sym { .. } => bail!("jalr sym not supported directly")
                }
            }
            SLLI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let sh = parse_u8(rest)?;
                self.emit_bytes(I32::SLLI { d: rd, s: rs1, shamt: sh });
            }
            SRLI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let sh = parse_u8(rest)?;
                self.emit_bytes(I32::SRLI { d: rd, s: rs1, shamt: sh });
            }
            SRAI => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let sh = parse_u8(rest)?;
                self.emit_bytes(I32::SRAI { d: rd, s: rs1, shamt: sh });
            }
        };

        Ok(())
    }

    #[inline]
    fn lookup_or_intern_symbol(&mut self, name: &str) -> SymbolId {
        if let Some(id) = self.symbol_id(name.as_bytes()) {
            return id
        }

        self.add_symbol_extern(
            name,
            SymbolKind::Data,
            SymbolScope::Compilation
        )
    }

    fn try_parse_imm<'b>(&mut self, s: &'b str) -> Result<(Imm, &'b str)> {
        let s = s.trim();

        // Case 1: starts with a digit or minus → parse number
        if let Some(first) = s.chars().next() {
            if first.is_ascii_digit() || first == '-' {
                let (num, rest) = take_number(s);
                let num = parse_i(num)?;
                return Ok((Imm::Int(num), rest));
            }
        }

        // Case 2: parse symbol name (alnum + '_' allowed)
        let (sym_str, rest) = take_ident(s);
        let sym = self.lookup_or_intern_symbol(sym_str);

        let mut addend = 0;
        let mut rest2 = rest.trim_start();
        if rest2.starts_with('+') || rest2.starts_with('-') {
            let sign = if rest2.starts_with('+') { 1 } else { -1 };
            rest2 = &rest2[1..];
            let (num, rest3) = take_number(rest2);
            addend = sign * parse_i::<i64>(num)?;
            rest2 = rest3;
        }

        Ok((Imm::Sym { sym, addend }, rest2))
    }
}

