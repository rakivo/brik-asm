use crate::loc::Loc;
use crate::fm::FileManager;
use crate::sym::SymInterner;
use crate::mnemonic::Mnemonic::{self, *};
use crate::parse::{
    parse_i,
    parse_u8,
    parse_i16,
    parse_reg,
    parse_aqrl,
    take_ident,
    take_number,
    ensure_empty,
};

use std::fmt::{self, Write};
use std::ops::{Deref, DerefMut};

use brik::rv64::I64;
use brik::rv32::I32;
use brik::rv32::Reg::*;
use brik::asm::Assembler;
use brik::asm::label::LabelId;
use brik::object::{SymbolKind, SymbolScope};
use brik::object::write::{Object, SymbolId};

use alive_map::AliveMap;
use anyhow::{bail, Result};

pub enum Imm {
    Int(i64), // can be any size
    Sym {
        sym: SymbolId,
        addend: i64
    }
}

pub struct UndefSym {
    loc: Loc
}

pub struct UndefSymsDiagnostics {
    rendered: String,
}

pub enum FinishError {
    Assembler(UndefSymsDiagnostics),
    Encoder(brik::asm::errors::FinishError)
}

impl fmt::Display for FinishError {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Encoder(e) => e.fmt(f),
            Self::Assembler(e) => e.rendered.fmt(f)
        }
    }
}

pub struct Encoder<'a> {
    asm: Assembler<'a>,
    sin: SymInterner,

    undef_syms: AliveMap<SymbolId, UndefSym>
}

impl<'a> Deref for Encoder<'a> {
    type Target = Assembler<'a>;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.asm
    }
}

impl DerefMut for Encoder<'_> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.asm
    }
}

impl<'a> Encoder<'a> {
    #[inline]
    pub fn new(asm: Assembler<'a>) -> Self {
        Self {
            asm,
            sin: SymInterner::new(),
            undef_syms: AliveMap::new()
        }
    }

    #[inline(always)]
    pub fn finish(self, fm: &FileManager) -> Result<Object<'a>, FinishError> {
        if self.undef_syms.is_empty() {
             return self.asm.finish().map_err(FinishError::Encoder)
        }

        let mut rendered = String::new();

        for (&id, UndefSym { loc }) in self.undef_syms.iter() {
            writeln!{
                rendered,
                "{loc}: symbol '{name}' is undefined",
                loc = loc.display(fm),
                name = unsafe {
                    str::from_utf8_unchecked(&self.symbol(id).name)
                }
            }.expect("[could not write undef labels diagnostics]");
        }

        Err(FinishError::Assembler(UndefSymsDiagnostics { rendered }))
    }

    #[inline(always)]
    pub fn symbol_id(&self, name: &[u8]) -> Option<SymbolId> {
        self.sin.get(name)
    }

    #[inline(always)]
    pub fn mark_sym_defined(&mut self, id: SymbolId) {
        _ = self.undef_syms.remove(&id)
    }

    #[inline(always)]
    pub fn intern_sym(
        &mut self,
        name: impl AsRef<[u8]>,
        sym_id: SymbolId
    ) {
        self.sin.intern(name.as_ref(), sym_id);
    }

    #[allow(unused)]
    #[inline(always)]
    pub fn intern_label(
        &mut self,
        name: impl AsRef<[u8]>,
        lbl_id: LabelId
    ) {
        let sym_id = self.get_label(lbl_id).sym;
        self.intern_sym(name, sym_id);
    }

    pub fn encode_inst(
        &mut self,
        m: Mnemonic,
        operands: &str,
        loc: Loc
    ) -> Result<()> {
        match m {
            SD => {
                let (s2, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
                maybe_reloc!(self, LUI, rd=rd, imm=imm, kind=RelocKind::PcrelHi20);
            }
            LI => {
                let (rd, rest) = parse_reg(operands)?;
                let im = parse_i::<i64>(rest)?;
                self.emit_li(rd, im);
            }
            LA => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
                match imm {
                    Imm::Sym { sym, addend } => {
                        self.emit_pcrel_load_addr(rd, sym, addend);
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
                maybe_reloc!(self, AUIPC, rd=rd, imm=imm, kind=RelocKind::PcrelHi20);
            }
            CALL => {
                if let Ok((s, _)) = parse_reg(operands) {
                    self.emit_bytes(
                        I32::JALR { d: RA, s, im: 0 }
                    );
                } else {
                    let (imm, _rest) = self.try_parse_imm(operands, loc)?;
                    match imm {
                        Imm::Int(val) => {
                            let rd = T0;
                            self.emit_li(rd, val);
                            self.emit_bytes(
                                I32::JALR { d: RA, s: rd, im: 0 }
                            );
                        }
                        Imm::Sym { sym, .. } => {
                            self.emit_call_plt(sym);
                        }
                    }
                }
            }
            JAL => {
                let (rd, rest) = parse_reg(operands)?;
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
                maybe_reloc!(self, JAL, rd=rd, imm=imm, kind=RelocKind::Jal);
            }
            JALR => {
                let (rd, rest) = parse_reg(operands)?;
                let (rs1, rest) = parse_reg(rest)?;
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
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
                let (imm, _rest) = self.try_parse_imm(rest, loc)?;
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
            // Branch instructions
            BEQ => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BEQ, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }
            BNE => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BNE, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }
            BLT => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BLT, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }
            BGE => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BGE, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }
            BLTU => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BLTU, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }
            BGEU => {
                let (s1, rest) = parse_reg(operands)?;
                let (s2, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, BGEU, rs1=s1, rs2=s2, imm=imm, kind=RelocKind::Branch);
            }

            // Load instructions
            LB => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::LB { d, s, im });
            }
            LH => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::LH { d, s, im });
            }
            LW => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::LW { d, s, im });
            }
            LBU => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::LBU { d, s, im });
            }
            LHU => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::LHU { d, s, im });
            }
            LWU => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I64::LWU { d, s, im });
            }

            // Store instructions
            SB => {
                let (s2, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::SB { s1, s2, im });
            }
            SH => {
                let (s2, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::SH { s1, s2, im });
            }
            SW => {
                let (s2, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let im = parse_i16(rest)?;
                self.emit_bytes(I32::SW { s1, s2, im });
            }

            // Immediate arithmetic/logic instructions
            SLTI => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, SLTI, rd=d, rs1=s, imm=imm, kind=RelocKind::PcrelLo12I);
            }
            SLTIU => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, SLTIU, rd=d, rs1=s, imm=imm, kind=RelocKind::PcrelLo12I);
            }

            // Register-register operations
            SUB => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SUB { d, s1, s2 });
            }
            SLL => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SLL { d, s1, s2 });
            }
            SRL => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SRL { d, s1, s2 });
            }
            SRA => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SRA { d, s1, s2 });
            }
            SLT => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SLT { d, s1, s2 });
            }
            SLTU => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::SLTU { d, s1, s2 });
            }
            XOR => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::XOR { d, s1, s2 });
            }
            OR => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::OR { d, s1, s2 });
            }
            AND => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::AND { d, s1, s2 });
            }

            // Fence instruction
            FENCE => {
                let im = parse_i16(operands)?;
                self.emit_bytes(I32::FENCE { im });
            }

            // M-extension instructions
            MUL => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::MUL { d, s1, s2 });
            }
            MULH => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::MULH { d, s1, s2 });
            }
            MULHSU => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::MULHSU { d, s1, s2 });
            }
            MULHU => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::MULHU { d, s1, s2 });
            }
            DIV => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::DIV { d, s1, s2 });
            }
            DIVU => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::DIVU { d, s1, s2 });
            }
            REM => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::REM { d, s1, s2 });
            }
            REMU => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I32::REMU { d, s1, s2 });
            }

            // A-extension Load-Reserved/Store-Conditional (32-bit)
            LR_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::LR_W { d, s1, aqrl });
            }
            SC_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::SC_W { d, s1, s2, aqrl });
            }

            // A-extension Atomic Memory Operations (32-bit)
            AMOADD_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOADD_W { d, s1, s2, aqrl });
            }
            AMOSWAP_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOSWAP_W { d, s1, s2, aqrl });
            }
            AMOAND_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOAND_W { d, s1, s2, aqrl });
            }
            AMOOR_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOOR_W { d, s1, s2, aqrl });
            }
            AMOXOR_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOXOR_W { d, s1, s2, aqrl });
            }
            AMOMAX_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOMAX_W { d, s1, s2, aqrl });
            }
            AMOMIN_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOMIN_W { d, s1, s2, aqrl });
            }
            AMOMAXU_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOMAXU_W { d, s1, s2, aqrl });
            }
            AMOMINU_W => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I32::AMOMINU_W { d, s1, s2, aqrl });
            }

            // RV64I-specific instructions
            ADDW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::ADDW { d, s1, s2 });
            }
            SUBW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::SUBW { d, s1, s2 });
            }
            ADDIW => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let (imm, rest) = self.try_parse_imm(rest, loc)?;
                ensure_empty(rest)?;
                maybe_reloc!(self, ADDIW, rd=d, rs1=s, imm=imm, kind=RelocKind::PcrelLo12I);
            }
            SLLW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::SLLW { d, s1, s2 });
            }
            SRLW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::SRLW { d, s1, s2 });
            }
            SRAW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::SRAW { d, s1, s2 });
            }
            SLLIW => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let shamt = parse_u8(rest)?;
                self.emit_bytes(I64::SLLIW { d, s, shamt });
            }
            SRLIW => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let shamt = parse_u8(rest)?;
                self.emit_bytes(I64::SRLIW { d, s, shamt });
            }
            SRAIW => {
                let (d, rest) = parse_reg(operands)?;
                let (s, rest) = parse_reg(rest)?;
                let shamt = parse_u8(rest)?;
                self.emit_bytes(I64::SRAIW { d, s, shamt });
            }

            // RV64 M-extension
            MULW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::MULW { d, s1, s2 });
            }
            DIVW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::DIVW { d, s1, s2 });
            }
            DIVUW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::DIVUW { d, s1, s2 });
            }
            REMW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::REMW { d, s1, s2 });
            }
            REMUW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::REMUW { d, s1, s2 });
            }
            MULHW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::MULHW { d, s1, s2 });
            }
            MULHSUW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::MULHSUW { d, s1, s2 });
            }
            MULHUW => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                ensure_empty(rest)?;
                self.emit_bytes(I64::MULHUW { d, s1, s2 });
            }

            // RV64 A-extension Load-Reserved/Store-Conditional (64-bit)
            LR_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::LR_D { d, s1, aqrl });
            }
            SC_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::SC_D { d, s1, s2, aqrl });
            }

            // RV64 A-extension Atomic Memory Operations (64-bit)
            AMOADD_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOADD_D { d, s1, s2, aqrl });
            }
            AMOSWAP_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOSWAP_D { d, s1, s2, aqrl });
            }
            AMOAND_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOAND_D { d, s1, s2, aqrl });
            }
            AMOOR_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOOR_D { d, s1, s2, aqrl });
            }
            AMOXOR_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOXOR_D { d, s1, s2, aqrl });
            }
            AMOMAX_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOMAX_D { d, s1, s2, aqrl });
            }
            AMOMIN_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOMIN_D { d, s1, s2, aqrl });
            }
            AMOMAXU_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOMAXU_D { d, s1, s2, aqrl });
            }
            AMOMINU_D => {
                let (d, rest) = parse_reg(operands)?;
                let (s1, rest) = parse_reg(rest)?;
                let (s2, rest) = parse_reg(rest)?;
                let aqrl = parse_aqrl(rest)?;
                self.emit_bytes(I64::AMOMINU_D { d, s1, s2, aqrl });
            }
        };

        Ok(())
    }

    #[inline]
    fn lookup_or_intern_symbol(
        &mut self,
        name: impl AsRef<[u8]>,
        loc: Loc
    ) -> SymbolId {
        let name = name.as_ref();

        if let Some(id) = self.sin.get(name) { return id }

        let id = self.add_symbol_extern(
            name,
            SymbolKind::Data,
            SymbolScope::Compilation
        );

        self.sin.intern(name, id);

        self.undef_syms.insert(id, UndefSym { loc });

        id
    }

    fn try_parse_imm<'b>(
        &mut self,
        s: &'b str,
        loc: Loc
    ) -> Result<(Imm, &'b str)> {
        let s = s.trim();

        // Case 1: starts with a digit or minus → parse number
        if let Some(first) = s.chars().next() {
            #[allow(clippy::collapsible_if)]
            if first.is_ascii_digit() || first == '-' {
                let (num, rest) = take_number(s);
                let num = parse_i(num)?;
                return Ok((Imm::Int(num), rest));
            }
        }

        // Case 2: parse symbol name (alnum + '_' allowed)
        let (sym_str, rest) = take_ident(s);
        let sym = self.lookup_or_intern_symbol(sym_str, loc);

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
