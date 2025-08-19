#[derive(Clone, Copy, Debug)]
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
pub enum Mnemonic {
    CALL,
    ECALL,
    LA,
    ADDI,
    ADD,
    LUI,
    AUIPC,
    JAL,
    JALR,
    ANDI,
    ORI,
    XORI,
    SLLI,
    SRLI,
    SRAI,
    EBREAK,
    SD,
    LD,

    // New I32 (RV32I Base) instructions
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    SLTI,
    SLTIU,
    SLL,
    SRL,
    SRA,
    SUB,
    SLT,
    SLTU,
    XOR,
    OR,
    AND,
    FENCE,

    // M-extension instructions
    MUL,
    MULH,
    MULHSU,
    MULHU,
    DIV,
    DIVU,
    REM,
    REMU,

    // A-extension Load-Reserved/Store-Conditional
    LR_W,
    SC_W,

    // A-extension Atomic Memory Operations (32-bit)
    AMOADD_W,
    AMOSWAP_W,
    AMOAND_W,
    AMOOR_W,
    AMOXOR_W,
    AMOMAX_W,
    AMOMIN_W,
    AMOMAXU_W,
    AMOMINU_W,

    // RV64I Base instructions
    LWU,
    ADDW,
    SUBW,
    ADDIW,
    SLLW,
    SRLW,
    SRAW,
    SLLIW,
    SRLIW,
    SRAIW,

    // RV64 M-extension
    MULW,
    DIVW,
    DIVUW,
    REMW,
    REMUW,
    MULHW,
    MULHSUW,
    MULHUW,

    // RV64 A-extension Load-Reserved/Store-Conditional (64-bit)
    LR_D,
    SC_D,

    // RV64 A-extension Atomic Memory Operations (64-bit)
    AMOADD_D,
    AMOSWAP_D,
    AMOAND_D,
    AMOOR_D,
    AMOXOR_D,
    AMOMAX_D,
    AMOMIN_D,
    AMOMAXU_D,
    AMOMINU_D,

    // ...
}

use Mnemonic::*;

impl Mnemonic {
    #[inline(always)]
    pub fn try_from_str(s: &str) -> Option<Self> {
        let lowered = s.to_ascii_lowercase();
        Self::try_from_str_(lowered.as_bytes())
    }

    #[inline]
    pub const fn try_from_str_(s: &[u8]) -> Option<Self> {
        Some(match s {
            // ... existing cases ...
            b"ecall" => ECALL,
            b"la" => LA,
            b"addi" => ADDI,
            b"add" => ADD,
            b"lui" => LUI,
            b"auipc" => AUIPC,
            b"jal" => JAL,
            b"jalr" => JALR,
            b"andi" => ANDI,
            b"call" => CALL,
            b"ori" => ORI,
            b"xori" => XORI,
            b"slli" => SLLI,
            b"srli" => SRLI,
            b"srai" => SRAI,
            b"ebreak" => EBREAK,
            b"ld" => LD,
            b"sd" => SD,

            // New I32 (RV32I Base) instructions
            b"beq" => BEQ,
            b"bne" => BNE,
            b"blt" => BLT,
            b"bge" => BGE,
            b"bltu" => BLTU,
            b"bgeu" => BGEU,
            b"lb" => LB,
            b"lh" => LH,
            b"lw" => LW,
            b"lbu" => LBU,
            b"lhu" => LHU,
            b"sb" => SB,
            b"sh" => SH,
            b"sw" => SW,
            b"slti" => SLTI,
            b"sltiu" => SLTIU,
            b"sll" => SLL,
            b"srl" => SRL,
            b"sra" => SRA,
            b"sub" => SUB,
            b"slt" => SLT,
            b"sltu" => SLTU,
            b"xor" => XOR,
            b"or" => OR,
            b"and" => AND,
            b"fence" => FENCE,

            // M-extension instructions
            b"mul" => MUL,
            b"mulh" => MULH,
            b"mulhsu" => MULHSU,
            b"mulhu" => MULHU,
            b"div" => DIV,
            b"divu" => DIVU,
            b"rem" => REM,
            b"remu" => REMU,

            // A-extension Load-Reserved/Store-Conditional
            b"lr.w" => LR_W,
            b"sc.w" => SC_W,

            // A-extension Atomic Memory Operations (32-bit)
            b"amoadd.w" => AMOADD_W,
            b"amoswap.w" => AMOSWAP_W,
            b"amoand.w" => AMOAND_W,
            b"amoor.w" => AMOOR_W,
            b"amoxor.w" => AMOXOR_W,
            b"amomax.w" => AMOMAX_W,
            b"amomin.w" => AMOMIN_W,
            b"amomaxu.w" => AMOMAXU_W,
            b"amominu.w" => AMOMINU_W,

            // RV64I Base instructions
            b"lwu" => LWU,
            b"addw" => ADDW,
            b"subw" => SUBW,
            b"addiw" => ADDIW,
            b"sllw" => SLLW,
            b"srlw" => SRLW,
            b"sraw" => SRAW,
            b"slliw" => SLLIW,
            b"srliw" => SRLIW,
            b"sraiw" => SRAIW,

            // RV64 M-extension
            b"mulw" => MULW,
            b"divw" => DIVW,
            b"divuw" => DIVUW,
            b"remw" => REMW,
            b"remuw" => REMUW,
            b"mulhw" => MULHW,
            b"mulhsuw" => MULHSUW,
            b"mulhuw" => MULHUW,

            // RV64 A-extension Load-Reserved/Store-Conditional (64-bit)
            b"lr.d" => LR_D,
            b"sc.d" => SC_D,

            // RV64 A-extension Atomic Memory Operations (64-bit)
            b"amoadd.d" => AMOADD_D,
            b"amoswap.d" => AMOSWAP_D,
            b"amoand.d" => AMOAND_D,
            b"amoor.d" => AMOOR_D,
            b"amoxor.d" => AMOXOR_D,
            b"amomax.d" => AMOMAX_D,
            b"amomin.d" => AMOMIN_D,
            b"amomaxu.d" => AMOMAXU_D,
            b"amominu.d" => AMOMINU_D,

            _ => return None
        })
    }
}
