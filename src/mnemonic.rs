#[derive(Clone, Copy, Debug)]
pub enum Mnemonic {
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
            b"ecall" => ECALL,
            b"la" => LA,
            b"addi" => ADDI,
            b"add" => ADD,
            b"lui" => LUI,
            b"auipc" => AUIPC,
            b"jal" => JAL,
            b"jalr" => JALR,
            b"andi" => ANDI,
            b"ori" => ORI,
            b"xori" => XORI,
            b"slli" => SLLI,
            b"srli" => SRLI,
            b"srai" => SRAI,
            b"ebreak" => EBREAK,
            b"ld" => LD,
            b"sd" => SD,
            _ => return None
        })
    }
}
