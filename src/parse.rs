use brik::rv32::Reg;

use anyhow::bail;
use memchr::memchr;

#[inline]
pub fn split_at_space(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    if let Some(i) = memchr(b' ', bytes) {
        let (a, b) = s.split_at(i);
        (a, b.trim_start())
    } else if let Some(i) = memchr(b'\t', bytes) {
        let (a, b) = s.split_at(i);
        (a, b.trim_start())
    } else {
        (s, "")
    }
}

#[inline]
pub fn strip_comment(mut s: &str) -> &str {
    if let Some(idx) = s.bytes().rposition(|b| b == b';') {
        s = &s[..idx]
    }

    s
}

#[inline]
pub fn take_number(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;
    if matches!(bytes.first(), Some(b'+') | Some(b'-')) { i += 1 }
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        i += 1
    }
    (&s[..i], &s[i..])
}

#[inline]
pub fn take_ident(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() && bytes[i].is_ascii_alphanumeric() {
        i += 1
    }
    (&s[..i], &s[i..])
}

#[inline]
pub fn take_hex(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;
    if matches!(bytes.first(), Some(b'+') | Some(b'-')) { i += 1 }
    while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
        i += 1
    }
    (&s[..i], &s[i..])
}

#[inline]
pub fn take_signed(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;
    if matches!(bytes.first(), Some(b'+') | Some(b'-')) { i += 1 }
    while i < bytes.len() && bytes[i].is_ascii_digit() {
        i += 1
    }
    (&s[..i], &s[i..])
}

#[inline]
pub fn take_string(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 1;
    while i < bytes.len() && bytes[i] != b'"' {
        i += 1
    }
    (&s[..i], &s[i..])
}

#[inline]
pub fn ensure_comma_or_end(s: &str) -> anyhow::Result<()> {
    let st = s.trim_start();
    if st.is_empty() || st.as_bytes()[0] == b',' {
        Ok(())
    } else {
        bail!("expected ',' or end, got: {s}")
    }
}

#[inline]
pub fn trim_next(s: &str) -> &str {
    s.trim_start().strip_prefix(',').map(|x| x.trim_start()).unwrap_or(s.trim_start())
}

#[inline]
pub fn ensure_empty(s: &str) -> anyhow::Result<()> {
    if s.trim().is_empty() { Ok(()) } else { bail!("extra tokens: {s}") }
}

#[inline]
pub fn parse_i16(s: &str) -> anyhow::Result<i16> { parse_i::<i16>(s) }

#[inline]
pub fn parse_u8(s: &str) -> anyhow::Result<u8> { parse_i::<u8>(s) }

#[inline]
pub fn parse_i<T: core::str::FromStr>(s: &str) -> anyhow::Result<T> {
    let s = s.trim_start();
    let (tok, rest) = if let Some(rs) = s.strip_prefix("0x") {
        take_hex(rs)
    } else {
        take_signed(s)
    };
    ensure_comma_or_end(rest)?;
    tok.parse().map_err(|_| anyhow::anyhow!("bad number: {tok}"))
}

pub fn parse_reg(s: &str) -> anyhow::Result<(Reg, &str)> {
    let s = s.trim_start();

    if let Some(rest) = s.strip_prefix('x') {
        let (num_str, rest) = take_number(rest);
        let n = num_str.parse::<u8>()?;
        ensure_comma_or_end(rest)?;
        return Ok((Reg::from_u32(n as _), trim_next(rest)));
    }

    let Some(first_byte) = s.as_bytes().first() else {
        bail!("undefined register: {s}")
    };

    #[allow(clippy::manual_strip)]
    let (reg_num, rest) = match first_byte {
        b'z' if s.starts_with("zero") => (0, &s[4..]),
        b'r' if s.starts_with("ra") => (1, &s[2..]),
        b's' => {
            if      s.starts_with("sp") { (2, &s[2..])   } else if s.starts_with("s0")  { (8, &s[2..])  }
            else if s.starts_with("s1") { (9, &s[2..])   } else if s.starts_with("s2")  { (18, &s[2..]) }
            else if s.starts_with("s3") { (19, &s[2..])  } else if s.starts_with("s4")  { (20, &s[2..]) }
            else if s.starts_with("s5") { (21, &s[2..])  } else if s.starts_with("s6")  { (22, &s[2..]) }
            else if s.starts_with("s7") { (23, &s[2..])  } else if s.starts_with("s8")  { (24, &s[2..]) }
            else if s.starts_with("s9") { (25, &s[2..])  } else if s.starts_with("s10") { (26, &s[3..]) }
            else if s.starts_with("s11") { (27, &s[3..]) } else { bail!("expected register, got: {s}") }
        }
        b't' => {
            if      s.starts_with("tp") { (4, &s[2..])  } else if s.starts_with("t0") { (5, &s[2..])  }
            else if s.starts_with("t1") { (6, &s[2..])  } else if s.starts_with("t2") { (7, &s[2..])  }
            else if s.starts_with("t3") { (28, &s[2..]) } else if s.starts_with("t4") { (29, &s[2..]) }
            else if s.starts_with("t5") { (30, &s[2..]) } else if s.starts_with("t6") { (31, &s[2..]) }
            else { bail!("expected register, got: {s}") }
        }
        b'a' => {
            if      s.starts_with("a0") { (10, &s[2..]) } else if s.starts_with("a1") { (11, &s[2..]) }
            else if s.starts_with("a2") { (12, &s[2..]) } else if s.starts_with("a3") { (13, &s[2..]) }
            else if s.starts_with("a4") { (14, &s[2..]) } else if s.starts_with("a5") { (15, &s[2..]) }
            else if s.starts_with("a6") { (16, &s[2..]) } else if s.starts_with("a7") { (17, &s[2..]) }
            else { bail!("expected register, got: {s}") }
        }
        b'f' if s.starts_with("fp") => (8, &s[2..]),
        b'g' if s.starts_with("gp") => (3, &s[2..]),
        _ => bail!("expected register, got: {s}"),
    };

    ensure_comma_or_end(rest)?;

    Ok((Reg::from_u32(reg_num), trim_next(rest)))
}
