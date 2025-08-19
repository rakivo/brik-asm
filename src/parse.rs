use std::{str, fmt, error};

use brik::rv32::{Reg, AqRl};

use anyhow::bail;
use num_traits::Num;
use memchr::{memchr, memchr2};

#[inline]
pub fn skip_whitespace(s: &str) -> usize {
    let bytes = s.as_bytes();

    let mut i = 0;

    // skip initial whitespace
    while i < bytes.len() && matches!{
        bytes[i], b' ' | b'\t' | b'\r' | b'\n'
    } {
        i += 1
    }

    i
}

#[inline]
pub fn split_at_space(s: &str) -> (&str, &str) {
    if let Some(i) = memchr2(b' ', b'\t', s.as_bytes()) {
        let (a, b) = s.split_at(i);
        (a, &b[skip_whitespace(b)..])
    } else {
        (s, "")
    }
}

#[inline]
pub fn strip_comment(s: &str) -> &str {
    if let Some(idx) = s.bytes().rposition(|b| b == b';') {
        &s[..idx]
    } else {
        s
    }
}

#[inline]
pub fn take_number(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;

    if matches!(bytes.first(), Some(b'+') | Some(b'-')) { i += 1 }

    let c_cmp = if matches!{
        (bytes.first(), bytes.get(1)), (Some(b'0'), Some(b'x'))
    } {
        i += 2;
        u8::is_ascii_hexdigit
    } else {
        u8::is_ascii_digit
    };

    while i + 4 <= bytes.len() {
        let chunk = [bytes[i], bytes[i+1], bytes[i+2], bytes[i+3]];
        if chunk.iter().all(c_cmp) {
            i += 4
        } else {
            break
        }
    }

    while i < bytes.len() && c_cmp(&bytes[i]) {
        i += 1
    }

    (&s[..i], &s[i..])
}

#[inline]
pub fn take_ident(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    let mut i = 0;
    // first char: must be letter or underscore
    if i < bytes.len() && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
        i += 1;
        while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1
        }
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
    if bytes.is_empty() || bytes[0] != b'"' {
        return (s, "")
    }

    if let Some(end) = memchr(b'"', &bytes[1..]) {
        let total_len = end + 2;
        (&s[..total_len], &s[total_len..])
    } else {
        (s, "")
    }
}

#[inline]
pub fn ensure_comma_or_end(s: &str) -> anyhow::Result<()> {
    let i = skip_whitespace(s);
    let bytes = s.as_bytes();

    if i >= bytes.len() || bytes[i] == b',' {
        Ok(())
    } else {
        bail!("expected ',' or end, got: {got}", got = &s[i..])
    }
}

#[inline]
pub fn trim_next(s: &str) -> &str {
    let bytes = s.as_bytes();

    let mut i = skip_whitespace(s);

    // skip comma if present
    if i < bytes.len() && bytes[i] == b',' {
        i += 1;
        i += skip_whitespace(&s[i..]);
    }

    &s[i..]
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
pub fn parse_i<T>(s: &str) -> anyhow::Result<T>
where
    T: Num,
    <T as Num>::FromStrRadixErr: fmt::Display
{
    let s = &s[skip_whitespace(s)..];
    let ((tok, rest), radix) = if let Some(rest) = s.strip_prefix("0x") {
        (take_hex(rest), 16)
    } else {
        (take_signed(s), 10)
    };
    ensure_comma_or_end(rest)?;
    T::from_str_radix(tok, radix).map_err(|e| {
        anyhow::anyhow!{
            "couldn't parse {tok:?} to {ty:?}: {e}",
            ty = std::any::type_name::<T>()
        }
    })
}

#[inline]
pub fn parse_aqrl(operands: &str) -> anyhow::Result<AqRl> {
    let trimmed = operands.trim();
    Ok(match trimmed {
        ""     => AqRl::None,
        "aq"   => AqRl::Acquire,
        "rl"   => AqRl::Release,
        "aqrl" => AqRl::AcquireRelease,
        _ => bail!("invalid acquire/release specifier"),
    })
}

#[inline]
pub fn with_cs_list<T>(input: &str, mut f: impl FnMut(T)) -> anyhow::Result<()>
where
    T: Num + str::FromStr,
    <T as Num>::FromStrRadixErr: fmt::Display,
    <T as str::FromStr>::Err: Send + Sync + fmt::Display + error::Error + 'static
{
    let bytes = input.as_bytes();

    let mut byte_offset = 0;
    while byte_offset < bytes.len() {
        let end = memchr(b',', &bytes[byte_offset..])
            .map(|pos| byte_offset + pos)
            .unwrap_or(bytes.len());

        let item = input[byte_offset..end].trim();
        if !item.is_empty() {
            let val = parse_i(item)?;
            f(val);
        }

        byte_offset = end + 1;
    }

    Ok(())
}

pub fn parse_reg(s: &str) -> anyhow::Result<(Reg, &str)> {
    let s = &s[skip_whitespace(s)..];
    let bytes = s.as_bytes();

    if bytes.is_empty() {
        bail!("undefined register: {s}");
    }

    // x?
    if bytes[0] == b'x' && bytes.len() > 1 {
        let (num_str, rest) = take_number(&s[1..]);
        let n = num_str.parse::<u8>()?;
        ensure_comma_or_end(rest)?;
        return Ok((Reg::from_u32(n as _), trim_next(rest)));
    }

    let (reg_num, consumed) = match (bytes[0], bytes.get(1), bytes.get(2)) {
        (b'a', Some(b'0'), _) => (10, 2),  // a0 - return value/first arg
        (b'a', Some(b'1'), _) => (11, 2),  // a1 - second arg
        (b'r', Some(b'a'), _) => (1, 2),   // ra - return address
        (b's', Some(b'p'), _) => (2, 2),   // sp - stack pointer
        (b't', Some(b'0'), _) => (5, 2),   // t0 - temp register
        (b's', Some(b'0'), _) => (8, 2),   // s0/fp - frame pointer
        (b'a', Some(b'2'), _) => (12, 2),  // a2
        (b'a', Some(b'3'), _) => (13, 2),  // a3
        (b't', Some(b'1'), _) => (6, 2),   // t1
        (b's', Some(b'1'), Some(b'0')) => (26, 3), // s10 (check 3-char first)
        (b's', Some(b'1'), Some(b'1')) => (27, 3), // s11
        (b's', Some(b'1'), _) => (9, 2),   // s1
        (b'g', Some(b'p'), _) => (3, 2),   // gp
        (b't', Some(b'p'), _) => (4, 2),   // tp
        (b't', Some(b'2'), _) => (7, 2),   // t2
        (b't', Some(b'3'), _) => (28, 2),  // t3
        (b't', Some(b'4'), _) => (29, 2),  // t4
        (b't', Some(b'5'), _) => (30, 2),  // t5
        (b't', Some(b'6'), _) => (31, 2),  // t6
        (b's', Some(b'2'), _) => (18, 2),  // s2
        (b's', Some(b'3'), _) => (19, 2),  // s3
        (b's', Some(b'4'), _) => (20, 2),  // s4
        (b's', Some(b'5'), _) => (21, 2),  // s5
        (b's', Some(b'6'), _) => (22, 2),  // s6
        (b's', Some(b'7'), _) => (23, 2),  // s7
        (b's', Some(b'8'), _) => (24, 2),  // s8
        (b's', Some(b'9'), _) => (25, 2),  // s9
        (b'a', Some(b'4'), _) => (14, 2),  // a4
        (b'a', Some(b'5'), _) => (15, 2),  // a5
        (b'a', Some(b'6'), _) => (16, 2),  // a6
        (b'a', Some(b'7'), _) => (17, 2),  // a7
        (b'f', Some(b'p'), _) => (8, 2),   // fp (alias)
        (b'z', Some(b'e'), Some(b'r')) if bytes.len() >= 4 && &bytes[..4] == b"zero" => (0, 4),

        _ => bail!("undefined register: {s}"),
    };

    let rest = &s[consumed..];
    ensure_comma_or_end(rest)?;

    Ok((Reg::from_u32(reg_num), trim_next(rest)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_skip_whitespace() {
        assert_eq!(skip_whitespace("  hello"), 2);
        assert_eq!(skip_whitespace("\t\n hello"), 3);
        assert_eq!(skip_whitespace("hello"), 0);
    }

    #[test]
    fn test_take_number() {
        assert_eq!(take_number("123abc"), ("123", "abc"));
        assert_eq!(take_number("+456def"), ("+456", "def"));
        assert_eq!(take_number("-789ghi"), ("-789", "ghi"));
        assert_eq!(take_number("12345678901234"), ("12345678901234", ""));
    }

    #[test]
    fn test_take_string() {
        assert_eq!(take_string(r#""hello" world"#), (r#""hello""#, " world"));
        assert_eq!(take_string(r#""unclosed"#), (r#""unclosed"#, ""));
        assert_eq!(take_string("not a string"), ("not a string", ""));
    }

    #[test]
    fn test_trim_next() {
        assert_eq!(trim_next("  , hello"), "hello");
        assert_eq!(trim_next(",world"), "world");
        assert_eq!(trim_next("  no comma"), "no comma");
    }
}
