use std::{str, fmt, ptr, error};

use brik::rv32::{Reg, AqRl};

use num_traits::Num;
use memchr::{memchr, memchr2};

use crate::encoder;

const DEC_MASK     : u8 = 1 << 0;
const HEX_MASK     : u8 = 1 << 1;
const DECHEX_MASK  : u8 = DEC_MASK | HEX_MASK;

const DIGIT_TABLE: [u8; 256] = const {
    let mut table = [0; 256];
    let mut i = 0;
    while i < 256 {
        if i >= b'0' as usize && i <= b'9' as usize {
            table[i] = DEC_MASK; // decimal digit
        } else if (i >= b'a' as usize && i <= b'f' as usize) ||
                  (i >= b'A' as usize && i <= b'F' as usize) {
            table[i] = HEX_MASK; // hex digit (also valid decimal)
        }
        i += 1;
    }
    // mark decimal digits as both 1 and 2
    i = b'0' as usize;
    while i <= b'9' as usize {
        table[i] = DECHEX_MASK; // both decimal and hex
        i += 1;
    }
    table
};

const IDENT_TABLE: [u8; 256] = const {
    let mut table = [0u8; 256];
    let mut i = 0;
    while i < 256 {
        if (i >= b'a' as usize && i <= b'z' as usize) ||
           (i >= b'A' as usize && i <= b'Z' as usize) ||
           i == b'_' as usize {
            table[i] = 1; // valid first char
        } else if i >= b'0' as usize && i <= b'9' as usize {
            table[i] = 2; // valid non-first char
        }
        i += 1;
    }
    // mark first chars as also valid non-first
    i = 0;
    while i < 256 {
        if table[i] == 1 {
            table[i] = 3; // valid both first and non-first
        }
        i += 1;
    }
    table
};

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

#[inline(always)]
pub fn split_at_space(s: &str) -> (&str, &str) {
    if let Some(i) = memchr2(b' ', b'\t', s.as_bytes()) {
        let (a, b) = s.split_at(i);
        (a, &b[skip_whitespace(b)..])
    } else {
        (s, "")
    }
}

#[inline]
pub fn take_number(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    if bytes.is_empty() { return ("", s); }

    let mut i = 0;

    // branchless sign check
    let has_sign = bytes[0] == b'+' || bytes[0] == b'-';
    i += has_sign as usize;

    if i >= bytes.len() { return (&s[..i], &s[i..]); }

    let is_hex = i + 1 < bytes.len() && bytes[i] == b'0' &&
                 (bytes[i + 1] == b'x' || bytes[i + 1] == b'X');
    i += (is_hex as usize) * 2;

    if i >= bytes.len() { return (&s[..i], &s[i..]); }

    let digit_mask = 1u8 << is_hex as u8;

    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            // load 8 bytes as u64
            ptr::read_unaligned(bytes.as_ptr().add(i) as *const u64).to_le()
        };

        cfg_if::cfg_if!{
            if #[cfg(target_feature = "sse2")] {
                use std::arch::x86_64::*;

                let mask = unsafe {
                    let bytes = _mm_cvtsi64_si128(chunk as _);

                    // -------- decimal: '0'..='9' --------
                    let dec_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'0' as i8));
                    let dec_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'9' as i8));
                    let dec_invalid = _mm_or_si128(dec_lo, dec_hi);
                    let mut invalid = dec_invalid;

                    if is_hex {
                        // ----- lowercase hex: 'a'..='f' -----
                        let lo_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'a' as i8));
                        let lo_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'f' as i8));
                        let lower_invalid = _mm_or_si128(lo_lo, lo_hi);

                        // ----- uppercase hex: 'A'..='F' ------
                        let up_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'A' as i8));
                        let up_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'F' as i8));
                        let upper_invalid = _mm_or_si128(up_lo, up_hi);

                        // valid_hex = !lower_invalid || !upper_invalid
                        let lower_valid = _mm_andnot_si128(lower_invalid, _mm_set1_epi8(-1));
                        let upper_valid = _mm_andnot_si128(upper_invalid, _mm_set1_epi8(-1));
                        let hex_valid = _mm_or_si128(lower_valid, upper_valid);

                        // valid = decimal_ok || hex_ok
                        let dec_valid = _mm_andnot_si128(dec_invalid, _mm_set1_epi8(-1));
                        let all_valid = _mm_or_si128(dec_valid, hex_valid);

                        invalid = _mm_cmpeq_epi8(all_valid, _mm_setzero_si128());
                    }

                    // return mask of invalid bytes (MSB 1 -> bad)
                    (_mm_movemask_epi8(invalid) & 0xFF) as u32
                };

                if mask == 0 {
                    i += 8
                } else {
                    // first failing byte index
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else if #[cfg(target_arch = "aarch64")] {
                use std::arch::aarch64::*;

                let mask = unsafe {
                    // load bytes into a 8x8-bit vector
                    let mut buf = [0u8; 16];
                    buf[..8].copy_from_slice(&chunk.to_le_bytes());
                    let bytes = vld1q_u8(buf.as_ptr());

                    // decimal check '0'..='9'
                    let dec_lo = vcltq_u8(bytes, vdupq_n_u8(b'0'));
                    let dec_hi = vcgtq_u8(bytes, vdupq_n_u8(b'9'));
                    let dec_invalid = vorrq_u8(dec_lo, dec_hi);
                    let mut invalid = dec_invalid;

                    if is_hex {
                        // lowercase hex 'a'..='f'
                        let lo_lo = vcltq_u8(bytes, vdupq_n_u8(b'a'));
                        let lo_hi = vcgtq_u8(bytes, vdupq_n_u8(b'f'));
                        let lower_invalid = vorrq_u8(lo_lo, lo_hi);

                        // uppercase hex 'A'..='F'
                        let up_lo = vcltq_u8(bytes, vdupq_n_u8(b'A'));
                        let up_hi = vcgtq_u8(bytes, vdupq_n_u8(b'F'));
                        let upper_invalid = vorrq_u8(up_lo, up_hi);

                        let lower_valid = vbicq_u8(vdupq_n_u8(0xFF), lower_invalid);
                        let upper_valid = vbicq_u8(vdupq_n_u8(0xFF), upper_invalid);
                        let hex_valid = vorrq_u8(lower_valid, upper_valid);

                        let dec_valid = vbicq_u8(vdupq_n_u8(0xFF), dec_invalid);
                        let all_valid = vorrq_u8(dec_valid, hex_valid);

                        invalid = vceqq_u8(all_valid, vdupq_n_u8(0));
                    }

                    let mut arr = [0u8; 16];
                    vst1q_u8(arr.as_mut_ptr(), invalid);

                    // extract mask (aarch64 doesn't have _mm_movemask_epi8)
                    let mut mask = 0u32;
                    for (i, v) in arr.into_iter().enumerate() {
                        mask |= ((v >> 7) as u32) << i;
                    } mask
                };

                if mask == 0 {
                    i += 8
                } else {
                    // first failing byte index
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else {
                let b0 = ((chunk >>  0) & 0xFF) as usize;
                let b1 = ((chunk >>  8) & 0xFF) as usize;
                let b2 = ((chunk >> 16) & 0xFF) as usize;
                let b3 = ((chunk >> 24) & 0xFF) as usize;
                let b4 = ((chunk >> 32) & 0xFF) as usize;
                let b5 = ((chunk >> 40) & 0xFF) as usize;
                let b6 = ((chunk >> 48) & 0xFF) as usize;
                let b7 = ((chunk >> 56) & 0xFF) as usize;

                if (DIGIT_TABLE[b0] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b1] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b2] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b3] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b4] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b5] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b6] & digit_mask) != 0 &&
                   (DIGIT_TABLE[b7] & digit_mask) != 0 {
                    i += 8;
                } else {
                    // find exactly where it stopped
                    if (DIGIT_TABLE[b0] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b1] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b2] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b3] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b4] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b5] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b6] & digit_mask) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b7] & digit_mask) == 0 { break }
                    i += 1;
                    break
                }
            }
        }
    }

    while i < bytes.len() && (DIGIT_TABLE[bytes[i] as usize] & digit_mask) != 0 {
        i += 1
    }

    (&s[..i], &s[i..])
}

#[inline]
pub fn take_ident(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    if bytes.is_empty() { return ("", s) }

    // if first byte is not A-z or '_' -> invalid
    if IDENT_TABLE[bytes[0] as usize] & 1 == 0 {
        return ("", s)
    }

    let mut i = 1;

    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            ptr::read_unaligned(bytes.as_ptr().add(i) as *const u64).to_le()
        };

        cfg_if::cfg_if!{
            if #[cfg(target_feature = "sse2")] {
                use std::arch::x86_64::*;

                // Z-a or '_' or 0-9

                let mask = unsafe {
                    let bytes = _mm_cvtsi64_si128(chunk as i64);

                    // ----- digits: '0'..='9' -----
                    let num_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'0' as i8));
                    let num_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'9' as i8));
                    let num_invalid = _mm_or_si128(num_lo, num_hi);
                    let num_valid = _mm_andnot_si128(num_invalid, _mm_set1_epi8(-1));

                    // ----- lowercase letters: 'a'..='z' -----
                    let lo_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'a' as i8));
                    let lo_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'z' as i8));
                    let lower_invalid = _mm_or_si128(lo_lo, lo_hi);
                    let lower_valid = _mm_andnot_si128(lower_invalid, _mm_set1_epi8(-1));

                    // ----- uppercase letters: 'A'..='Z' -----
                    let up_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'A' as i8));
                    let up_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'Z' as i8));
                    let upper_invalid = _mm_or_si128(up_lo, up_hi);
                    let upper_valid = _mm_andnot_si128(upper_invalid, _mm_set1_epi8(-1));

                    // ----- underscore '_' -----
                    let uscore_valid = _mm_cmpeq_epi8(bytes, _mm_set1_epi8(b'_' as i8));

                    // combine: digit OR letter OR underscore
                    let letter_valid = _mm_or_si128(lower_valid, upper_valid);
                    let all_valid = _mm_or_si128(num_valid, _mm_or_si128(letter_valid, uscore_valid));

                    let invalid = _mm_cmpeq_epi8(all_valid, _mm_setzero_si128());

                    (_mm_movemask_epi8(invalid) & 0xFF) as u32
                };

                if mask == 0 {
                    i += 8;
                } else {
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else if #[cfg(target_arch = "aarch64")] {
                use std::arch::aarch64::*;

                // Z-a or '_' or 0-9

                let mask = unsafe {
                    // load bytes into a 8x8-bit vector
                    let mut buf = [0u8; 16];
                    buf[..8].copy_from_slice(&chunk.to_le_bytes());
                    let bytes = vld1q_u8(buf.as_ptr());

                    // decimal check '0'..='9'
                    let num_lo = vcltq_u8(bytes, vdupq_n_u8(b'0'));
                    let num_hi = vcgtq_u8(bytes, vdupq_n_u8(b'9'));
                    let num_invalid = vorrq_u8(num_lo, num_hi);
                    let num_valid = vbicq_u8(vdupq_n_u8(0xFF), num_invalid);

                    // ----- lowercase letters: 'a'..='z' -----
                    let lo_lo = vcltq_u8(bytes, vdupq_n_u8(b'a'));
                    let lo_hi = vcgtq_u8(bytes, vdupq_n_u8(b'z'));
                    let lower_invalid = vorrq_u8(lo_lo, lo_hi);

                    // ----- uppercase letters: 'A'..='Z' -----
                    let up_lo = vcltq_u8(bytes, vdupq_n_u8(b'A'));
                    let up_hi = vcgtq_u8(bytes, vdupq_n_u8(b'Z'));
                    let upper_invalid = vorrq_u8(up_lo, up_hi);

                    // ----- underscore '_' -----
                    let uscore_valid = vceqq_u8(bytes, vdupq_n_u8(b'_'));

                    let lower_valid = vbicq_u8(vdupq_n_u8(0xFF), lower_invalid);
                    let upper_valid = vbicq_u8(vdupq_n_u8(0xFF), upper_invalid);

                    // combine: digit OR letter OR underscore
                    let letter_valid = vorrq_u8(lower_valid, upper_valid);
                    let all_valid = vorrq_u8(num_valid, vorrq_u8(letter_valid, uscore_valid));

                    let invalid = vceqq_u8(all_valid, vdupq_n_u8(0));

                    let mut arr = [0u8; 16];
                    vst1q_u8(arr.as_mut_ptr(), invalid);

                    // extract mask (aarch64 doesn't have _mm_movemask_epi8)
                    let mut mask = 0u32;
                    for (i, v) in arr.into_iter().enumerate() {
                        mask |= ((v >> 7) as u32) << i;
                    } mask
                };

                if mask == 0 {
                    i += 8
                } else {
                    // first failing byte index
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else {
                let b0 = ((chunk >>  0) & 0xFF) as usize;
                let b1 = ((chunk >>  8) & 0xFF) as usize;
                let b2 = ((chunk >> 16) & 0xFF) as usize;
                let b3 = ((chunk >> 24) & 0xFF) as usize;
                let b4 = ((chunk >> 32) & 0xFF) as usize;
                let b5 = ((chunk >> 40) & 0xFF) as usize;
                let b6 = ((chunk >> 48) & 0xFF) as usize;
                let b7 = ((chunk >> 56) & 0xFF) as usize;

                // check if all are valid identifier chars (first or non-first)
                if IDENT_TABLE[b0] != 0 &&
                   IDENT_TABLE[b1] != 0 &&
                   IDENT_TABLE[b2] != 0 &&
                   IDENT_TABLE[b3] != 0 &&
                   IDENT_TABLE[b4] != 0 &&
                   IDENT_TABLE[b5] != 0 &&
                   IDENT_TABLE[b6] != 0 &&
                   IDENT_TABLE[b7] != 0 {
                    i += 8;
                } else {
                    // find exactly where it stopped
                    if IDENT_TABLE[b0] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b1] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b2] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b3] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b4] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b5] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b6] == 0 { break }
                    i += 1;
                    if IDENT_TABLE[b7] == 0 { break }
                    i += 1;
                    break
                }
            }
        }
    }

    while i < bytes.len() && IDENT_TABLE[bytes[i] as usize] != 0 {
        i += 1
    }

    (&s[..i], &s[i..])
}

#[inline]
pub fn take_hex(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    if bytes.is_empty() { return ("", s) }

    let mut i = 0;

    // branchless sign check
    let has_sign = bytes[0] == b'+' || bytes[0] == b'-';
    i += has_sign as usize;

    // SIMD-like 8 bytes at once
    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            ptr::read_unaligned(bytes.as_ptr().add(i) as *const u64).to_le()
        };

        cfg_if::cfg_if!{
            if #[cfg(target_feature = "sse2")] {
                use std::arch::x86_64::*;

                let mask = unsafe {
                    let bytes = _mm_cvtsi64_si128(chunk as _);

                    // -------- decimal: '0'..='9' --------
                    let dec_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'0' as i8));
                    let dec_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'9' as i8));
                    let dec_invalid = _mm_or_si128(dec_lo, dec_hi);

                    // ----- lowercase hex: 'a'..='f' -----
                    let lo_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'a' as i8));
                    let lo_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'f' as i8));
                    let lower_invalid = _mm_or_si128(lo_lo, lo_hi);

                    // ----- uppercase hex: 'A'..='F' ------
                    let up_lo = _mm_cmplt_epi8(bytes, _mm_set1_epi8(b'A' as i8));
                    let up_hi = _mm_cmpgt_epi8(bytes, _mm_set1_epi8(b'F' as i8));
                    let upper_invalid = _mm_or_si128(up_lo, up_hi);

                    // valid_hex = !lower_invalid || !upper_invalid
                    let lower_valid = _mm_andnot_si128(lower_invalid, _mm_set1_epi8(-1));
                    let upper_valid = _mm_andnot_si128(upper_invalid, _mm_set1_epi8(-1));
                    let hex_valid = _mm_or_si128(lower_valid, upper_valid);

                    // valid = decimal_ok || hex_ok
                    let dec_valid = _mm_andnot_si128(dec_invalid, _mm_set1_epi8(-1));
                    let all_valid = _mm_or_si128(dec_valid, hex_valid);

                    let invalid = _mm_cmpeq_epi8(all_valid, _mm_setzero_si128());

                    (_mm_movemask_epi8(invalid) & 0xFF) as u32
                };

                if mask == 0 {
                    i += 8
                } else {
                    // first failing byte index
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else if #[cfg(target_arch = "aarch64")] {
                use std::arch::aarch64::*;

                let mask = unsafe {
                    // load bytes into a 8x8-bit vector
                    let mut buf = [0u8; 16];
                    buf[..8].copy_from_slice(&chunk.to_le_bytes());
                    let bytes = vld1q_u8(buf.as_ptr());

                    // decimal check '0'..='9'
                    let dec_lo = vcltq_u8(bytes, vdupq_n_u8(b'0'));
                    let dec_hi = vcgtq_u8(bytes, vdupq_n_u8(b'9'));
                    let dec_invalid = vorrq_u8(dec_lo, dec_hi);

                    // lowercase hex 'a'..='f'
                    let lo_lo = vcltq_u8(bytes, vdupq_n_u8(b'a'));
                    let lo_hi = vcgtq_u8(bytes, vdupq_n_u8(b'f'));
                    let lower_invalid = vorrq_u8(lo_lo, lo_hi);

                    // uppercase hex 'A'..='F'
                    let up_lo = vcltq_u8(bytes, vdupq_n_u8(b'A'));
                    let up_hi = vcgtq_u8(bytes, vdupq_n_u8(b'F'));
                    let upper_invalid = vorrq_u8(up_lo, up_hi);

                    let lower_valid = vbicq_u8(vdupq_n_u8(0xFF), lower_invalid);
                    let upper_valid = vbicq_u8(vdupq_n_u8(0xFF), upper_invalid);
                    let hex_valid = vorrq_u8(lower_valid, upper_valid);

                    let dec_valid = vbicq_u8(vdupq_n_u8(0xFF), dec_invalid);
                    let all_valid = vorrq_u8(dec_valid, hex_valid);

                    let invalid = vceqq_u8(all_valid, vdupq_n_u8(0));

                    let mut arr = [0u8; 16];
                    vst1q_u8(arr.as_mut_ptr(), invalid);

                    // extract mask (aarch64 doesn't have _mm_movemask_epi8)
                    let mut mask = 0u32;
                    for (i, v) in arr.into_iter().enumerate() {
                        mask |= ((v >> 7) as u32) << i;
                    } mask
                };

                if mask == 0 {
                    i += 8
                } else {
                    // first failing byte index
                    i += mask.trailing_zeros() as usize;
                    break
                }
            } else {
                let b0 = ((chunk >>  0) & 0xFF) as usize;
                let b1 = ((chunk >>  8) & 0xFF) as usize;
                let b2 = ((chunk >> 16) & 0xFF) as usize;
                let b3 = ((chunk >> 24) & 0xFF) as usize;
                let b4 = ((chunk >> 32) & 0xFF) as usize;
                let b5 = ((chunk >> 40) & 0xFF) as usize;
                let b6 = ((chunk >> 48) & 0xFF) as usize;
                let b7 = ((chunk >> 56) & 0xFF) as usize;

                if (DIGIT_TABLE[b0] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b1] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b2] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b3] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b4] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b5] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b6] & HEX_MASK) != 0 &&
                   (DIGIT_TABLE[b7] & HEX_MASK) != 0 {
                    i += 8;
                } else {
                    // find exactly where it stopped
                    if (DIGIT_TABLE[b0] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b1] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b2] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b3] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b4] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b5] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b6] & HEX_MASK) == 0 { break }
                    i += 1;
                    if (DIGIT_TABLE[b7] & HEX_MASK) == 0 { break }
                    i += 1;
                    break
                }
            }
        }
    }

    while i < bytes.len() && (DIGIT_TABLE[bytes[i] as usize] & 2) != 0 {
        i += 1;
    }

    (&s[..i], &s[i..])
}

#[inline]
pub fn take_signed(s: &str) -> (&str, &str) {
    let bytes = s.as_bytes();
    if bytes.is_empty() { return ("", s); }

    let mut i = 0;

    // branchless sign check
    let has_sign = bytes[0] == b'+' || bytes[0] == b'-';
    i += has_sign as usize;

    // SIMD-like 8 bytes at once
    while i + 8 <= bytes.len() {
        let chunk = unsafe {
            ptr::read_unaligned(bytes.as_ptr().add(i) as *const u64).to_le()
        };

        // SWAR trick
        // check if all bytes are in range b'0'..=b'9'
        let all_digits =
            (chunk.wrapping_sub(0x3030303030303030)) & 0x8080808080808080 == 0 &&
            (chunk | 0x2020202020202020).wrapping_sub(0x3A3A3A3A3A3A3A3A) & 0x8080808080808080 == 0x8080808080808080;

        if all_digits {
            i += 8
        } else {
            for b in chunk.to_le_bytes() {
                if b.is_ascii_digit() {
                    i += 1;
                } else {
                    break
                }
            }

            break
        }
    }

    while i < bytes.len() && bytes[i] >= b'0' && bytes[i] <= b'9' {
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
pub fn ensure_comma_or_end(s: &str) -> Result<(), Box<str>> {
    let i = skip_whitespace(s);
    let bytes = s.as_bytes();

    if i >= bytes.len() || bytes[i] == b',' {
        Ok(())
    } else {
        Err(format!("expected ',' or end, got: {got}", got = &s[i..]).into())
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
pub fn ensure_empty(s: &str) -> Result<(), &str> {
    if s.trim().is_empty() { Ok(()) } else { Err("extra tokens: {s}") }
}

#[inline]
pub fn parse_i16(s: &str) -> Result<i16, Box<str>> { parse_i::<i16>(s) }

#[inline]
pub fn parse_u8(s: &str) -> Result<u8, Box<str>> { parse_i::<u8>(s) }

#[inline]
pub fn parse_i<T>(s: &str) -> Result<T, Box<str>>
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
        format!{
            "couldn't parse {tok:?} to {ty:?}: {e}",
            ty = std::any::type_name::<T>()
        }.into()
    })
}

#[inline]
pub fn parse_aqrl(operands: &str) -> Result<AqRl, encoder::EncoderError> {
    let trimmed = operands.trim();
    Ok(match trimmed {
        ""     => AqRl::None,
        "aq"   => AqRl::Acquire,
        "rl"   => AqRl::Release,
        "aqrl" => AqRl::AcquireRelease,
        _ => return Err(encoder::EncoderError::InvalidAqrl(trimmed))
    })
}

#[inline]
pub fn parsing_list<T>(input: &str, sepa: u8, mut f: impl FnMut(T)) -> Result<(), Box<str>>
where
    T: Num + str::FromStr,
    <T as Num>::FromStrRadixErr: fmt::Display,
    <T as str::FromStr>::Err: Send + Sync + fmt::Display + error::Error + 'static
{
    let bytes = input.as_bytes();

    let mut byte_offset = 0;
    while byte_offset < bytes.len() {
        let end = memchr(sepa, &bytes[byte_offset..])
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

pub fn parse_reg(s: &str) -> Result<(Reg, &str), encoder::EncoderError> {
    let s = &s[skip_whitespace(s)..];
    let bytes = s.as_bytes();

    if bytes.is_empty() {
        return Err(encoder::EncoderError::InvalidRegister(s))
    }

    // x?
    if bytes[0] == b'x' && bytes.len() > 1 {
        let (num_str, rest) = take_number(&s[1..]);
        let n = num_str.parse::<u8>().map_err(|_| {
            encoder::EncoderError::InvalidRegister(s)
        })?;
        ensure_comma_or_end(rest).map_err(|_| {
            encoder::EncoderError::InvalidRegister(s)
        })?;
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

        _ => return Err(encoder::EncoderError::InvalidRegister(s))
    };

    let rest = &s[consumed..];
    ensure_comma_or_end(rest).map_err(|_| {
        encoder::EncoderError::InvalidRegister(s)
    })?;

    Ok((Reg::from_u32(reg_num), trim_next(rest)))
}

/// Return (first_token_opt, rest_opt) where both are Option<&str>
/// - first_token_opt = Some(&str) when the line has at least one non-whitespace token
/// - rest_opt = Some(&str) if there's trailing text after the token (leading whitespace trimmed)
#[inline]
pub fn first_token_and_rest(s: &str) -> (Option<&str>, Option<&str>) {
    let bytes = s.as_bytes();
    // find first non-whitespace
    let mut i = 0usize;
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    if i >= bytes.len() {
        return (None, None);
    }
    let start = i;
    // find first whitespace after token
    while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    let first = &s[start..i];
    // skip whitespace to get to rest
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    let rest = if i < bytes.len() { Some(&s[i..]) } else { None };
    (Some(first), rest)
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
