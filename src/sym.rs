use std::mem;

use brik::object::write::SymbolId;

#[derive(Copy, Clone)]
struct SymEntry {
    hash: u64,
    id: SymbolId,
}

impl SymEntry {
    const EMPTY_ID: u32 = u32::MAX;

    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        let id: usize = unsafe { mem::transmute(self.id) };
        id == Self::EMPTY_ID as _
    }
}

impl Default for SymEntry {
    #[inline(always)]
    fn default() -> Self {
        Self {
            hash: 0,

            // SymbolId internal usize is private, so..
            id: unsafe { mem::transmute(Self::EMPTY_ID as usize) }
        }
    }
}

pub struct SymInterner {
    // hash -> id mapping, pow2 sized for fast modulo
    table: Vec<SymEntry>,
    mask: usize,
}

impl SymInterner {
    #[inline(always)]
    pub fn new() -> Self {
        Self::with_capacity(128)
    }

    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        let cap = capacity.next_power_of_two().max(64);
        Self {
            table: vec![SymEntry::default(); cap],
            mask: cap - 1,
        }
    }

    /// fastest possible lookup - single hash + single array access
    #[inline]
    pub fn get(&self, name: &[u8]) -> Option<SymbolId> {
        let hash = sym_hash_(name);
        let idx = (hash as usize) & self.mask;

        unsafe {
            let entry = self.table.get_unchecked(idx);
            if entry.hash == hash && !entry.is_empty() {
                Some(entry.id)
            } else {
                None
            }
        }
    }

    #[inline(always)]
    pub fn intern(&mut self, name: &[u8], id: SymbolId) {
        let hash = sym_hash_(name);
        let idx = (hash as usize) & self.mask;

        unsafe {
            *self.table.get_unchecked_mut(idx) = SymEntry { hash, id };
        }
    }

    #[allow(unused)]
    #[inline(always)]
    pub const fn capacity(&self) -> usize {
        self.table.len()
    }

    #[inline]
    #[allow(unused)]
    /// Resize table (call if you expect more symbols)
    pub fn reserve(&mut self, additional: usize) {
        if additional > self.capacity() / 2 {
            let new_cap = (self.capacity() + additional).next_power_of_two();
            let old_table = mem::replace(&mut self.table, vec![SymEntry::default(); new_cap]);
            self.mask = new_cap - 1;

            // rehash existing entries
            for entry in old_table {
                if !entry.is_empty() {
                    let idx = (entry.hash as usize) & self.mask;
                    unsafe {
                        *self.table.get_unchecked_mut(idx) = entry;
                    }
                }
            }
        }
    }
}

/// hash optimized for assembly symbols
#[inline]
fn sym_hash_(data: &[u8]) -> u64 {
    match data.len() {
        0 => 0,
        1 => data[0] as _,
        2 => u16::from_le_bytes([data[0], data[1]]) as _,
        3 => {
            let mut bytes = [0u8; 4];
            bytes[..3].copy_from_slice(data);
            u32::from_le_bytes(bytes) as _
        },
        4 => u32::from_le_bytes([data[0], data[1], data[2], data[3]]) as _,
        5..=8 => {
            let mut bytes = [0u8; 8];
            bytes[..data.len()].copy_from_slice(data);
            u64::from_le_bytes(bytes)
        }
        _ => crc32fast::hash(data) as _
    }
}
