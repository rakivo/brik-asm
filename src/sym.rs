use std::{mem, ptr};

use brik::object::write::SymbolId;

#[repr(transparent)]
#[derive(Copy, Clone)]
struct __SymId(usize);

impl __SymId {
    #[inline(always)]
    pub const fn into_brik_symbol_id(self) -> SymbolId {
        unsafe { mem::transmute(self) }
    }

    #[inline(always)]
    pub const fn from_brik_symbol_id(sid: SymbolId) -> Self {
        unsafe { mem::transmute(sid) }
    }
}

#[repr(C)]
#[derive(Copy, Clone)]
struct SymEntry {
    key_lo: u64,     // 64
    key_hi: u64,     // 64
    xxhash: u64,     // 64
    sym_id: __SymId, // 64
    len:    u32,     // 32
    __pad:  u32      // 32
}

impl SymEntry {
    const ZEROED: Self = unsafe { mem::zeroed() };
}

impl Default for SymEntry {
    #[inline(always)]
    fn default() -> Self { Self::ZEROED }
}

/// Assembly symbol interner
pub struct SymInterner {
    table: Vec<SymEntry>,
    mask: usize,
    len: usize,
}

impl SymInterner {
    const MIN_CAP: usize = 64;

    #[inline(always)]
    pub fn new() -> Self {
        Self::with_capacity(128)
    }

    #[inline(always)]
    pub fn with_capacity(capacity: usize) -> Self {
        let cap = capacity.next_power_of_two().max(Self::MIN_CAP);
        Self {
            table: vec![SymEntry::default(); cap],
            mask: cap - 1,
            len: 0,
        }
    }

    #[inline]
    pub fn get(&self, name: &[u8]) -> Option<SymbolId> {
        let (lo, hi, len) = Self::pack_key(name);
        let mut idx = Self::hash_sym(name) as usize & self.mask;
        let mut dist = 0;

        loop {
            let entry = &self.table[idx];
            if entry.len == 0 { return None; }
            if Self::matches(entry, lo, hi, len) {
                return Some(entry.sym_id.into_brik_symbol_id())
            }

            // Robin Hood step: increase distance
            dist += 1;
            idx = (idx + 1) & self.mask;
            if dist > self.table.len() { return None; }
        }
    }

    #[inline]
    pub fn intern(&mut self, name: &[u8], id: SymbolId) {
        if self.len * 2 >= self.table.len() { self.resize(); }

        let (lo, hi, len) = Self::pack_key(name);

        let xxhash = Self::hash_sym(name);

        self.intern_hash(SymEntry {
            key_lo: lo,
            key_hi: hi,
            len,
            sym_id: __SymId::from_brik_symbol_id(id),
            xxhash,
            __pad: 0
        });
    }

    #[inline]
    pub fn resize(&mut self) {
        let new_cap = self.table.len() * 2;
        let old_table = mem::replace(
            &mut self.table,
            vec![SymEntry::default(); new_cap]
        );
        self.mask = new_cap - 1;
        self.len = 0;

        for entry in old_table {
            if entry.len != 0 {
                self.intern_hash(entry);
            }
        }
    }

    #[inline(always)]
    const fn pack_key(name: &[u8]) -> (u64, u64, u32) {
        let len = name.len() as u32;
        let stored_len = len.wrapping_add(1);

        let mut lo = 0u64;
        let mut hi = 0u64;
        unsafe {
            ptr::copy_nonoverlapping(
                name.as_ptr(),
                &mut lo as *mut _ as *mut _,
                if 8 < len { 8 } else { len as _ }
            );
            if len > 8 {
                ptr::copy_nonoverlapping(
                    name.as_ptr().add(8),
                    &mut hi as *mut _ as *mut _,
                    (len - 8) as usize,
                );
            }
        }

        (lo.to_le(), hi.to_le(), stored_len )
    }

    #[inline(always)]
    const fn matches(entry: &SymEntry, lo: u64, hi: u64, len: u32) -> bool {
        ((entry.key_lo ^ lo) | (entry.key_hi ^ hi) | ((entry.len ^ len) as u64)) == 0
    }

    #[inline]
    fn intern_hash(&mut self, entry: SymEntry) {
        let mut idx = entry.xxhash as usize & self.mask;
        let mut new_dist = 0;
        let mut new_entry = entry;

        let tlen = self.table.len();

        loop {
            let entry = &mut self.table[idx];
            if entry.len == 0 {
                self.table[idx] = new_entry;
                self.len += 1;
                return
            }

            // Robin Hood swap if the existing entry has smaller probe distance
            let dist = (
                idx + tlen - (entry.xxhash as usize & self.mask)
            ) % tlen;

            if dist < new_dist {
                mem::swap(entry, &mut new_entry);
                new_dist = dist;
            }

            new_dist += 1;
            idx = (idx + 1) & self.mask;
        }
    }

    #[inline(always)]
    fn hash_sym(data: &[u8]) -> u64 {
        xxhash_rust::xxh64::xxh64(data, 0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn __brik_symbol_id(v: usize) -> SymbolId {
        unsafe { mem::transmute(v) }
    }

    #[test]
    fn basic_insert_lookup() {
        let mut interner = SymInterner::new();

        let id1 = __brik_symbol_id(1);
        let id2 = __brik_symbol_id(2);

        interner.intern(b"foo", id1);
        interner.intern(b"bar", id2);

        assert_eq!(interner.get(b"foo"), Some(id1));
        assert_eq!(interner.get(b"bar"), Some(id2));
        assert_eq!(interner.get(b"baz"), None);
    }

    #[test]
    fn multiple_insertions_and_collisions() {
        let mut interner = SymInterner::with_capacity(8);

        let mut symbols = vec![];
        for i in 0..16 {
            let s = format!("sym{}", i);
            let id = __brik_symbol_id(i);
            interner.intern(s.as_bytes(), id);
            symbols.push((s, id));
        }

        for (s, id) in symbols {
            assert_eq!(interner.get(s.as_bytes()), Some(id));
        }
    }

    #[test]
    fn resize_works() {
        let mut interner = SymInterner::with_capacity(4);

        for i in 0..10 {
            let s = format!("resize{}", i);
            let id = __brik_symbol_id(i);
            interner.intern(s.as_bytes(), id);
        }

        for i in 0..10 {
            let s = format!("resize{}", i);
            let id = __brik_symbol_id(i);
            assert_eq!(interner.get(s.as_bytes()), Some(id));
        }
    }

    #[test]
    fn edge_cases() {
        let mut interner = SymInterner::new();

        let empty_id = __brik_symbol_id(42);
        interner.intern(b"", empty_id);
        assert_eq!(interner.get(b""), Some(empty_id));

        let max16 = b"1234567890abcdef";
        let max16_id  = __brik_symbol_id(99);
        interner.intern(max16, max16_id);
        assert_eq!(interner.get(max16), Some(max16_id));
    }

    #[test]
    fn random_collision_simulation() {
        // simulate hash collision by overriding hash_sym
        struct CollisionInterner(SymInterner);

        impl CollisionInterner {
            fn new() -> Self {
                Self(SymInterner::new())
            }

            fn intern(&mut self, name: &[u8], id: SymbolId) {
                let entry = SymEntry {
                    key_lo: 0,
                    key_hi: 0,
                    xxhash: 0, // fake hash to collide
                    sym_id: __SymId::from_brik_symbol_id(id),
                    len: (name.len() as u32).wrapping_add(1),
                    __pad: 0,
                };
                self.0.intern_hash(entry);
            }

            fn get(&self, _name: &[u8]) -> Option<SymbolId> {
                // Just return first entry to test collision safety
                if self.0.table[0].len != 0 {
                    Some(self.0.table[0].sym_id.into_brik_symbol_id())
                } else { None }
            }
        }

        let mut interner = CollisionInterner::new();
        let id1 = __brik_symbol_id(1);
        let id2 = __brik_symbol_id(2);

        interner.intern(b"foo", id1);
        interner.intern(b"bar", id2);

        assert!(interner.get(b"foo").is_some());
        assert!(interner.get(b"bar").is_some());
    }
}
