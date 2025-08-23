use std::{mem, ptr};

use brik::object::write::SymbolId;

#[repr(transparent)]
#[derive(Copy, Clone)]
struct SymId_(usize);

impl SymId_ {
    #[inline(always)]
    pub const fn into_brik(self) -> SymbolId { unsafe { mem::transmute(self) } }

    #[inline(always)]
    pub const fn from_brik(sid: SymbolId) -> Self { unsafe { mem::transmute(sid) } }
}

#[derive(Copy, Clone)]
struct SymEntry {
    key_lo: u64,    // first 8 bytes (LE) ...........64
    key_hi: u64,    // next 8 bytes (LE) ............64
    hash:   u64,    // xxhash64 over full bytes .....64
    len1:   u32,    // length + 1 (0 == sentinel) ...32
    dist:   u32,    // probe distance (Robin Hood) ..32
    off:    u32,    // offset into arena (bytes) ....32
    id:     SymId_, // brik SymbolId ................64
}

impl SymEntry {
    const EMPTY: Self = unsafe { mem::zeroed() };

    #[inline(always)]
    const fn is_empty(&self) -> bool { self.len1 == 0 }

    #[inline(always)]
    const fn len(&self) -> usize { (self.len1 - 1) as usize }
}

pub struct SymInterner {
    table: Vec<SymEntry>,
    mask:  usize,
    len:   usize,
    arena: Vec<u8>
}

#[inline(always)]
fn hash_(data: &[u8]) -> u64 {
    wyhash::wyhash(data, 0)
}

#[inline(always)]
fn pack16_(name: &[u8]) -> (u64, u64) {
    let mut lo = 0u64;
    let mut hi = 0u64;
    unsafe {
        let n0 = name.len().min(8);
        ptr::copy_nonoverlapping(
            name.as_ptr(),
            &mut lo as *mut _ as *mut u8,
            n0
        );
        if name.len() > 8 {
            let n1 = (name.len() - 8).min(8);
            ptr::copy_nonoverlapping(
                name.as_ptr().add(8),
                &mut hi as *mut _ as *mut u8,
                n1
            );
        }
    }
    (lo.to_le(), hi.to_le())
}

impl SymInterner {
    const MIN_CAP  : usize = 64;
    const LOAD_NUM : usize =  7; // load factor ~0.7
    const LOAD_DEN : usize = 10;

    #[inline(always)]
    pub fn new() -> Self { Self::with_capacity(128) }

    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        let cap = capacity.next_power_of_two().max(Self::MIN_CAP);
        SymInterner {
            table: vec![SymEntry::EMPTY; cap],
            mask: cap - 1,
            len: 0,
            arena: Vec::new(),
        }
    }

    #[inline]
    pub fn get(&self, name: &[u8]) -> Option<SymbolId> {
        let (lo, hi) = pack16_(name);
        let len1 = (name.len() as u32).wrapping_add(1);
        let h = hash_(name);

        self.is_interned(name, h, lo, hi, len1).ok().map(|(idx, _)| {
            self.table[idx].id.into_brik()
        })
    }

    #[inline]
    pub fn intern(&mut self, name: &[u8], id: SymbolId) {
        if self.len * Self::LOAD_DEN >= self.table.len() * Self::LOAD_NUM {
            self.resize()
        }

        let (lo, hi) = pack16_(name);
        let len1 = (name.len() as u32).wrapping_add(1);
        let h = hash_(name);

        let (mut idx, dist) = match self.is_interned(name, h, lo, hi, len1) {
            Err(x) => x,

            // already interned
            Ok(..) => return
        };

        let off = self.arena.len() as u32;
        self.arena.extend_from_slice(name);

        let mut new = SymEntry {
            key_lo: lo,
            key_hi: hi,
            hash: h,
            len1,
            dist,
            off,
            id: SymId_::from_brik(id),
        };

        loop {
            let e = &mut self.table[idx];
            if e.is_empty() {
                *e = new;
                self.len += 1;
                return
            }

            if e.dist < new.dist {
                // Robin Hood swap
                mem::swap(e, &mut new);
            }

            new.dist += 1;
            idx = (idx + 1) & self.mask;
        }
    }

    fn is_interned(
        &self,
        name: &[u8],
        h: u64,
        lo: u64,
        hi: u64,
        len1: u32,
    ) -> Result<(usize, u32), (usize, u32)> {
        let mut idx = (h as usize) & self.mask;
        let mut dist = 0u32;

        loop {
            let e = &self.table[idx];

            if e.is_empty() || e.dist < dist { return Err((idx, dist)) }

            if e.hash == h && e.len1 == len1 && e.key_lo == lo && e.key_hi == hi {
                if e.len() <= 16 { return Ok((idx, dist)) }

                let off = e.off as usize;
                if &self.arena[off..off + e.len()] == name {
                    // already interned
                    return Ok((idx, dist))
                }
            }

            dist += 1;
            idx = (idx + 1) & self.mask;
        }
    }

    pub fn resize(&mut self) {
        let new_cap = self.table.len() * 2;
        let mut newtab = vec![SymEntry::EMPTY; new_cap];
        let newmask = new_cap - 1;

        for mut e in self.table.drain(..) {
            if e.is_empty() { continue }

            e.dist = 0;

            let mut idx = (e.hash as usize) & newmask;
            loop {
                let slot = &mut newtab[idx];
                if slot.is_empty() {
                    *slot = e;
                    break
                }

                if slot.dist < e.dist {
                    mem::swap(slot, &mut e);
                }

                e.dist += 1;
                idx = (idx + 1) & newmask;
            }
        }

        self.table = newtab;
        self.mask = newmask;
        // self.len unchanged
    }
}
