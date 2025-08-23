use std::hint;
use std::rc::Rc;
use std::io::{self, Read};
use std::fs::{self, File};
use std::collections::HashMap;

use memmap2::{Mmap, MmapOptions};

#[derive(Eq, Hash, Copy, Clone, Debug, PartialEq)]
pub struct FileId(u32);

#[derive(Debug)]
pub enum BrikFileContents {
    Buf(Vec<u8>),
    Mmap(Mmap)
}

impl BrikFileContents {
    #[inline(always)]
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            Self::Buf(vec) => vec,
            Self::Mmap(mmap) => &mmap[..]
        }
    }

    #[track_caller]
    #[inline(always)]
    pub fn as_buf_unchecked(&self) -> &Vec<u8> {
        match self {
            Self::Buf(b) => b,
            _ => unsafe { hint::unreachable_unchecked() }
        }
    }

    #[track_caller]
    #[inline(always)]
    pub fn as_mmap_unchecked(&self) -> &Mmap {
        match self {
            Self::Mmap(m) => m,
            _ => unsafe { hint::unreachable_unchecked() }
        }
    }
}

#[derive(Debug)]
pub struct BrikFile {
    // user path (not canonicalized)
    pub upath: Rc<str>,

    pub meta: fs::Metadata,

    pub handle: File,

    contents: Option<BrikFileContents>
}

impl BrikFile {
    #[inline(always)]
    pub fn new(upath: &str) -> io::Result<Self> {
        let handle = File::open(upath)?;
        let meta = handle.metadata()?;
        let upath = upath.into();

        Ok(Self { meta, upath, handle, contents: None })
    }

    #[inline(always)]
    pub fn read_contents_unchecked(&self) -> &BrikFileContents {
        unsafe { self.contents.as_ref().unwrap_unchecked() }
    }

    #[inline]
    pub fn read(&mut self) -> io::Result<&[u8]> {
        // read directly anything under 1 MiB; otherwise mmap
        #[allow(clippy::identity_op)]
        const MMAP_THRESHOLD: u64 = 1 * 1024 * 1024;

        let file_size = self.meta.len();

        #[cfg_attr(all(feature = "madvise", not(unix)), allow(unused))]
        let (ptr, len) = if file_size < MMAP_THRESHOLD {
            let bytes = self.read_file_to_vec()?;
            (bytes.as_ptr(), bytes.len())
        } else {
            let mmap = self.mmap_file()?;
            let bytes = &mmap[..];
            (bytes.as_ptr(), bytes.len())
        };

        #[cfg(all(feature = "madvise", unix))]
        unsafe {
            _ = libc::madvise(
                ptr as *mut libc::c_void,
                len,
                libc::MADV_SEQUENTIAL | libc::MADV_WILLNEED,
            )
        }

        Ok(self.read_contents_unchecked().as_bytes())
    }

    #[inline]
    pub fn read_file_to_vec(&mut self) -> io::Result<&[u8]> {
        let file_size = self.meta.len() as usize;

        match self.contents {
            Some(BrikFileContents::Buf(_)) => {}
            Some(BrikFileContents::Mmap(_)) => unreachable!{
                "`read_file_to_end` called on a mmapped file"
            },

            None => {
                let mut buf = Vec::with_capacity(file_size);
                // to not initialize buf with zeroes
                #[allow(clippy::uninit_vec)]
                unsafe { buf.set_len(file_size); }

                self.handle.read_exact(&mut buf)?;
                self.contents = Some(BrikFileContents::Buf(buf))
            }
        }

        Ok(self.read_contents_unchecked().as_buf_unchecked())
    }

    #[inline]
    pub fn mmap_file(&mut self) -> io::Result<&Mmap> {
        if let Some(BrikFileContents::Mmap(_)) = &self.contents {
            return Ok(self.read_contents_unchecked().as_mmap_unchecked())
        }

        if self.contents.is_none() {
            let mut opts = MmapOptions::new();
            opts.len(self.meta.len() as usize);

            let mmap = unsafe { opts.map(&self.handle)? };

            self.contents = Some(BrikFileContents::Mmap(mmap))
        }

        Ok(self.read_contents_unchecked().as_mmap_unchecked())
    }
}

#[derive(Debug, Default)]
pub struct FileManager {
    pub files: HashMap<FileId, BrikFile, wyhash::WyHasherBuilder>,

    file_id: u32,

    // seen canonicalized filepaths
    seen: HashMap<String, FileId, wyhash::WyHasherBuilder>,
}

impl FileManager {
    #[track_caller]
    #[inline(always)]
    pub fn get_file_unchecked(&self, file_id: FileId) -> &BrikFile {
        &self.files[&file_id]
    }

    #[track_caller]
    #[inline(always)]
    pub fn get_file_path_rc_unchecked(&self, file_id: FileId) -> Rc<str> {
        Rc::clone(&self.files[&file_id].upath)
    }

    #[inline]
    fn next_file_id(&mut self) -> FileId {
        let id = self.file_id;
        self.file_id += 1;
        FileId(id)
    }

    #[inline]
    pub fn read_file(&mut self, file: BrikFile) -> io::Result<(FileId, &[u8])> {
        let canon = fs::canonicalize(&*file.upath)?
            .to_string_lossy()
            .into_owned();

        if let Some(&old_file_id) = self.seen.get(&canon) {
            let bytes = self
                .get_file_unchecked(old_file_id)
                .read_contents_unchecked()
                .as_bytes();

            return Ok((old_file_id, bytes))
        }

        let file_id = self.next_file_id();
        self.seen.insert(canon, file_id);

        let e = self.files.entry(file_id);
        let e = e.insert_entry(file);

        let file = e.into_mut();
        file.read().map(|r| (file_id, r))
    }
}
