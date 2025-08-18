use std::io::Read;
use std::path::Path;
use std::fs::OpenOptions;

use memmap2::MmapOptions;

// read directly anything under 1 MiB; otherwise mmap
#[allow(clippy::identity_op)]
const MMAP_THRESHOLD: usize = 1 * 1024 * 1024;

pub fn with_file<R>(
    path: impl AsRef<Path>,
    f: impl FnOnce(&[u8]) -> R
) -> anyhow::Result<R> {
    let mut file = OpenOptions::new()
        .read(true)
        .open(path)?;

    let meta = file.metadata()?;

    let file_size = meta.len() as usize;

    let ret = if file_size < MMAP_THRESHOLD {
        let mut buf = Vec::with_capacity(file_size);
        file.read_to_end(&mut buf)?;

        #[cfg(feature = "madvise")]
        unsafe {
            _ = libc::madvise(
                buf.as_ptr() as *mut libc::c_void,
                buf.len(),
                libc::MADV_SEQUENTIAL | libc::MADV_WILLNEED,
            )
        }

        f(&buf)
    } else {
        let mut opts = MmapOptions::new();
        opts.len(file_size);

        let mmap = unsafe { opts.map(&file)? };

        #[cfg(feature = "madvise")]
        unsafe {
            _ = libc::madvise(
                mmap.as_ptr() as *mut libc::c_void,
                mmap.len(),
                libc::MADV_SEQUENTIAL | libc::MADV_WILLNEED,
            )
        }

        f(&mmap[..])
    };

    Ok(ret)
}
