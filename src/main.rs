// TODO(#11): Conditional compilation
// TODO(#9): Local labels
// TODO(#8): .size directive
// TODO(#10): Report undefined symbols
// TODO(#2): Properly manage symbol sizes
// TODO(#6): Factor out assembler API into a lib.rs file

#[cfg(feature = "mimalloc-allocator")]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::{fs, io, str};
use std::path::{Path, PathBuf};

use brik::asm::Assembler;
use brik::asm::arch::Arch;
use brik::object::{
    FileFlags,
    Endianness,
    BinaryFormat,
};

use clap::Parser;
use anyhow::{Context, Result};

#[macro_use]
mod util;

mod sym;
mod parse;
mod reader;
mod encoder;
mod mnemonic;
mod assembler;

use encoder::Encoder;

#[derive(Debug, Parser)]
#[command(version, about = "Single-pass RISC-V assembler")]
struct Args {
    /// Input assembly file (UTF-8)
    #[arg(required = true)]
    input: PathBuf,

    /// Output object file
    #[arg(short, long)]
    out: Option<PathBuf>,

    /// Target ISA (brik uses this to bake .riscv.attributes)
    #[arg(long, default_value = "rv64gc")]
    isa: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let out_path = args
        .out
        .clone()
        .unwrap_or_else(|| default_out(&args.input).expect("output"));

    let in_display  = args.input.display();
    let out_display = out_path.display();

    let mut asm = Assembler::new(
        BinaryFormat::Elf,
        Arch::Riscv64,
        Endianness::Little,
        &args.isa
    );

    asm.set_object_flags(FileFlags::Elf {
        os_abi: 0,
        abi_version: 0,
        e_flags: 0x4
    });

    let asm = assembler::Assembler::new(Encoder::new(asm));

    let obj = reader::with_file(&args.input, |src| {
        let src = unsafe {
            str::from_utf8_unchecked(src)
        };

        asm.assemble_file(&args.input, src)
            .with_context(|| format!("assembling {in_display}"))
    })??;

    let handle = fs::File::create(&out_path)?;
    let mut buffered = io::BufWriter::new(handle);
    if let Err(e) = obj.write_stream(&mut buffered) {
        eprintln!("couldn't write to {out_display}: {e}")
    }

    println!("[wrote object file to {out_display}]");

    Ok(())
}

fn default_out(input: &Path) -> Result<PathBuf> {
    const DEFAULT_OUT_FILE_NAME: &str = "a";

    let stem = input.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(DEFAULT_OUT_FILE_NAME);

    Ok(PathBuf::from(format!("{stem}.o")))
}
