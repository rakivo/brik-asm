// TODO(#2): Properly manage symbol sizes

use std::fs;
use std::path::{Path, PathBuf};

use clap::Parser;
use anyhow::{Context, Result};

use brik::asm::Assembler;
use brik::asm::arch::Arch;
use brik::object::{
    FileFlags,
    Endianness,
    BinaryFormat,
};

#[macro_use]
mod util;

mod parse;
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

    let mut asm = Assembler::new(
        BinaryFormat::Elf,
        Arch::Riscv64,
        Endianness::Little,
        &args.isa,
    );

    asm.set_object_flags(FileFlags::Elf {
        os_abi: 0,
        abi_version: 0,
        e_flags: 0x4,
    });

    let in_display  = args.input.display();
    let out_display = out_path.display();

    let src = fs::read_to_string(&args.input)
        .with_context(|| format!("reading {in_display}"))?;

    let asm = assembler::Assembler::new(Encoder(asm));

    let obj = asm.assemble_file(&args.input, &src)
        .with_context(|| format!("assembling {in_display}"))?;

    let handle = fs::File::create(&out_path)?;
    if let Err(e) = obj.write_stream(&handle) {
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
