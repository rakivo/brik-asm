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
use assembler::{Sections, assemble_file};

#[derive(Parser, Debug)]
#[command(version, about = "Single-pass RISC-V assembler (sections, labels, mnemonics)")]
struct Args {
    /// Input assembly file (UTF-8)
    #[arg(required = true)]
    input: PathBuf,

    /// Output file (ELF relocatable by default)
    #[arg(short, long)]
    out: Option<PathBuf>,

    /// Target ISA (brik uses this to bake .riscv.attributes). Default: rv32i
    #[arg(long, default_value = "rv32i")]
    isa: String,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let out_path = args
        .out
        .clone()
        .unwrap_or_else(|| default_out(&args.input).expect("output"));

    let asm = Assembler::new(
        BinaryFormat::Elf,
        Arch::Riscv64,
        Endianness::Little,
        &args.isa,
    );

    let mut encoder = Encoder(asm);

    encoder.set_object_flags(FileFlags::Elf {
        os_abi: 0,
        abi_version: 0,
        e_flags: 0x4,
    });

    let sections = Sections {
        rodata : encoder.add_rodata_section(),
        text   : encoder.add_text_section(),
        data   : encoder.add_data_section(),
        bss    : encoder.add_bss_section(),
    };

    let display = out_path.display();

    let src = fs::read_to_string(&args.input)
        .with_context(|| format!("reading {display}"))?;

    let encoder = assemble_file(&args.input, &src, encoder, &sections)
        .with_context(|| format!("assembling {display}"))?;

    let obj = encoder.finish().unwrap();

    let handle = fs::File::create(&out_path)?;
    if let Err(e) = obj.write_stream(&handle) {
        eprintln!("couldn't write to {display}: {e}")
    }

    Ok(())
}

fn default_out(input: &Path) -> Result<PathBuf> {
    const DEFAULT_OUT_FILE_NAME: &str = "a";

    let stem = input.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(DEFAULT_OUT_FILE_NAME);

    Ok(PathBuf::from(format!("{stem}.o")))
}
