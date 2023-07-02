mod formater;

mod helper;
use peripheral::Peripherals;

mod field;
mod register;
mod memory;
mod peripheral;

use std::path::Path;

use anyhow::Result;
use proc_macro2::TokenStream;

pub const ADDR_BITS: u32 = 12; //AKA page is 0x1000 bytes
pub const PAGE_LEN: u64 = 1 << ADDR_BITS;
pub const PAGE_MASK: u64 = u64::MAX << ADDR_BITS;
pub const ADDR_MASK: u64 = !PAGE_MASK;

pub fn gen_empty_implementation(
    svd: impl AsRef<Path>,
    lib_rs: impl AsRef<Path>,
) -> Result<()> {
    let svd = svd.as_ref();
    let lib_rs = lib_rs.as_ref();
    inner_gen_empty_implementation(svd, lib_rs)
}

fn inner_gen_empty_implementation(svd: &Path, lib_rs: &Path) -> Result<()> {
    let svd_data = std::fs::read_to_string(svd)?;
    let svd = svd_parser::parse(&svd_data)?;
    let peripherals = Peripherals::new(&svd);
    let mut output = TokenStream::new();
    peripherals.gen_all(&mut output);
    std::fs::write(lib_rs, output.to_string().as_bytes())?;
    Ok(())
}
