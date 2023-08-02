mod formater;

mod helper;
use peripheral::Peripherals;

mod field;
mod memory;
mod peripheral;
mod register;

use std::path::Path;

use anyhow::Result;
use proc_macro2::TokenStream;

pub const ADDR_BITS: u32 = 12; //AKA page is 0x1000 bytes
pub const PAGE_LEN: u64 = 1 << ADDR_BITS;
pub const PAGE_MASK: u64 = u64::MAX << ADDR_BITS;
pub const ADDR_MASK: u64 = !PAGE_MASK;

pub fn gen_empty_implementation(svds: &[&Path], lib_rs: &Path) -> Result<()> {
    let svds = svds.iter().map(|svd| {
        let svd_data = std::fs::read_to_string(svd)?;
        svd_parser::parse(&svd_data)
    }).collect::<Result<Vec<_>,_>>()?;
    let peripherals = Peripherals::new(&svds)?;
    let mut output = TokenStream::new();
    peripherals.gen_all(&mut output);
    std::fs::write(lib_rs, output.to_string().as_bytes())?;
    Ok(())
}
