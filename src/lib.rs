use proc_macro2::TokenStream;
use std::path::Path;

mod enumeration;
mod field;
mod formater;
mod helper;
mod memory;
mod peripheral;
mod register;

use peripheral::Device;

pub const ADDR_BITS: u32 = 12;
pub const PAGE_LEN: u64 = 1 << ADDR_BITS;
pub const PAGE_MASK: u64 = u64::MAX << ADDR_BITS;
pub const ADDR_MASK: u64 = !PAGE_MASK;
pub fn gen_empty_implementation(svds: &[&Path], lib_rs: &Path) {
    let svds = svds
        .iter()
        .map(|svd| {
            let svd_data = std::fs::read_to_string(svd).unwrap();
            svd_parser::parse(&svd_data).unwrap()
        })
        .collect::<Vec<_>>();
    let peripherals = Device::new(&svds);
    let mut output = TokenStream::new();
    peripherals.gen_all(&mut output);
    std::fs::write(lib_rs, output.to_string().as_bytes()).unwrap();
}
