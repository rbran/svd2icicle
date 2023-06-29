mod formater;
use formater::*;

mod helper;
use helper::*;

mod peripheral;
mod field;

use std::path::Path;

use anyhow::Result;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use svd_parser::svd::Peripheral;

const ADDR_BITS: u32 = 12; //AKA page is 0x1000 bytes
const PAGE_LEN: u64 = 1 << ADDR_BITS;

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
    let peripherals = svd.peripherals.iter().map(gen_peripheral);
    let peripherals_field = svd.peripherals.iter().map(|per| {
        let mod_name = snake_case(&per.name);
        let struct_name = camel_case(&per.name);
        quote! { #mod_name: #mod_name::#struct_name}
    });
    let memory_pages = memory_pages_from_chunks(ADDR_BITS, &svd.peripherals);
    let memory_cpu_map = gen_memory_cpu_map(&memory_pages);
    let memory_impl = gen_memory_impl(&memory_pages);

    let output = quote! {
        #(#peripherals)*
        #[derive(Default)]
        pub struct Peripherals {
            #(#peripherals_field,)*
        }
        #(#memory_impl)*
        #memory_cpu_map
    };
    std::fs::write(lib_rs, output.to_string().as_bytes())?;
    Ok(())
}

fn gen_memory_cpu_map(memory_pages: &[MemoryPage]) -> TokenStream {
    let memory_cpu_map = memory_pages.iter().map(|page| {
        let addr_start = Literal::u64_unsuffixed(page.addr);
        let pseudo_struct = format_ident!("PeripheralPage0x{:X}", page.addr);
        quote!{
            let io = _cpu.mem.register_io_handler(#pseudo_struct(std::sync::Arc::clone(_pe)));
            _cpu.mem.map_memory_len(#addr_start, #PAGE_LEN, io);
        }
    });
    quote! {
        pub fn map_cpu(_pe: &std::sync::Arc<std::sync::Mutex<Peripherals>>, _cpu: &mut icicle_vm::cpu::Cpu) {
            #(#memory_cpu_map)*
        }
    }
}

fn gen_memory_impl<'a>(
    memory_pages: &'a [MemoryPage],
) -> impl Iterator<Item = TokenStream> + 'a {
    let addr_mask = u64::MAX >> (u64::BITS - ADDR_BITS);
    memory_pages.iter().map(move |page| {
        let pseudo_struct = format_ident!("PeripheralPage0x{:X}", page.addr);
        let read = page.chunks.iter().map(|chunk| {
            let start = Literal::u64_unsuffixed(chunk.0.start - page.addr);
            let startp1 = Literal::u64_unsuffixed((chunk.0.start + 1)  - page.addr);
            let endm1 = Literal::u64_unsuffixed((chunk.0.end - 1) - page.addr);
            let end = Literal::u64_unsuffixed(chunk.0.end - page.addr);
            quote! {
                (#start..=#endm1, #startp1..=#end) => todo!(),
            }
        });
        let write = page.chunks.iter().map(move |chunk| {
            let start = Literal::u64_unsuffixed(chunk.0.start - page.addr);
            let startp1 = Literal::u64_unsuffixed((chunk.0.start + 1)  - page.addr);
            let endm1 = Literal::u64_unsuffixed((chunk.0.end - 1) - page.addr);
            let end = Literal::u64_unsuffixed(chunk.0.end - page.addr);
            quote! {
                (#start..=#endm1, #startp1..=#end) => todo!(),
            }
        });
        let addr_mask = Literal::u64_unsuffixed(addr_mask);
        quote! {
            struct #pseudo_struct(std::sync::Arc<std::sync::Mutex<Peripherals>>);
            impl icicle_vm::cpu::mem::IoMemory for #pseudo_struct {
                fn read(
                    &mut self,
                    _addr: u64,
                    _buf: &mut [u8],
                ) -> icicle_vm::cpu::mem::MemResult<()> {
                    let _start = _addr & #addr_mask;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #(#read)*
                        _ => Err(icicle_vm::cpu::mem::MemError::Unmapped),
                    }
                }
                fn write(
                    &mut self,
                    _addr: u64,
                    _buf: &[u8],
                ) -> icicle_vm::cpu::mem::MemResult<()> {
                    let _start = _addr & #addr_mask;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #(#write)*
                        _ => Err(icicle_vm::cpu::mem::MemError::Unmapped),
                    }
                }
            }
        }
    })
}

fn gen_peripheral(peripheral: &Peripheral) -> TokenStream {
    let mod_name = snake_case(&peripheral.name);
    let struct_name = camel_case(&peripheral.name);
    let fields = peripheral
        .registers()
        .flat_map(|reg| {
            let (reg_info, dim) = match reg {
                svd_parser::svd::MaybeArray::Single(reg) => (reg, None),
                svd_parser::svd::MaybeArray::Array(reg, dim) => {
                    (reg, Some(dim))
                }
            };
            reg.fields().zip(std::iter::repeat((reg_info, dim)))
        })
        .map(|(field, (reg, dim))| {
            let field_bytes =
                (((field.bit_offset() % 8) + field.bit_width()) + 7) / 8;
            let reg_name = snake_case(&dim_to_N(&reg.name));
            let read_params = if field_bytes == 1 {
                quote! { _byte: &mut u8 }
            } else {
                (0..field_bytes)
                    .into_iter()
                    .map(|i| {
                        let name = format_ident!("_byte_{}", i);
                        quote! { #name: Option<&mut u8>, }
                    })
                    .collect()
            };
            let write_params = if field_bytes == 1 {
                quote! { _byte: &u8 }
            } else {
                (0..field_bytes)
                    .into_iter()
                    .map(|i| {
                        let name = format_ident!("_byte_{}", i);
                        quote! { #name: Option<&u8>, }
                    })
                    .collect()
            };
            let read_fun =
                format_ident!("read_{}_{}", reg_name, snake_case(&field.name));
            let write_fun =
                format_ident!("write_{}_{}", reg_name, snake_case(&field.name));
            let dim = dim.map(|_| quote! {_dim: usize,});
            let read = reg
                .properties
                .access
                .map(|c| c.can_read())
                .unwrap_or(false)
                .then(|| {
                    quote! {
                        fn #read_fun(#dim #read_params) {
                            todo!();
                        }
                    }
                });
            let write = reg
                .properties
                .access
                .map(|c| c.can_write())
                .unwrap_or(false)
                .then(|| {
                    quote! {
                        fn #write_fun(#dim #write_params) {
                            todo!();
                        }
                    }
                });
            quote! {
                #read #write
            }
        });
    quote! {pub mod #mod_name {
        pub struct #struct_name {}
        impl Default for #struct_name { fn default() -> Self { Self {} } }
        impl #struct_name {
            #(#fields)*
        }
    }}
}
