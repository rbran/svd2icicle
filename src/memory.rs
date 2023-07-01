use std::ops::Range;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::Peripheral;

use crate::{peripheral::Peripherals, ADDR_MASK, PAGE_MASK};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryPage {
    pub pseudo_struct: Ident,
    pub addr: u64,
    pub chunks: Vec<MemoryChunk>,
}

impl MemoryPage {
    pub fn gen_pages<'a, 'b: 'a>(
        &'b self,
        peripherals: &'b Peripherals<'a>,
    ) -> impl ToTokens + 'b {
        struct Pages<'a, 'b>(&'b MemoryPage, &'b Peripherals<'a>);
        impl ToTokens for Pages<'_, '_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_pseudo_struct(self.1, tokens)
            }
        }
        Pages(self, peripherals)
    }
    fn gen_pseudo_struct(
        &self,
        peripherals: &Peripherals,
        tokens: &mut TokenStream,
    ) {
        let pseudo_struct = &self.pseudo_struct;
        let read = self.chunks.iter().map(|chunk| {
            let start = Literal::u64_unsuffixed(chunk.0.start - self.addr);
            let startp1 =
                Literal::u64_unsuffixed((chunk.0.start + 1) - self.addr);
            let endm1 = Literal::u64_unsuffixed((chunk.0.end - 1) - self.addr);
            let end = Literal::u64_unsuffixed(chunk.0.end - self.addr);
            quote! {
                (#start..=#endm1, #startp1..=#end) => todo!(),
            }
        });
        let write = self.chunks.iter().map(move |chunk| {
            let start = Literal::u64_unsuffixed(chunk.0.start - self.addr);
            let startp1 =
                Literal::u64_unsuffixed((chunk.0.start + 1) - self.addr);
            let endm1 = Literal::u64_unsuffixed((chunk.0.end - 1) - self.addr);
            let end = Literal::u64_unsuffixed(chunk.0.end - self.addr);
            quote! {
                (#start..=#endm1, #startp1..=#end) => todo!(),
            }
        });
        let register_functions =
            peripherals.registers.iter().filter_map(|(addr, reg)| {
                (*addr & PAGE_MASK == self.addr)
                    .then(|| reg.functions(peripherals))
            });
        let addr_mask = Literal::u64_unsuffixed(ADDR_MASK);
        tokens.extend(quote! {
            pub struct #pseudo_struct(pub std::sync::Arc<std::sync::Mutex<crate::Peripherals>>);
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
            impl #pseudo_struct {
                #(#register_functions)*
            }
        });
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryChunk(pub Range<u64>);

/// create a page-mapped list of peripherals
/// TODO check that each page is aligned
pub fn pages_from_chunks<'a>(
    addr_bits: u32,
    peripherals: &[Peripheral],
) -> Vec<MemoryPage> {
    let page_mask = (u64::MAX >> addr_bits) << addr_bits;
    let mut chunks = Vec::new();
    for per in peripherals.iter() {
        for reg in per.all_registers() {
            let start = per.base_address + reg.address_offset as u64;
            let bits = reg
                .properties
                .size
                .or(per.default_register_properties.size)
                .unwrap();
            match reg {
                svd_parser::svd::MaybeArray::Single(_reg) => {
                    let end = start + (bits as u64 / 8);
                    // don't allow register between pages
                    assert_eq!(
                        start & page_mask,
                        (end - 1) & page_mask,
                        "reg start {start} end {end}"
                    );
                    chunks.push(MemoryChunk(start..end))
                }
                svd_parser::svd::MaybeArray::Array(_reg, dim) => {
                    let mut start = start;
                    for _ in 0..dim.dim {
                        let end = start + (bits as u64 / 8);
                        chunks.push(MemoryChunk(start..end));

                        start += dim.dim_increment as u64;
                    }
                }
            }
        }
    }
    // sort chunks so lowest address comes first, also but biggest first to
    // optimize the algorithm below.
    chunks.sort_unstable_by(|a, b| match a.0.start.cmp(&b.0.start) {
        std::cmp::Ordering::Equal => b.0.end.cmp(&a.0.end),
        x => x,
    });

    // combine multiple chunks into pages
    chunks.into_iter().fold(vec![], |mut pages, chunk| {
        // if overlapping with the last page, just accumulate. Otherwise
        // start a new page
        let chunk_page = chunk.0.start & page_mask;
        match pages.last_mut() {
            // if it belongs to the same page, add this chunk to the last page
            Some(last) if last.addr == chunk_page => {
                // merge the chunks if they are intersecting/sequential
                match last.chunks.last_mut() {
                    Some(last) if last.0.end >= chunk.0.start => {
                        last.0.end = last.0.end.max(chunk.0.end)
                    }
                    _ => last.chunks.push(chunk),
                }
            }
            // start a new page
            _ => pages.push(MemoryPage {
                pseudo_struct: format_ident!(
                    "PeripheralPage0x{:X}",
                    chunk_page
                ),
                addr: chunk_page,
                chunks: vec![chunk],
            }),
        }
        pages
    })
}
