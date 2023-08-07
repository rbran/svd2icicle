use std::ops::Range;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};

use crate::{
    helper, peripheral::Peripherals, register::RegisterAccess, PAGE_MASK,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryPage {
    pub pseudo_struct: Ident,
    pub page_offset: u64,
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
        let register_functions = peripherals
            .registers
            .iter()
            .filter(|reg| reg.base_addr & PAGE_MASK == self.page_offset)
            .map(|reg| reg.mem_map_functions());

        struct ChunkCall<'a, 'b>(
            &'a MemoryPage,
            &'a Peripherals<'b>,
            &'a MemoryChunk,
            bool,
        );
        impl ToTokens for ChunkCall<'_, '_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_chunk_read_write(self.1, self.2, self.3, tokens)
            }
        }
        let read = self
            .chunks
            .iter()
            .map(|chunk| ChunkCall(self, peripherals, chunk, true));
        let write = self
            .chunks
            .iter()
            .map(|chunk| ChunkCall(self, peripherals, chunk, false));

        let page_offset = Literal::u64_unsuffixed(self.page_offset);
        tokens.extend(quote! {
            pub(crate) struct #pseudo_struct(
                pub std::sync::Arc<std::sync::Mutex<super::peripheral::Peripherals>>
            );
            impl icicle_vm::cpu::mem::IoMemory for #pseudo_struct {
                fn read(
                    &mut self,
                    _addr: u64,
                    _buf: &mut [u8],
                ) -> MemResult<()> {
                    let _start = _addr - #page_offset;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #(#read)*
                        _ => return Err(MemError::Unmapped),
                    }
                }
                fn write(
                    &mut self,
                    _addr: u64,
                    _buf: &[u8],
                ) -> MemResult<()> {
                    let _start = _addr - #page_offset;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #(#write)*
                        _ => return Err(MemError::Unmapped),
                    }
                }
            }
            impl #pseudo_struct {
                #(#register_functions)*
            }
        });
    }

    fn gen_chunk_read_write(
        &self,
        peripherals: &Peripherals,
        chunk: &MemoryChunk,
        read: bool,
        tokens: &mut TokenStream,
    ) {
        let page_offset = self.page_offset;
        let start = chunk.0.start - page_offset;
        let end = chunk.0.end - page_offset;
        // NOTE is very common to have a entier block read/write only, so
        // if this is the case return an error for the block.
        let startp1 = Literal::u64_unsuffixed(start + 1);
        let start = (start != 0).then_some(Literal::u64_unsuffixed(start));
        let endm1 = Literal::u64_unsuffixed(end - 1);
        let end = Literal::u64_unsuffixed(end);
        let not_available_block = peripherals
            .registers
            .iter()
            .filter(|reg| reg.base_addr & PAGE_MASK == self.page_offset)
            .all(|reg| {
                if read {
                    reg.read_fun.is_none()
                } else {
                    reg.write_fun.is_none()
                }
            });
        if not_available_block {
            if read {
                tokens.extend(quote! {
                    (#start..=#endm1, #startp1..=#end) => return Err(MemError::ReadViolation),
                });
            } else {
                tokens.extend(quote! {
                    (#start..=#endm1, #startp1..=#end) => return Err(MemError::WriteViolation),
                });
            }
        } else {
            let registers_write = peripherals
                .registers
                .iter()
                .filter(|reg| reg.base_addr & PAGE_MASK == self.page_offset)
                .flat_map(|reg| {
                    self.call_register_from_chunk(chunk, reg, read)
                });
            tokens.extend(quote! {
                (#start..=#endm1, #startp1..=#end) => {
                    #(#registers_write)*
                    Ok(())
                },
            });
        }
    }

    fn call_register_from_chunk<'b>(
        &'b self,
        chunk: &'b MemoryChunk,
        reg: &'b RegisterAccess,
        read: bool,
    ) -> impl Iterator<Item = TokenStream> + 'b {
        reg.dim_iter().enumerate().map(move |(dim_i, reg_addr)| {
            // if not in this page or chunk, do nothing
            if !chunk.0.contains(&reg_addr) {
                return quote!{};
            }
            let value = format_ident!("_value");
            let dim = reg.is_dim().then(|| Literal::usize_unsuffixed(dim_i));

            let offset_start = reg_addr - chunk.0.start;
            let offset_end = offset_start + reg.bytes as u64;

            let offset_start_lit = Literal::u64_unsuffixed(offset_start);
            let offset_end_lit = Literal::u64_unsuffixed(offset_end);

            // just to avoid the warning "comparison is useless due to type limits"
            // for `var_uint < 0`
            let in_range_start = (offset_start > 0).then(|| {
                quote!{_start < #offset_start_lit }
            }).into_iter();
            let in_range = quote! {
                #(#in_range_start &&)* _end > #offset_end_lit
            };
            if read {
                let call = if let Some(read) = reg.read_fun.as_ref() {
                    // if single field, use the peripheral call directly,
                    // otherwise call the register implementation from the pages
                    // struct
                    let fun = if reg.is_single_field() {
                        quote!{ self.0.lock().unwrap().#read }
                    } else {
                        quote! { self.#read }
                    };
                    let bytes = (offset_start..offset_end).enumerate().map(|(byte_i, byte)| {
                        let byte = Literal::u64_unsuffixed(byte);
                        let byte_i = Literal::usize_unsuffixed(byte_i);
                        quote!{
                            if _start <= #byte && _end > #byte {
                                _buf[(#byte - _start) as usize] =
                                    #value[#byte_i];
                            }
                        }
                    });
                    // TODO implement byte endian here
                    quote!{
                        let #value = #fun(#dim)?.to_ne_bytes();
                        #(#bytes)*
                    }
                } else {
                    quote! { return Err(MemError::ReadViolation); }
                };
                quote! {
                    if #in_range { #call }
                }
            } else {
                if let Some(write) = reg.write_fun.as_ref() {
                    let reg_bytes = Literal::u32_unsuffixed(reg.bytes);
                    // if single field, use the peripheral call directly,
                    // otherwise call the register implementation from the pages
                    // struct
                    if reg.is_single_field() {
                        let part_no_impl = format!(
                            "partial write for {} {} not implemented",
                            &reg.regs[0].0.name,
                            &reg.regs[0].1.name,
                        );
                        let value_type = helper::DataType::from_bytes(reg.bytes);
                        let dim = dim.into_iter();
                        // TODO implement byte endian here
                        quote! {
                            if #in_range {
                                assert!(
                                    _start <= #offset_start_lit && _end >= #offset_end,
                                    #part_no_impl,
                                );
                                let start = _start.saturating_sub(#offset_start_lit) as usize;
                                let end = (_end.saturating_sub(#offset_start_lit) as usize)
                                    .min(start + #reg_bytes);
                                self.0.lock().unwrap().#write(
                                    #(#dim,)*
                                    #value_type::from_ne_bytes(
                                        _buf[start..end].try_into().unwrap()
                                    )
                                )?;
                            }
                        }
                    } else {
                        let dim = dim.into_iter();
                        quote! {
                            if #in_range {
                                let offset = #offset_start.saturating_sub(_start);
                                let start = _start.saturating_sub(#offset_start_lit) as usize;
                                let end = (_end.saturating_sub(#offset_start_lit) as usize)
                                    .min(start + #reg_bytes);
                                self.#write(#(#dim,)* offset, &_buf[start..end])?;
                            }
                        }
                    }
                } else {
                    quote! {
                        if #in_range { return Err(MemError::ReadViolation); }
                    }
                }
            }
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemoryChunk(pub Range<u64>);

/// create a page-mapped list of peripherals
/// TODO check that each page is aligned
pub fn pages_from_chunks(
    addr_bits: u32,
    registers: &[RegisterAccess],
) -> Vec<MemoryPage> {
    let page_mask = (u64::MAX >> addr_bits) << addr_bits;
    let mut chunks = Vec::new();
    for regs in registers.iter() {
        for reg_addr in regs.dim_iter() {
            let start = reg_addr;
            let end = start + regs.bytes as u64;
            assert_eq!(
                start & page_mask,
                (end - 1) & page_mask,
                "reg start {start} end {end}"
            );
            chunks.push(MemoryChunk(start..end))
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
            Some(last) if last.page_offset == chunk_page => {
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
                page_offset: chunk_page,
                chunks: vec![chunk],
            }),
        }
        pages
    })
}
