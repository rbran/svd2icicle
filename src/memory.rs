use std::ops::Range;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::Device;

use crate::{
    helper, peripheral::Peripherals, register::RegisterAccess, PAGE_MASK,
};

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
        let (read, write): (TokenStream, TokenStream) = self
            .chunks
            .iter()
            .map(|chunk| self.gen_chunk_read_write(peripherals, chunk))
            .unzip();
        let register_functions =
            peripherals.registers.iter().filter_map(|(addr, reg)| {
                (*addr & PAGE_MASK == self.addr)
                    .then(|| reg.mem_map_functions())
            });
        let page_offset = Literal::u64_unsuffixed(self.addr);
        tokens.extend(quote! {
            pub struct #pseudo_struct(
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
                        #read
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
                        #write
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
    ) -> (TokenStream, TokenStream) {
        let page_offset = self.addr;
        let start = chunk.0.start - page_offset;
        let end = chunk.0.end - page_offset;
        let registers_in_chunk = peripherals
            .registers
            .iter()
            .filter(|(addr, _reg)| chunk.0.contains(*addr));
        // NOTE is very common to have a entier block read/write only, so
        // if this is the case return an error for the block.
        let startp1 = Literal::u64_unsuffixed(start + 1);
        let start = (start != 0).then_some(Literal::u64_unsuffixed(start));
        let endm1 = Literal::u64_unsuffixed(end - 1);
        let end = Literal::u64_unsuffixed(end);
        let read = if registers_in_chunk
            .clone()
            .all(|(_addr, reg)| reg.read_fun.is_none())
        {
            quote! { (#start..=#endm1, #startp1..=#end) => return Err(MemError::ReadViolation), }
        } else {
            let registers_read =
                registers_in_chunk.clone().flat_map(|(addr, reg)| {
                    self.call_register_from_chunk(*addr, reg, true)
                });
            quote! {
                (#start..=#endm1, #startp1..=#end) => {
                    #(#registers_read)*
                    Ok(())
                },
            }
        };
        let write = if registers_in_chunk
            .clone()
            .all(|(_addr, reg)| reg.write_fun.is_none())
        {
            quote! { (#start..=#endm1, #startp1..=#end) => return Err(MemError::WriteViolation), }
        } else {
            let registers_write = registers_in_chunk.flat_map(|(addr, reg)| {
                self.call_register_from_chunk(*addr, reg, false)
            });
            quote! {
                (#start..=#endm1, #startp1..=#end) => {
                    #(#registers_write)*
                    Ok(())
                },
            }
        };
        (read, write)
    }

    fn call_register_from_chunk<'b>(
        &'b self,
        base_addr: u64,
        reg: &'b RegisterAccess,
        read: bool,
    ) -> impl Iterator<Item = TokenStream> + 'b {
        (0..reg.dim).map(move |dim_i| {
            let chunk_offset = base_addr - self.addr;
            let dim_offset = dim_i as u64 * reg.bytes as u64;
            let reg_start = chunk_offset + dim_offset;
            let reg_end = reg_start + reg.bytes as u64;
            let range = reg_start..reg_end;
            let value = format_ident!("_value");
            let value_type = helper::DataType::from_bytes(reg.bytes);
            let reg_start = (range.start > 0).then_some(Literal::u64_unsuffixed(range.start)).into_iter();
            let reg_start2 = reg_start.clone();
            let reg_end = Literal::u64_unsuffixed(range.end);
            let dim = (reg.dim > 1).then(|| Literal::u32_unsuffixed(dim_i));
            if read {
                let bytes = range.enumerate().map(|(byte_i, byte)| {
                    let byte = Literal::u64_unsuffixed(byte);
                    let byte_i = Literal::usize_unsuffixed(byte_i);
                    // TODO implement byte endian here
                    quote!{
                        if _start <= #byte && _end > #byte {
                            _buf[(#byte - _start) as usize] = ((#value >> #byte_i) & 0xff) as u8;
                        }
                    }
                });
                let call = reg
                    .gen_mem_page_function_call(dim, None)
                    .map(|call| {
                        quote!{
                            let #value = #call;
                            #(#bytes)*
                        }
                    })
                    .unwrap_or_else(||
                        quote! {
                            return Err(MemError::ReadViolation);
                        }
                    );
                quote! {
                    if (#(_start >= #reg_start &&)* _start < #reg_end)
                        || (#(_end > #reg_start2 &&)* _end <= #reg_end) {
                            #call
                    }
                }
            }else {
                let bytes = range.enumerate().map(|(byte_i, byte)| {
                    let byte = Literal::u64_unsuffixed(byte);
                    let byte_i = Literal::usize_unsuffixed(byte_i);
                    // TODO implement byte endian here
                    // TODO allow partial writes to registers?
                    // TODO pass the &mut u32 directly if data allign?
                    quote!{
                        #value |= (_buf[(#byte - _start) as usize] << #byte_i) as #value_type;
                    }
                });
                let call = reg
                    .gen_mem_page_function_call(dim, Some(&value))
                    .map(|call| {
                        quote!{
                            let mut #value = 0;
                            #(#bytes)*
                            #call;
                        }
                    })
                    .unwrap_or_else(||
                        quote! {
                            return Err(MemError::WriteViolation);
                        }
                    );
                // TODO allow partial writes to registers?
                // TODO pass the &mut u32 directly if data allign?
                let part_no_impl = format!(
                    "partial write ({{}}:{{}}) for {} {} not implemented",
                    &reg.regs[0].0.name,
                    &reg.regs[0].1.name,
                );
                let reg_start3 = Literal::u64_unsuffixed(chunk_offset + dim_offset);
                quote! {
                    if (#(_start >= #reg_start &&)* _start < #reg_end)
                        || (#(_end > #reg_start2 &&)* _end <= #reg_end) {
                        assert!(
                            _start <= #reg_start3 && _end >= #reg_end,
                            #part_no_impl,
                            _start, _end,
                        );
                        #call
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
pub fn pages_from_chunks(addr_bits: u32, svds: &[Device]) -> Vec<MemoryPage> {
    let page_mask = (u64::MAX >> addr_bits) << addr_bits;
    let mut chunks = Vec::new();
    for per in svds.iter().flat_map(|svd| svd.peripherals.iter()) {
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
