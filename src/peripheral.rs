use anyhow::Result;
use indexmap::IndexMap;
use proc_macro2::Literal;
use proc_macro2::TokenStream;
use quote::quote;
use svd_parser::svd::Device;

use crate::register::RegisterAccess;
use crate::{memory, PAGE_LEN};
use crate::{memory::MemoryPage, ADDR_BITS};

pub struct Peripherals<'a> {
    pub memory: Vec<MemoryPage>,
    pub registers: IndexMap<u64, RegisterAccess<'a>>,
    pub svds: &'a [Device],
}

impl<'a> Peripherals<'a> {
    pub fn new(svds: &'a [Device]) -> Result<Self> {
        let mut registers: IndexMap<u64, Vec<_>> = IndexMap::new();
        for per in svds.iter().flat_map(|x| x.peripherals.iter()) {
            for reg in per.registers() {
                let addr = per.base_address + reg.address_offset as u64;
                registers
                    .entry(addr)
                    .and_modify(|regs| regs.push((per, reg)))
                    .or_insert_with(|| vec![(per, reg)]);
            }
        }
        let registers = registers
            .into_iter()
            .map(|(addr, regs)| -> Result<_> {
                Ok((addr, RegisterAccess::new(regs)?))
            })
            .collect::<Result<_, _>>()?;
        let memory = memory::pages_from_chunks(ADDR_BITS, svds);
        Ok(Self {
            memory,
            svds,
            registers,
        })
    }

    fn gen_map_pages(&self, tokens: &mut TokenStream) {
        // memory pages mapping to the cpu
        let memory_cpu_map = self.memory.iter().map(|page| {
            let addr_start = Literal::u64_unsuffixed(page.addr);
            let pseudo_struct = &page.pseudo_struct;
            quote!{
                let io = _cpu.mem.register_io_handler(pages::#pseudo_struct(std::sync::Arc::clone(_pe)));
                _cpu.mem.map_memory_len(#addr_start, #PAGE_LEN, io);
            }
        });
        tokens.extend(quote! {
            pub fn map_cpu(_pe: &std::sync::Arc<std::sync::Mutex<peripheral::Peripherals>>, _cpu: &mut icicle_vm::cpu::Cpu) {
                #(#memory_cpu_map)*
            }
        })
    }

    fn gen_peripheral(&self, tokens: &mut TokenStream) {
        // the register/fields read/write functions
        let regs_funs =
            self.registers.values().map(|reg| reg.fields_functions());
        // all the memory blocks
        tokens.extend(quote! {
            mod peripheral {
                use icicle_vm::cpu::mem::MemResult;
                #[derive(Default)]
                pub struct Peripherals {
                    #[doc = "TODO: implement the peripherals data here"]
                    _todo: (),
                }
                impl Peripherals {
                    #(#regs_funs)*
                }
            }
        });
    }

    fn gen_pages(&self, tokens: &mut TokenStream) {
        // all the memory blocks
        let pages = self.memory.iter().map(|mem| mem.gen_pages(self));
        // gen the empty struct, and read/write functions
        tokens.extend(quote! {
            mod pages {
                use icicle_vm::cpu::mem::MemResult;
                // helper functions to get buffer split
                fn buffer_mut(
                    _start: u64,
                    _end: u64,
                    _byte: u64,
                    _buf: &[u8],
                ) -> Option<&mut u8> {
                    if _start > _byte || _end <= _byte {
                        return None;
                    }
                    let addr = _buf.as_ptr() as usize + (_byte - _start) as usize;
                    Some(unsafe { std::mem::transmute(addr) })
                }
                fn buffer_const(
                    _start: u64,
                    _end: u64,
                    _byte: u64,
                    _buf: &[u8],
                ) -> Option<&u8> {
                    if _start > _byte || _end <= _byte {
                        return None;
                    }
                    Some(&_buf[(_byte - _start) as usize])
                }
                #(#pages)*
            }
        });
    }

    #[allow(dead_code)]
    pub fn gen_all_but_peripherals(&self, tokens: &mut TokenStream) {
        self.gen_pages(tokens);
        self.gen_map_pages(tokens);
    }

    #[allow(dead_code)]
    pub fn gen_all(&self, tokens: &mut TokenStream) {
        self.gen_peripheral(tokens);
        self.gen_pages(tokens);
        self.gen_map_pages(tokens);
    }
}
