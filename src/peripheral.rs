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
        for svd in svds {
            for per in svd.peripherals.iter() {
                // only allow derived if there is no registers, and vise-versa
                assert!(
                    per.derived_from.is_some() ^ (per.registers().count() > 0)
                );
                let per = if let Some(derived) = per.derived_from.as_ref() {
                    let derived = svd.get_peripheral(derived).unwrap();
                    assert!(derived.registers().count() > 0);
                    derived
                } else {
                    per
                };
                for reg in per.registers() {
                    let addr = per.base_address + reg.address_offset as u64;
                    registers
                        .entry(addr)
                        .and_modify(|regs| regs.push((per, reg)))
                        .or_insert_with(|| vec![(per, reg)]);
                }
                for clu in per.clusters() {
                    let addr = per.base_address + clu.address_offset as u64;
                    for reg in clu.registers() {
                        let addr = addr + reg.address_offset as u64;
                        registers
                            .entry(addr)
                            .and_modify(|regs| regs.push((per, reg)))
                            .or_insert_with(|| vec![(per, reg)]);
                    }
                }
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
            pub mod peripheral {
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
                use icicle_vm::cpu::mem::{MemError, MemResult};
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
