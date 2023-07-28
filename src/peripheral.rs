use anyhow::Result;
use indexmap::IndexMap;
use proc_macro2::Literal;
use proc_macro2::TokenStream;
use quote::quote;
use svd_parser::svd::Device;

use crate::helper::{
    gen_function_read_from_buffer, gen_function_write_from_buffer,
};
use crate::register::RegisterAccess;
use crate::{memory, PAGE_LEN};
use crate::{memory::MemoryPage, ADDR_BITS};

pub struct Peripherals<'a> {
    pub memory: Vec<MemoryPage>,
    pub registers: IndexMap<u64, RegisterAccess<'a>>,
    pub svd: &'a Device,
}

impl<'a> Peripherals<'a> {
    pub fn new(svd: &'a Device) -> Result<Self> {
        let mut registers: IndexMap<u64, Vec<_>> = IndexMap::new();
        for per in svd.peripherals.iter() {
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
            .map(|(addr, regs)| {
                Ok::<_, anyhow::Error>((addr, RegisterAccess::new(regs)?))
            })
            .collect::<Result<_, _>>()?;
        let memory = memory::pages_from_chunks(ADDR_BITS, &svd.peripherals);
        Ok(Self {
            memory,
            svd,
            registers,
        })
    }
    fn gen_struct(&self, tokens: &mut TokenStream) {
        tokens.extend(quote! {
            #[derive(Default)]
            pub struct Peripherals {
                #[doc = "TODO: implement the peripherals data here"]
                _todo: (),
            }
        })
    }

    fn gen_map_pages(&self, tokens: &mut TokenStream) {
        let memory_cpu_map = self.memory.iter().map(|page| {
            let addr_start = Literal::u64_unsuffixed(page.addr);
            let pseudo_struct = &page.pseudo_struct;
            quote!{
                let io = _cpu.mem.register_io_handler(pages::#pseudo_struct(std::sync::Arc::clone(_pe)));
                _cpu.mem.map_memory_len(#addr_start, #PAGE_LEN, io);
            }
        });
        tokens.extend(quote! {
            pub fn map_cpu(_pe: &std::sync::Arc<std::sync::Mutex<Peripherals>>, _cpu: &mut icicle_vm::cpu::Cpu) {
                #(#memory_cpu_map)*
            }
        })
    }

    pub fn gen_all(&self, tokens: &mut TokenStream) {
        // gen the empty struct
        self.gen_struct(tokens);
        // then the register functions
        let regs_funs =
            self.registers.values().map(|reg| reg.fields_functions());
        tokens.extend(quote! {
            impl Peripherals {
                #(#regs_funs)*
            }
        });
        // helper functions to get buffer split
        gen_function_write_from_buffer(tokens);
        gen_function_read_from_buffer(tokens);
        // all the memory blocks
        let pages = self.memory.iter().map(|mem| mem.gen_pages(self));
        tokens.extend(quote! {pub mod pages {
            #(#pages)*
        }});
        self.gen_map_pages(tokens);
    }
}
