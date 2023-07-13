use indexmap::IndexMap;
use proc_macro2::Literal;
use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use quote::quote;
use svd_parser::svd::{Device, Peripheral};

use crate::helper::{gen_function_write_from_buffer, gen_function_read_from_buffer};
use crate::register::RegisterFunctions;
use crate::{formater::*, memory::MemoryPage, ADDR_BITS};
use crate::{memory, PAGE_LEN};

pub struct Peripherals<'a> {
    pub memory: Vec<MemoryPage>,
    pub peripheral_structs: Vec<PeripheralStruct<'a>>,
    pub registers: IndexMap<u64, RegisterFunctions<'a>>,
    pub svd: &'a Device,
}

impl<'a> Peripherals<'a> {
    pub fn new(svd: &'a Device) -> Self {
        let peripheral_structs = svd
            .peripherals
            .iter()
            .enumerate()
            .map(|(i, per)| PeripheralStruct::new(per, i))
            .collect();
        let mut registers: IndexMap<u64, RegisterFunctions<'a>> =
            IndexMap::new();
        for (per_i, per) in svd.peripherals.iter().enumerate() {
            for reg in per.registers() {
                let addr = per.base_address + reg.address_offset as u64;
                let bits = reg
                    .properties
                    .size
                    .or(per.default_register_properties.size)
                    .unwrap();
                assert!(bits % 8 == 0);
                let bytes = bits / 8;
                let entry = registers.entry(addr).or_insert_with(|| {
                    RegisterFunctions::new_empty(bytes, reg)
                });
                entry.add(bytes, per_i, reg);
            }
        }
        let memory = memory::pages_from_chunks(ADDR_BITS, &svd.peripherals);
        Self {
            memory,
            peripheral_structs,
            svd,
            registers,
        }
    }
    fn gen_struct(&self, tokens: &mut TokenStream) {
        let peripherals_field = self.peripheral_structs.iter().map(|per| {
            let mod_name = &per.mod_name;
            let struct_name = &per.struct_name;
            quote! { pub #mod_name: #mod_name::#struct_name}
        });
        tokens.extend(quote! {
            #[derive(Default)]
            pub struct Peripherals {
                #(#peripherals_field,)*
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
        self.gen_struct(tokens);
        self.peripheral_structs
            .iter()
            .for_each(|per| per.gen_mod(self, tokens));
        gen_function_write_from_buffer(tokens);
        gen_function_read_from_buffer(tokens);
        let pages = self.memory
            .iter()
            .map(|mem| mem.gen_pages(self));
        tokens.extend(quote!{pub mod pages {
            #(#pages)*
        }});
        self.gen_map_pages(tokens);
    }
}

pub struct PeripheralStruct<'a> {
    pub index: usize,
    pub mod_name: Ident,
    pub struct_name: Ident,
    pub peripheral: &'a Peripheral,
}

impl<'a> PeripheralStruct<'a> {
    pub fn new(peripheral: &'a Peripheral, index: usize) -> Self {
        let mod_name = format_ident!("{}", snake_case(&peripheral.name));
        let struct_name = format_ident!("{}", camel_case(&peripheral.name));
        Self {
            index,
            mod_name,
            struct_name,
            peripheral,
        }
    }
    pub fn gen_mod(
        &self,
        peripherals: &Peripherals<'a>,
        token: &mut TokenStream,
    ) {
        let mod_name = &self.mod_name;
        let struct_name = &self.struct_name;
        let fields = self.peripheral.registers().flat_map(|reg| {
            let addr = self.peripheral.base_address + reg.address_offset as u64;
            peripherals
                .registers
                .get(&addr)
                .unwrap()
                .fields
                .iter()
                .filter(|field| field.peripheral_index == self.index)
        });
        token.extend(quote! {
            mod #mod_name {
                pub struct #struct_name {}
                impl Default for #struct_name { fn default() -> Self { Self {} } }
                impl #struct_name {
                    #(#fields)*
                }
            }
        });
    }
}
