use std::collections::HashMap;

use anyhow::{bail, Result};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::{Device, MaybeArray, Name, PeripheralInfo};

use crate::formater::{camel_case, dim_to_n, snake_case};
use crate::helper::Dim;
use crate::memory::{Memory, MemoryChunks, MemoryThingFinal};
use crate::{ADDR_BITS, ADDR_MASK, PAGE_LEN, PAGE_MASK};

pub struct Peripherals<'a> {
    pub svds: &'a [Device],
    pub pages: Vec<PeripheralPage<'a>>,
    //pub registers: Vec<RegisterAccess<'a>>,
}

impl<'a> Peripherals<'a> {
    pub fn new(svds: &'a [Device]) -> Result<Self> {
        let mut peripherals: HashMap<u64, (_, Vec<_>)> = HashMap::new();
        for svd in svds {
            for per in svd.peripherals.iter() {
                let page = per.base_address & PAGE_MASK;
                peripherals.entry(page).or_insert((svd, vec![])).1.push(per);
            }
        }
        // TODO improve this, consume the peripherals HashMap -> Vec -> Vec
        // could be avoided. This is here so we can be sure the errors in
        // PeripheralPage errors happen are reproducible. The problem is that
        // consuming directly from the HashMap return elements in a random
        // order.
        let mut pages: Vec<(&Device, Vec<_>)> = peripherals
            .into_iter()
            .map(|(_page, (svd, pers))| (svd, pers))
            .collect();
        pages.sort_unstable_by_key(|(_svd, pers)| {
            pers[0].base_address & PAGE_MASK
        });
        let pages: Vec<_> = pages
            .into_iter()
            .map(|(svd, pers)| Ok(PeripheralPage::new(svd, pers)?))
            .collect::<Result<_, anyhow::Error>>()?;

        //let memory = memory::pages_from_chunks(ADDR_BITS, &registers);
        Ok(Self {
            //memory,
            svds,
            pages,
        })
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

    fn gen_peripheral(&self, tokens: &mut TokenStream) {
        // structs for each peripherals
        let per_structs = self.pages.iter().map(PeripheralPage::gen_struct);
        // structs fields for each peripherals in the main Struct
        let per_structs_fields =
            self.pages.iter().map(PeripheralPage::gen_struct_field);
        // register function that could be overwriten by the user
        let register_functions =
            self.pages.iter().map(PeripheralPage::gen_register_fun);
        //// the register/fields read/write functions
        //let regs_funs = self.pages.iter().map(Peripherals::fields_functions);
        // all the memory blocks
        tokens.extend(quote! {
            pub mod peripheral {
                #(#per_structs)*
                #[derive(Default)]
                pub struct Peripherals {
                    #(pub #per_structs_fields)*
                }
                pub(crate) mod registers {
                    use icicle_vm::cpu::mem::{MemResult, MemError};
                    impl super::Peripherals {
                        #(#register_functions)*
                    }
                }
            }
        });
    }

    fn gen_pages(&self, tokens: &mut TokenStream) {
        // all the memory blocks
        let pages = self.pages.iter().map(PeripheralPage::gen_pages);
        // gen the empty struct, and read/write functions
        tokens.extend(quote! {
            mod pages {
                use icicle_vm::cpu::mem::{MemError, MemResult};
                #(#pages)*
            }
        });
    }

    fn gen_map_pages(&self, tokens: &mut TokenStream) {
        // memory pages mapping to the cpu
        let memory_cpu_map = self.pages.iter().map(|page| {
            let addr_start = Literal::u64_unsuffixed(
                page.peripherals[0].base_address & PAGE_MASK,
            );
            let pseudo_struct = &page.pseudo_struct;
            quote! {
                let io = _cpu.mem.register_io_handler(
                    pages::#pseudo_struct(std::sync::Arc::clone(_pe))
                );
                _cpu.mem.map_memory_len(#addr_start, #PAGE_LEN, io);
            }
        });
        tokens.extend(quote! {
            pub fn map_cpu(
                _pe: &std::sync::Arc<std::sync::Mutex<peripheral::Peripherals>>,
                _cpu: &mut icicle_vm::cpu::Cpu,
            ) {
                #(#memory_cpu_map)*
            }
        })
    }
}

pub struct PeripheralPage<'a> {
    pub pseudo_struct: Ident,
    pub mod_name: Ident,
    pub peripheral_struct: Ident,
    pub field_name: Ident,
    pub peripherals: Vec<&'a MaybeArray<PeripheralInfo>>,
    pub chunks: MemoryChunks<MemoryThingFinal<'a>>,
}

impl<'a> PeripheralPage<'a> {
    fn new(
        device: &'a Device,
        peripherals: Vec<&'a MaybeArray<PeripheralInfo>>,
    ) -> Result<Self> {
        // peripherals can't be bigger then a page
        if matches!(peripherals
            .iter()
            .filter_map(|per| per.array())
            .map(|dim| dim.dim_increment)
            .max(), Some(len) if len != PAGE_LEN as u32)
        {
            bail!("Peripheral with invalid len");
        }

        // if one is dim, all need to be dim and equal
        let first = peripherals[0].array();
        if peripherals[1..].iter().any(|per| per.array() != first) {
            bail!("Invalid Peripheral dim");
        }

        // TODO other verifications...

        let raw_name = if peripherals.len() == 1 {
            dim_to_n(peripherals[0].name())
        } else {
            let page = (peripherals[0].base_address & PAGE_MASK) >> ADDR_BITS;
            match page {
                0x1000_0 => "ficr".to_owned(),
                0x1000_1..=0x1FFF_F => format!("uicr{}", page - 0x1000_1),
                0x4000_0..=0x4FFF_F => format!("apb{}", page - 0x4000_0),
                0x5000_0..=0x6FFF_F => format!("ahb{}", page - 0x5000_0),
                0xE000_0 => "itm".to_owned(),
                0xE000_1 => "dwt".to_owned(),
                0xE000_2 => "fpb".to_owned(),
                0xE000_E => "scs".to_owned(),
                0xE004_0 => "tpiu".to_owned(),
                0xE004_1 => "etm".to_owned(),
                0xE004_2..=0xE0FE_F => format!("eppb{}", page - 0xE004_2),
                _ => format!("unknown0x{page:x}"),
            }
        };
        let snake_case_name = snake_case(&raw_name);
        let camel_case_name = camel_case(&raw_name);
        let pseudo_struct = format_ident!("PeripheralPage{}", &camel_case_name);
        let mod_name = format_ident!("{}", &snake_case_name);
        let peripheral_struct = format_ident!("{}", &camel_case_name);
        let field_name = format_ident!("{}", &snake_case_name);

        let chunks = MemoryChunks::new_page(device, &raw_name, &peripherals)?;
        if chunks.len() > PAGE_LEN {
            bail!("Peripheral with size bigger then a page");
        }

        Ok(Self {
            pseudo_struct,
            mod_name,
            peripheral_struct,
            field_name,
            peripherals,
            chunks,
        })
    }

    pub fn page_addr(&self) -> u64 {
        // TODO the base address should not be allowed to no be page aligned
        self.peripherals[0].base_address & PAGE_MASK
    }

    fn gen_struct<'b>(&'b self) -> impl ToTokens + 'b {
        struct Tokens<'b>(&'b PeripheralPage<'b>);
        impl ToTokens for Tokens<'_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_struct_to_tokens(tokens)
            }
        }
        Tokens(self)
    }

    fn gen_struct_to_tokens(&self, tokens: &mut TokenStream) {
        use std::fmt::Write;
        let mod_name = &self.mod_name;
        let peripheral_struct = &self.peripheral_struct;
        let mut doc = String::new();
        for per in self.peripherals.iter() {
            if let Some(display_name) = &per.display_name {
                write!(doc, "{} - ", display_name).unwrap();
            }
            write!(doc, "{} - 0x{:08X}\n", &per.name, per.base_address)
                .unwrap();
            if let Some(description) = &per.description {
                write!(doc, "{}\n\n", &description).unwrap();
            }
        }
        let peripheral_fields = self.gen_fields_functions();
        tokens.extend(quote! {
            pub mod #mod_name {
                use icicle_vm::cpu::mem::MemResult;
                #[derive(Default)]
                #[doc = #doc]
                pub struct #peripheral_struct {
                    #[doc = "TODO: implement things here"]
                    _todo: (),
                }
                impl #peripheral_struct {
                    #peripheral_fields
                }
            }
        })
    }

    fn gen_struct_field<'b>(&'b self) -> impl ToTokens + 'b {
        struct Tokens<'b>(&'b PeripheralPage<'b>);
        impl ToTokens for Tokens<'_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_struct_field_to_tokens(tokens)
            }
        }
        Tokens(self)
    }

    fn gen_struct_field_to_tokens(&self, tokens: &mut TokenStream) {
        let field_name = &self.field_name;
        let peripheral_mod = &self.mod_name;
        let peripheral_struct = &self.peripheral_struct;
        if let Some(dim_num) = self.peripherals[0].array().map(|dim| dim.dim) {
            tokens.extend(quote! {
                #field_name: [#peripheral_struct; #dim_num],
            })
        } else {
            tokens.extend(quote! {
                #field_name: #peripheral_mod::#peripheral_struct,
            })
        }
    }

    fn gen_pages<'b>(&'b self) -> impl ToTokens + 'b {
        struct Tokens<'b>(&'b PeripheralPage<'b>);
        impl ToTokens for Tokens<'_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_pages_to_tokens(tokens)
            }
        }
        Tokens(self)
    }

    fn gen_pages_to_tokens(&self, tokens: &mut TokenStream) {
        let pseudo_struct = &self.pseudo_struct;
        let write = self.chunks.gen_match_chunks(false, 0);
        let read = self.chunks.gen_match_chunks(true, 0);
        let is_dim = self.peripherals[0].array().is_some();
        let instance_num = is_dim.then(|| {
            let first_page =
                Literal::u64_unsuffixed(self.peripherals[0].base_address);
            quote! { let _instance = (_addr >> #ADDR_BITS) - #first_page; }
        });
        tokens.extend(quote! {
            pub(crate) struct #pseudo_struct(
                pub std::sync::Arc<
                    std::sync::Mutex<super::peripheral::Peripherals>
                >,
            );
            impl icicle_vm::cpu::mem::IoMemory for #pseudo_struct {
                fn read(
                    &mut self,
                    _addr: u64,
                    _buf: &mut [u8],
                ) -> MemResult<()> {
                    #instance_num
                    let _start = _addr & #ADDR_MASK;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #read
                        _ => return Err(MemError::Unmapped),
                    }
                    #[allow(unreachable_code)]
                    Ok(())
                }
                fn write(
                    &mut self,
                    _addr: u64,
                    _buf: &[u8],
                ) -> MemResult<()> {
                    #instance_num
                    let _start = _addr & #ADDR_MASK;
                    let _end = _start + u64::try_from(_buf.len()).unwrap();
                    match (_start, _end) {
                        #write
                        _ => return Err(MemError::Unmapped),
                    }
                    #[allow(unreachable_code)]
                    Ok(())
                }
            }
        });
    }

    fn gen_register_fun(&self) -> TokenStream {
        self.chunks.gen_register_fun(self)
    }
    fn gen_fields_functions(&self) -> TokenStream {
        self.chunks.gen_fields_fun(self)
    }
}
