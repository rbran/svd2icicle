use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Write;
use std::num::NonZeroU32;
use std::ops::Range;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use svd_parser::svd::{self, Name};

use crate::enumeration::EnumerationValues;
use crate::formater::{camel_case, dim_to_n, snake_case};
use crate::helper::{str_to_doc, Dim, DisplayName};
use crate::memory::{ContextMemoryGen, MemoryChunks, MemoryThingFinal};
use crate::register::{ClusterAccess, RegisterAccess};
use crate::{ADDR_BITS, ADDR_MASK, PAGE_LEN, PAGE_MASK};

pub struct Device {
    pub peripherals: Vec<Peripheral>,
    pub field_values: Vec<EnumerationValues>,
}

#[derive(Debug, Clone, Copy)]
pub struct EnumerationValuesId(pub(crate) usize);

pub struct Peripheral {
    pub mod_name: Ident,
    pub name_struct: Ident,
    pub field_name: Ident,
    pub page_to_index_function: Ident,
    pub doc_names: Vec<String>,
    pub doc: String,
    pub chunks: MemoryChunks<MemoryThingFinal>,
    pub instances: Vec<PeripheralInstance>,
}

#[derive(Debug)]
pub struct PeripheralInstance {
    pub doc_names: Vec<String>,
    pub struct_name: Ident,
    pub page: u64,
    pub num: NonZeroU32,
}

impl Device {
    pub fn new(svds: &[svd::Device]) -> Self {
        // first we list the peripherals instances and merge then by page addr:
        let peripherals_iter =
            svds.iter().flat_map(|svd| svd.peripherals.iter());
        let og_peripherals_iter = peripherals_iter
            .clone()
            .filter(|per| per.derived_from.is_none());
        let derived_peripherals_iter = peripherals_iter
            .clone()
            .filter(|per| per.derived_from.is_some());

        // this is list of unique peripherals, this will be used to create the
        // final peripherals, and instances id will be referencing this.
        let mut peripherals = combine_peripherals(og_peripherals_iter);

        // list of instances that are derived from peripherals
        let derived_peripherals = combine_peripherals(derived_peripherals_iter);

        let mut peripheral_instances: Vec<_> =
            Vec::with_capacity(peripherals.len() + derived_peripherals.len());
        // instances that are not derived from other peripherals
        peripheral_instances.extend(peripherals.iter().enumerate().map(
            |(idx, (page, pers))| {
                let dim = <svd::MaybeArray<_> as Dim>::array(&pers[0]);
                let num = dim.map(|dim| dim.dim_increment).unwrap_or(1);
                if let Some(dim) = dim {
                    assert_eq!(dim.dim_increment as u64, PAGE_LEN);
                }
                (idx, PeripheralInstance::new(pers, *page, num))
            },
        ));

        // instances that are derived from other peripherals
        peripheral_instances.extend(derived_peripherals.into_iter().map(
            |(page, deriveds)| {
                let idx = find_peripheral_or_create_implementation(
                    svds,
                    &mut peripherals,
                    page,
                    &deriveds,
                );
                let dim = deriveds[0].array();
                let page = deriveds[0].base_address & PAGE_MASK;
                let num = dim.map(|dim| dim.dim_increment).unwrap_or(1);
                if let Some(dim) = dim {
                    assert_eq!(dim.dim_increment as u64, PAGE_LEN);
                }
                (idx, PeripheralInstance::new(&deriveds, page, num))
            },
        ));
        peripheral_instances.sort_unstable_by_key(|i| i.1.page);

        // TODO what are those????
        if let Some(((_i1, ins1), (_i2, ins2))) = peripheral_instances
            .iter()
            .enumerate()
            .zip(peripheral_instances.iter().enumerate())
            .filter(|((i1, _), (i2, _))| i1 != i2)
            .find(|((_, (_, ins1)), (_, (_, ins2)))| ins1.page == ins2.page)
        {
            todo!("Overlapping instances\n{ins1:#?}\n{ins2:#?}");
        }
        // TODO merge sequential instances

        let mut field_values = vec![];
        // create the list of unique peripherals
        let mut peripherals: Vec<_> = peripherals
            .into_iter()
            .map(|(page, peripherals)| {
                Peripheral::new_uniques(
                    svds,
                    page,
                    &peripherals,
                    &mut field_values,
                )
            })
            .collect();

        // populate instances
        for (idx, instance) in peripheral_instances.into_iter() {
            peripherals[idx].include_instance(instance)
        }

        // sort peripherals instances to make the code reproducible
        for peripheral in peripherals.iter_mut() {
            peripheral.instances.sort_unstable_by_key(|i| i.page);
        }

        Self {
            peripherals,
            field_values,
        }
    }

    pub fn get_enum(&self, enum_id: EnumerationValuesId) -> &EnumerationValues {
        &self.field_values[enum_id.0]
    }
}

impl Peripheral {
    fn new_uniques(
        devices: &[svd::Device],
        page: u64,
        pers: &[&svd::Peripheral],
        field_values: &mut Vec<EnumerationValues>,
    ) -> Self {
        let name = peripheral_name(pers, page).to_string();

        let doc = pers
            .iter()
            .map(|per| {
                let name = per.doc_name();
                let description = per
                    .description
                    .as_ref()
                    .map(String::as_str)
                    .map(str_to_doc)
                    .unwrap_or("No Documentation".to_string());
                format!("{name}: {description}<br>")
            })
            .chain(["<br>Instances:<br>".to_string()].into_iter())
            .collect();
        let doc_names = pers.iter().map(|per| per.doc_name()).collect();
        let mut context = ContextMemoryGen {
            svds: devices,
            pheriperals_name: &name,
            enumerated_values: field_values,
            clusters: vec![],
        };
        Self {
            mod_name: format_ident!("{}", snake_case(&name)),
            name_struct: format_ident!("{}", camel_case(&name)),
            field_name: format_ident!("{}", snake_case(&name)),
            page_to_index_function: format_ident!("page_to_index"),
            chunks: MemoryChunks::new_page(&mut context, pers),
            doc,
            doc_names,
            // populated later
            instances: vec![],
        }
    }

    fn include_instance(&mut self, instance: PeripheralInstance) {
        let page = instance.page;
        let names = instance.doc_names.join(", ");
        write!(self.doc, "0x{page:08x}: {names}<br>").unwrap();
        self.instances.push(instance);
    }
}

impl PeripheralInstance {
    fn new(pers: &[&svd::Peripheral], page: u64, num: u32) -> Self {
        let names = pers.iter().map(|per| per.doc_name()).collect();
        let struct_name =
            format_ident!("{}", camel_case(&peripheral_name(pers, page)));
        Self {
            doc_names: names,
            struct_name,
            page,
            num: NonZeroU32::new(num).unwrap(),
        }
    }
}

impl Device {
    pub fn gen_all(&self, tokens: &mut TokenStream) {
        let mut context = ContextCodeGen {
            // HACK allow empty peripheral? Make this a builder like pattern?
            peripheral: &self.peripherals[0],
            clusters: vec![],
            register: None,
            device: self,
        };
        self.gen_peripheral(&mut context, tokens);
        self.gen_pages(&mut context, tokens);
        self.gen_map_pages(tokens);
    }

    fn gen_peripheral<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        tokens: &mut TokenStream,
    ) {
        // structs for each peripherals
        let per_structs: TokenStream = self
            .peripherals
            .iter()
            .map(|x| x.gen_struct(context))
            .collect();
        // structs fields for each peripherals in the main Struct
        let per_structs_fields =
            self.peripherals.iter().map(Peripheral::gen_struct_fields);
        // register function that could be overwriten by the user
        let register_functions = self.peripherals.iter().map(|peripheral| {
            context.peripheral = peripheral;
            peripheral.chunks.gen_register_fun(context)
        });
        // all enums used by fields/registers
        let enums_declare =
            self.field_values.iter().map(EnumerationValues::gen_enum);
        // all the memory blocks
        tokens.extend(quote! {
            #[doc = "All peripheral related data is contained here"]
            pub mod peripheral {
                #per_structs

                #[doc = "Register read/write from pages, used for behavior affecting multiple peripherals"]
                pub(crate) mod registers {
                    use icicle_vm::cpu::mem::{MemResult, MemError};
                    impl super::Peripherals {
                        #(#register_functions)*
                    }
                }
                #[doc = "Values used for read/write by the peripheral fields"]
                pub mod enums {
                    #(#enums_declare)*
                }

                #[doc = "A Global Peripheral device"]
                #[derive(Default)]
                pub struct Peripherals {
                    #(#per_structs_fields)*
                }
            }
        });
    }

    fn gen_pages<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        tokens: &mut TokenStream,
    ) {
        // all the memory blocks
        let pages = self.peripherals.iter().map(|peripheral| {
            context.peripheral = peripheral;
            peripheral.gen_pages(context)
        });
        // gen the empty struct, and read/write functions
        tokens.extend(quote! {
            mod pages {
                use icicle_vm::cpu::mem::{MemError, MemResult};
                #(#pages)*
            }
        });
    }

    fn gen_map_pages<'a>(&'a self, tokens: &mut TokenStream) {
        // memory pages mapping to the cpu
        let memory_cpu_map = self.peripherals.iter().map(|per| {
            let pseudo_struct = &per.name_struct;
            let map_pages = per.instances.iter().map(|instance| {
                let addr_start = Literal::u64_unsuffixed(instance.page);
                quote! {
                    _cpu.mem.map_memory_len(#addr_start, #PAGE_LEN, io);
                }
            });
            quote! {
                let io = _cpu.mem.register_io_handler(
                    pages::#pseudo_struct(std::sync::Arc::clone(_pe))
                );
                #(#map_pages)*
            }
        });
        tokens.extend(quote! {
            #[doc = "Map Peripherals to Icicle VM"]
            pub fn map_cpu(
                _pe: &std::sync::Arc<std::sync::Mutex<peripheral::Peripherals>>,
                _cpu: &mut icicle_vm::cpu::Cpu,
            ) {
                #(#memory_cpu_map)*
            }
        })
    }
}

impl Peripheral {
    fn gen_page_to_index_function(&self) -> TokenStream {
        let function = &self.page_to_index_function;
        let page_instance =
            self.instances.iter().enumerate().map(|(index, inst)| {
                let page = inst.page >> ADDR_BITS;
                quote! { #page => #index, }
            });
        quote! {
            pub(crate) fn #function(page: u64) -> usize {
                match page {
                    #(#page_instance)*
                    _ => unreachable!(),
                }
            }
        }
    }

    fn gen_struct<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        context.peripheral = self;
        let mod_name = &self.mod_name;
        let peripheral_struct = &self.name_struct;
        let doc = &self.doc;
        let peripheral_fields = self.gen_fields_functions(context);
        let doc_mod = format!(
            "Peripheral{} {}",
            if self.doc_names.len() > 1 { "s" } else { "" },
            self.doc_names.join(", "),
        );
        let page_to_index_function = self.gen_page_to_index_function();
        quote! {
            #[doc = #doc_mod]
            pub mod #mod_name {
                use icicle_vm::cpu::mem::MemResult;
                #[derive(Default)]
                #[doc = #doc]
                pub struct #peripheral_struct {
                    #[doc = "TODO: implement things here"]
                    _todo: (),
                }
                impl #peripheral_struct {
                    #page_to_index_function
                    #peripheral_fields
                }
            }
        }
    }

    fn num_instances(&self) -> u32 {
        self.instances.iter().map(|ins| ins.num.get()).sum()
    }

    fn gen_struct_fields(&self) -> TokenStream {
        let peripheral_mod = &self.mod_name;
        let peripheral_struct = &self.name_struct;
        let num_instances = self.num_instances();
        let field_name = &self.field_name;
        if num_instances > 1 {
            let num_instances = Literal::u32_unsuffixed(num_instances);
            let inst_pages: Vec<_> = self
                .instances
                .iter()
                .map(|instance| format!("0x{:08x}", instance.page))
                .collect();
            let doc = format!("Peripheral at {}", inst_pages.join(", "));
            quote! {
                #[doc = #doc]
                pub #field_name: [#peripheral_mod::#peripheral_struct; #num_instances],
            }
        } else {
            let doc =
                format!("Peripheral at 0x{:08x}", &self.instances[0].page);
            quote! {
                #[doc = #doc]
                pub #field_name: #peripheral_mod::#peripheral_struct,
            }
        }
    }

    fn gen_pages<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        context.peripheral = self;
        let pseudo_struct = &self.name_struct;
        let mod_name = &self.mod_name;
        let og_struct = &self.name_struct;
        let page_to_index = &self.page_to_index_function;
        let chunks_to_match = |(range, mem): (Range<u64>, _)| {
            let start = (range.start != 0)
                .then_some(Literal::u64_unsuffixed(range.start));
            let end = Literal::u64_unsuffixed(range.end);
            let startp1 = Literal::u64_unsuffixed(range.start + 1);
            let endm1 = Literal::u64_unsuffixed(range.end - 1);
            quote! {
                (#start..=#endm1, #startp1..=#end) => {
                    #mem
                },
            }
        };
        let write = self.chunks.gen_chunks(context, false, 0, chunks_to_match);
        let read = self.chunks.gen_chunks(context, true, 0, chunks_to_match);
        quote! {
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
                    let _instance_page = crate::peripheral::#mod_name::#og_struct::#page_to_index(
                        _addr >> #ADDR_BITS
                    );
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
                    let _instance_page = crate::peripheral::#mod_name::#og_struct::#page_to_index(
                        _addr >> #ADDR_BITS
                    );
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
        }
    }

    fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        context.peripheral = self;
        self.chunks.gen_fields_fun(context)
    }
}

// combine peripherals that share the same page
fn combine_peripherals<'a>(
    peripherals_iter: impl Iterator<Item = &'a svd::Peripheral>,
) -> Vec<(u64, Vec<&'a svd::Peripheral>)> {
    let mut peripherals: HashMap<u64, Vec<_>> = HashMap::new();
    for per in peripherals_iter {
        let page = per.base_address & PAGE_MASK;
        peripherals.entry(page).or_insert(vec![]).push(per);
    }
    //TODO check that peripherals don't overlap because of the dim
    let mut peripherals: Vec<_> = peripherals.into_iter().collect();
    peripherals.sort_unstable_by_key(|x| x.0);
    peripherals
}

// generate a name for peripherals instances
// Peripherals don't have a single page, they could have multiple instances,
// each on it's own page
fn peripheral_name(
    peripherals: &[&svd::Peripheral],
    page: u64,
) -> Cow<'static, str> {
    if peripherals.len() == 1 {
        // Single peripheral, use it's name
        Cow::Owned(dim_to_n(&peripherals[0].name.to_lowercase()))
    } else {
        let page = page >> ADDR_BITS;
        match page {
            0x1000_0 => Cow::Borrowed("ficr"),
            0x1000_1..=0x1FFF_F => {
                Cow::Owned(format!("uicr{}", page - 0x1000_1))
            }
            0x4000_0..=0x4FFF_F => {
                Cow::Owned(format!("apb{}", page - 0x4000_0))
            }
            0x5000_0..=0x6FFF_F => {
                Cow::Owned(format!("ahb{}", page - 0x5000_0))
            }
            0xE000_0 => Cow::Borrowed("itm"),
            0xE000_1 => Cow::Borrowed("dwt"),
            0xE000_2 => Cow::Borrowed("fpb"),
            0xE000_E => Cow::Borrowed("scs"),
            0xE004_0 => Cow::Borrowed("tpiu"),
            0xE004_1 => Cow::Borrowed("etm"),
            0xE004_2..=0xE0FE_F => {
                Cow::Owned(format!("eppb{}", page - 0xE004_2))
            }
            _ => panic!("Peripherals at unknown page 0x{page:x}"),
        }
    }
}

/// find the peripheral block this derived block points to
fn find_peripheral_implementation(
    peripherals: &[(u64, Vec<&svd::Peripheral>)],
    deriveds: &[&svd::Peripheral],
) -> Option<usize> {
    if deriveds.len() == 1 {
        let per = &deriveds[0];
        assert!(per.registers.is_none());
        let name = per.derived_from.as_ref().unwrap().as_str();
        return peripherals.iter().position(|pers| {
            pers.1.iter().find(|per| &per.name == name).is_some()
        });
    }
    // all deriveds need to be contained in peripherals and vise-versa
    peripherals.iter().position(|pers| {
        if pers.1.len() != deriveds.len() {
            return false;
        }
        deriveds.iter().all(|derived| {
            let derived_name =
                derived.derived_from.as_ref().map(String::as_str).unwrap();
            pers.1.iter().any(|per| per.name() == derived_name)
        })
    })
}

/// find the peripheral block this derived block points to, or create a new
/// peripheral implementation, if this block combination is unique
fn find_peripheral_or_create_implementation<'a>(
    svds: &'a [svd::Device],
    peripherals: &mut Vec<(u64, Vec<&'a svd::Peripheral>)>,
    page: u64,
    deriveds: &[&svd::Peripheral],
) -> usize {
    find_peripheral_implementation(peripherals, deriveds).unwrap_or_else(|| {
        let new_per = deriveds
            .iter()
            .map(|der| {
                let name = der.derived_from.as_ref().unwrap().as_str();
                svds.iter()
                    .flat_map(|svd| svd.peripherals.iter())
                    .find(|per| per.name() == name)
                    .expect("Unable to find derived_from peripheral")
            })
            .collect();
        let idx = peripherals.len();
        peripherals.push((page, new_per));
        idx
    })
}

pub(crate) struct ContextCodeGen<'a> {
    pub device: &'a Device,
    pub peripheral: &'a Peripheral,
    pub clusters: Vec<&'a ClusterAccess>,
    pub register: Option<&'a RegisterAccess>,
}

impl ContextCodeGen<'_> {
    pub fn doc_peripheral(&self) -> String {
        let per_mod = &self.peripheral.mod_name;
        let per_struct = &self.peripheral.name_struct;
        format!("[crate::peripheral::{per_mod}::{per_struct}]")
    }
}
