use anyhow::{anyhow, bail, Result};
use indexmap::IndexMap;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::{Name, Peripheral, Register};

use crate::field::{FieldAccess, FieldData};
use crate::formater::{self, dim_to_n, snake_case};
use crate::helper;

#[derive(Debug)]
pub struct RegisterAccess<'a> {
    pub read_fun: Option<Ident>,
    pub write_fun: Option<Ident>,
    pub dim: u32,
    pub bytes: u32,
    pub regs: Vec<&'a Register>,
    pub fields: Vec<FieldAccess<'a>>,
}

impl<'a> RegisterAccess<'a> {
    pub fn new(regs: Vec<(&'a Peripheral, &'a Register)>) -> Result<Self> {
        // all register names, used for error messages
        let regs_names = || {
            regs.iter()
                .map(|(per, reg)| format!("{}::{}", per.name(), reg.name()))
                .collect::<Vec<_>>()
                .join(", ")
        };

        // combine the names of all peripherals
        let per_name: String = {
            let mut pers = std::collections::HashSet::new();
            regs.iter()
                .filter_map(move |(per, _reg)| {
                    pers.insert(
                        (*per as *const svd_parser::svd::MaybeArray<_>)
                            as usize,
                    )
                    .then_some(formater::snake_case(per.name.as_str()))
                })
                .collect()
        };
        //TODO how to handle regs with multiple names?
        let name = snake_case(&dim_to_n(&regs[0].1.name));

        // registers can have diferent dims, use the biggest one
        let dim = regs
            .iter()
            .map(|(_per, reg)| match reg {
                svd_parser::svd::MaybeArray::Single(_reg) => 1,
                svd_parser::svd::MaybeArray::Array(_reg, dim) => dim.dim,
            })
            .max()
            .unwrap();

        // all register need to have the same access permissions
        let mut access_iter = regs.iter().map(|(per, reg)| {
            reg.properties
                .access
                .or(per.default_register_properties.access)
        });
        let access = access_iter.next().unwrap().ok_or_else(|| {
            anyhow!(
                "registers {}::{} without permissions",
                regs[0].0.name(),
                regs[0].1.name(),
            )
        })?;
        if access_iter.any(|other_access| other_access != Some(access)) {
            bail!(
                "overlapping registers {} with diferent permissions",
                regs_names(),
            )
        }
        let read_fun = access
            .can_read()
            .then(|| format_ident!("read_{}_{}", &per_name, name));
        let write_fun = access
            .can_write()
            .then(|| format_ident!("write_{}_{}", &per_name, name));

        let mut regs_sizes = regs.iter().map(|(per, reg)| {
            reg.properties.size.or(per.default_register_properties.size)
        });
        let bits = regs_sizes.next().unwrap().ok_or_else(|| {
            anyhow!(
                "registers {}::{} without size",
                regs[0].0.name(),
                regs[0].1.name(),
            )
        })?;
        if regs_sizes.any(|o_bits| o_bits != Some(bits)) {
            bail!("overlapping register {} with diferent sizes", regs_names())
        }
        if bits % 8 != 0 {
            bail!(
                "overlapping register {} with invalid size {}bits",
                regs_names(),
                bits,
            )
        }
        let bytes = bits / 8;

        let mut fields: IndexMap<u32, Vec<_>> = IndexMap::new();
        for (per, reg, field) in regs.iter().flat_map(|(per, reg)| {
            reg.fields().map(move |field| (*per, *reg, field))
        }) {
            fields
                .entry(field.bit_offset())
                .and_modify(|regs| regs.push((per, reg, field)))
                .or_insert_with(|| vec![(per, reg, field)]);
        }

        // TODO check if the fields overlap or goes outside the register
        let fields = fields
            .into_iter()
            .map(|(_offset, fields)| {
                // TODO compare field access with register access
                FieldAccess::new(fields, dim, &per_name, &name, access)
            })
            .collect::<Result<_, _>>()?;

        let regs = regs.into_iter().map(|(_per, reg)| reg).collect();

        Ok(Self {
            dim,
            bytes,
            read_fun,
            write_fun,
            regs,
            fields,
        })
    }

    pub fn mem_map_functions<'b>(&'b self) -> impl ToTokens + 'b {
        struct Tokens<'a, 'b: 'a>(&'b RegisterAccess<'a>);
        impl ToTokens for Tokens<'_, '_> {
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                self.0.gen_mem_map_functions(tokens)
            }
        }
        Tokens(self)
    }

    fn gen_mem_map_functions(&self, tokens: &mut proc_macro2::TokenStream) {
        let dim_declare = (self.dim > 1).then(|| quote! {_dim: usize,});
        let dim_use = (self.dim > 1).then(|| quote! {_dim,});
        if self.bytes == 1 {
            if let Some(read) = self.read_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let lsb = field.fields[0].2.lsb();
                    Some(quote! {
                        self.0.lock().unwrap().#field_fun(#dim_use)? << #lsb;
                    })
                });
                tokens.extend(quote! {
                    pub fn #read(
                        &self,
                        #dim_declare
                    ) -> icicle_vm::cpu::mem::MemResult<u8> {
                        let mut _value = 0;
                        #(_value |= #fields)*
                        Ok(value)
                    }
                })
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let lsb = field.fields[0].2.lsb();
                    let mask =
                        u8::MAX >> (u8::BITS - field.fields[0].2.bit_width());
                    let dim = (self.dim > 1).then(|| quote! {_dim}).into_iter();
                    Some(quote! {
                        self.0.lock().unwrap().#field_fun(
                            #(#dim,)*
                            (_value >> #lsb) #mask
                        )?;
                    })
                });
                tokens.extend(quote! {
                    pub fn #write(
                        &self,
                        #dim_declare
                        _value: u8
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                })
            }
        } else {
            let params: Box<[_]> = (0..self.bytes)
                .into_iter()
                .map(|i| format_ident!("_byte_{}", i))
                .collect();
            if let Some(read) = self.read_fun.as_ref() {
                let declare_params = params.iter().map(|param| {
                    quote! { mut #param: Option<&mut u8> }
                });
                let fields = self.fields.iter().filter_map(|field| {
                    self.gen_field_register(
                        &params,
                        field.read.as_ref()?,
                        field,
                        |byte_match, field_fun, lsb| quote! {
                            if let Some(byte) = &mut #byte_match {
                                **byte |= self
                                    .0
                                    .lock()
                                    .unwrap()
                                    .#field_fun(#dim_use)? << #lsb;
                            }
                        },
                        |field_fun, params| quote! {
                            if #(#params.is_some())||* {
                                self
                                    .0
                                    .lock()
                                    .unwrap()
                                    .#field_fun(#dim_use #(&mut #params),*)?;
                            }
                        }
                    )
                });
                tokens.extend(quote! {
                    pub fn #read(
                        &self,
                        #dim_declare #(#declare_params),*
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                });
            }
            if let Some(write) = self.write_fun.as_ref() {
                let declare_params = params.iter().map(|param| {
                    quote! { #param: Option<&u8> }
                });
                let fields = self.fields.iter().filter_map(|field| {
                    self.gen_field_register(
                        &params,
                        field.write.as_ref()?,
                        field,
                        |byte_match, field_fun, lsb| {
                            let mask = Literal::u8_unsuffixed(
                                u8::MAX
                                    >> (u8::BITS
                                        - field.fields[0].2.bit_width()),
                            );
                            quote! {
                                if let Some(byte) = #byte_match {
                                    self
                                        .0
                                        .lock()
                                        .unwrap()
                                        .#field_fun(
                                            #dim_use (*byte >> #lsb) & #mask
                                        )?;
                                }
                            }
                        },
                        |field_fun, params| {
                            quote! {
                                if #(#params.is_some())||* {
                                    self
                                        .0
                                        .lock()
                                        .unwrap()
                                        .#field_fun(#dim_use #(#params,)*)?;
                                }
                            }
                        },
                    )
                });
                tokens.extend(quote! {
                    pub fn #write(
                        &self,
                        #dim_declare #(#declare_params),*
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                });
            }
        }
    }

    fn gen_field_register<S, M>(
        &self,
        params: &[Ident],
        field_fun: &Ident,
        field: &FieldAccess,
        mut caller_single: S,
        mut caller_multiple: M,
    ) -> Option<TokenStream>
    where
        S: FnMut(&Ident, &Ident, Literal) -> TokenStream,
        M: FnMut(&Ident, &[Ident]) -> TokenStream,
    {
        let lsb = field.fields[0].2.lsb();
        let first_byte = (lsb / 8) as usize;
        match field.data {
            FieldData::Single(_bits) => {
                let byte_match = &params[first_byte];
                let lsb = Literal::u32_unsuffixed(lsb);
                Some(caller_single(byte_match, field_fun, lsb))
            }
            FieldData::Multiple { first, bytes, last } => {
                let last_byte = first_byte
                    + (first != 0) as usize
                    + (last != 0) as usize
                    + bytes as usize;
                let params = &params[first_byte..last_byte];
                Some(caller_multiple(field_fun, params))
            }
        }
    }

    pub fn gen_mem_page_function_call<I>(
        &self,
        read: bool,
        dim: Option<Literal>,
        params: I,
    ) -> Option<TokenStream>
    where
        I: Iterator<Item = TokenStream>,
    {
        let fun = if read {
            self.read_fun.as_ref()?
        } else {
            self.write_fun.as_ref()?
        };
        let dim = dim.into_iter();
        Some(quote! { self.#fun(#(#dim,)* #(#params),*)?; })
    }

    fn gen_fields_functions(&self, tokens: &mut TokenStream) {
        // if no fields, means it is a single implicit field
        if self.fields.is_empty() {
            helper::read_write_field(
                self.dim,
                self.read_fun.as_ref(),
                self.write_fun.as_ref(),
                self.bytes,
                tokens,
            )
        } else {
            self.fields.iter().for_each(|x| x.to_tokens(tokens))
        }
    }

    pub fn fields_functions<'b>(&'b self) -> impl ToTokens + 'b {
        struct Token<'b>(&'b RegisterAccess<'b>);
        impl ToTokens for Token<'_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_fields_functions(tokens)
            }
        }
        Token(self)
    }
}
