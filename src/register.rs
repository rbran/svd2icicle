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
    pub regs: Vec<(&'a Peripheral, &'a Register)>,
    pub fields: Vec<FieldAccess<'a>>,
    /// reset value, except for the fields (0s there)
    pub clean_value: u64,
    pub reset_value: u64,
    pub reset_mask: u64,
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

        // all register need to have the same reset_value
        let mut reset_value_mask_iter = regs.iter().map(|(_per, reg)| {
            let value = reg.properties.reset_value.unwrap_or(0);
            let mask = reg.properties.reset_mask.unwrap_or(u64::MAX);
            (value, mask)
        });
        let (reset_value, reset_mask) = reset_value_mask_iter.next().unwrap();
        if reset_value_mask_iter
            .any(|other_reset| other_reset != (reset_value, reset_mask))
        {
            bail!(
                "overlapping registers {} with diferent reset value/mask",
                regs_names(),
            )
        }
        // remove the fields from the reset value
        let clean_value = fields.iter().fold(
            reset_value & reset_mask,
            |clean, (_offset, field)| {
                let bits = u64::MAX >> (u64::BITS - field[0].2.bit_width());
                let mask = bits << field[0].2.lsb();
                clean & !mask
            },
        );

        // TODO check if the fields overlap or goes outside the register
        let fields = fields
            .into_iter()
            .map(|(_offset, fields)| {
                // TODO compare field access with register access
                FieldAccess::new(
                    fields,
                    dim,
                    &per_name,
                    &name,
                    access,
                    reset_value,
                    reset_mask,
                )
            })
            .collect::<Result<_, _>>()?;

        Ok(Self {
            dim,
            bytes,
            read_fun,
            write_fun,
            regs,
            fields,
            clean_value,
            reset_value,
            reset_mask,
        })
    }

    /// true if is compose of a single field with the same len then the register
    pub fn is_single_field(&self) -> bool {
        // if no fields, means it is a single implicit field
        if self.fields.is_empty() {
            return true;
        }

        // only one field and it have the same size then the register
        if self.fields.len() == 1 {
            match self.fields[0].data {
                FieldData::Single(8) if self.bytes == 1 => true,
                FieldData::Multiple {
                    first: 0,
                    bytes,
                    last: 0,
                } if self.bytes == bytes => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn mem_map_functions(&self) -> impl ToTokens + '_ {
        struct Tokens<'a, 'b: 'a>(&'b RegisterAccess<'a>);
        impl ToTokens for Tokens<'_, '_> {
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                self.0.gen_mem_map_functions(tokens)
            }
        }
        Tokens(self)
    }

    fn gen_mem_map_functions(&self, tokens: &mut proc_macro2::TokenStream) {
        // if is single fields, there is no need to create a function
        // in pages, we can call the function from `Peripherals` directly
        if self.is_single_field() {
            return;
        }
        let dim_declare = (self.dim > 1).then(|| quote! {_dim: usize,});
        let dim_use = (self.dim > 1).then(|| quote! {_dim,});
        let clean_value = Literal::u64_unsuffixed(self.clean_value);
        let value_type = helper::DataType::from_bytes(self.bytes);
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
                    fn #read(
                        &self,
                        #dim_declare
                    ) -> MemResult<#value_type> {
                        let mut _value = #clean_value;
                        #(_value |= #fields)*
                        Ok(value)
                    }
                })
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.write.as_ref()?;
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
                    fn #write(
                        &mut self,
                        #dim_declare
                        _value: #value_type,
                    ) -> MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                })
            }
        } else {
            if let Some(read) = self.read_fun.as_ref() {
                let fields = self.fields.iter().map(|field| {
                    let read = field.read.as_ref().unwrap();
                    let lsb = field.fields[0].2.lsb();
                    quote! {
                        _value |= #value_type::from(
                            self.0.lock().unwrap().#read(#dim_use)?
                        ) << #lsb;
                    }
                });
                let clean_value = Literal::u64_unsuffixed(self.clean_value);
                tokens.extend(quote! {
                    // NOTE no comma on dim_declare
                    fn #read(&self, #dim_declare) -> MemResult<#value_type> {
                        let mut _value = #clean_value;
                        #(#fields)*
                        Ok(_value)
                    }
                });
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().map(|field| {
                    let field_start = field.fields[0].2.lsb() / 8;
                    let field_bytes = field.data.bytes();
                    let field_lsb = Literal::u32_unsuffixed(field.fields[0].2.lsb() % 8);
                    let field_end = Literal::u32_unsuffixed(field_start + field_bytes);
                    let field_start = Literal::u32_unsuffixed(field_start);

                    let write = field.write.as_ref().unwrap();

                    match field.data {
                        FieldData::Single(bits) => {
                            let mask = if bits == 1 {
                                quote!{ 1 != 0 }
                            } else {
                                (u8::MAX >> (u8::BITS - bits)).to_token_stream()
                            };
                            quote! {
                                // TODO implement byte endian here
                                if (_start.._end).contains(&#field_start) {
                                    let _i = (_start - #field_start) as usize;
                                    self.0.lock().unwrap().#write(
                                        #dim_use
                                        (_value[_i] >> #field_lsb) & #mask,
                                    )?;
                                }
                            }
                        }
                        FieldData::Multiple { first: 0, bytes, last: 0 } => {
                            let value_type = helper::DataType::from_bytes(bytes);
                            let bytes = Literal::u32_unsuffixed(bytes);
                            // TODO implement byte endian here
                            quote! {
                                if _start <= #field_start && _end >= #field_end {
                                    let _offset_start = (#field_start - _start) as usize;
                                    let _offset_end = _offset_start + #bytes;
                                    let _value = #value_type::from_ne_bytes(
                                        _value[_offset_start.._offset_end]
                                        .try_into().unwrap()
                                    );
                                    self.0.lock().unwrap().#write(#dim_use _value)?;
                                } else if (_start > #field_start && _start < #field_end)
                                    || (_end > #field_start && _end < #field_end) {
                                    return Err(MemError::WriteViolation);
                                }
                            }
                        }
                        FieldData::Multiple { first, bytes, last } => {
                            let value_type = helper::DataType::from_bytes(field_bytes);
                            let first_byte = (first > 0).then(|| {
                                let first_mask = u8::MAX >> (u8::BITS - first);
                                quote!{
                                    _extracted |= ((_value[(#field_start - _start) as usize] >> #field_lsb)
                                        & #first_mask) as #value_type;
                                }
                            });
                            let middle_bytes = (bytes > 0).then(|| {
                                let bytes = Literal::u32_unsuffixed(bytes);
                                quote!{
                                    for byte_i in 0..#bytes {
                                        _extracted |= (_value[
                                            ((#field_start + byte_i + 1) - _start) as usize
                                        ] as #value_type) << (#field_lsb + (byte_i * 8));
                                    }
                                }
                            });
                            let last_byte = (last > 0).then(|| {
                                let last_mask = u8::MAX >> (u8::BITS - last);
                                let bytes = Literal::u32_unsuffixed(bytes);
                                quote! {
                                    _extracted |= ((_value[
                                        ((#field_start + #bytes + 1) - _start) as usize
                                    ] & #last_mask) as #value_type) << (#field_lsb + (#bytes * 8));
                                }
                            });
                            // TODO implement byte endian here
                            quote! {
                                if _start <= #field_start && _end >= #field_end {
                                    let mut _extracted: #value_type = 0;
                                    #first_byte
                                    #middle_bytes
                                    #last_byte
                                    self.0.lock().unwrap().#write(#dim_use _extracted)?;
                                } else if (_start > #field_start && _start < #field_end)
                                    || (_end > #field_start && _end < #field_end) {
                                    return Err(MemError::WriteViolation);
                                }
                            }
                        }
                    }

                });
                // TODO implement byte endian here
                tokens.extend(quote! {
                    // NOTE no comma on dim_declare
                    fn #write(
                        &mut self,
                        #dim_declare
                        _start: u64,
                        _value: &[u8],
                    ) -> MemResult<()> {
                        debug_assert!(!_value.is_empty());
                        let _end = _start + _value.len() as u64;
                        #(#fields)*
                        Ok(())
                    }
                });
            }
        }
    }

    fn gen_fields_functions(&self, tokens: &mut TokenStream) {
        if self.is_single_field() {
            let docs: Vec<_> = self
                .regs
                .iter()
                .filter_map(|(per, reg)| {
                    Some(format!(
                        "{} {}: {}",
                        per.name(),
                        reg.name(),
                        reg.description.as_ref()?,
                    ))
                })
                .collect();
            let docs = docs.join("\n\n");
            let name: Vec<_> = self
                .regs
                .iter()
                .map(|(per, reg)| format!("{} {}", per.name(), reg.name()))
                .collect();
            let name = name.join(", ");
            helper::read_write_field(
                self.dim,
                &name,
                self.read_fun.as_ref(),
                self.write_fun.as_ref(),
                FieldData::from_bytes(self.bytes),
                self.reset_value,
                self.reset_mask,
                &docs,
                tokens,
            )
        } else {
            self.fields.iter().for_each(|x| x.to_tokens(tokens))
        }
    }

    pub fn fields_functions(&self) -> impl ToTokens + '_ {
        struct Token<'b>(&'b RegisterAccess<'b>);
        impl ToTokens for Token<'_> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.gen_fields_functions(tokens)
            }
        }
        Token(self)
    }
}
