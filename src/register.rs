use std::collections::HashMap;

use anyhow::{bail, Result};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::{
    ClusterInfo, DimElement, MaybeArray, Name, RegisterInfo, RegisterProperties,
};

use crate::field::{FieldAccess, FieldData};
use crate::formater::dim_to_n;
use crate::helper::{self, Dim};
use crate::memory::{
    Context, Memory, MemoryChunks, MemoryThingCondensated, MemoryThingFinal,
};
use crate::peripheral::PeripheralPage;

pub struct ClusterAccess<'a> {
    pub bytes: u64,
    pub clusters: Vec<&'a MaybeArray<ClusterInfo>>,
    pub memory: MemoryChunks<MemoryThingFinal<'a>>,
}

impl<'a> ClusterAccess<'a> {
    pub(crate) fn new<'b>(
        context: &mut Context<'b>,
        clusters: Vec<&'a MaybeArray<ClusterInfo>>,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    ) -> Result<Self> {
        // generate the names
        let mut bytes = memory.len();
        if let Some(dim) = clusters[0].array() {
            bytes = dim.dim as u64 * dim.dim_increment as u64;
        }
        // combine the clusters names, if they overlap
        let cluster_name = clusters
            .iter()
            .map(|cluster| dim_to_n(&cluster.name().to_lowercase()))
            .collect();
        context.clusters.push(cluster_name);
        let memory = memory.finalize(context)?;
        context.clusters.pop();
        Ok(Self {
            clusters,
            bytes,
            memory,
        })
    }

    pub(crate) fn gen_register_functions(
        &self,
        peripheral: &PeripheralPage,
    ) -> TokenStream {
        self.memory.gen_register_fun(peripheral)
    }

    pub(crate) fn gen_fields_functions(
        &self,
        peripheral: &PeripheralPage,
    ) -> TokenStream {
        self.memory.gen_fields_functions(peripheral)
    }
}

#[derive(Debug)]
pub struct RegisterAccess<'a> {
    pub read_fun: Option<Ident>,
    pub write_fun: Option<Ident>,
    pub registers: Vec<&'a MaybeArray<RegisterInfo>>,
    pub fields: Vec<FieldAccess<'a>>,
    pub properties: RegisterProperties,
    /// reset value, except for the fields (0s in there)
    pub clean_value: u64,
}

impl<'a> RegisterAccess<'a> {
    pub(crate) fn new(
        context: &Context,
        properties: RegisterProperties,
        registers: Vec<&'a MaybeArray<RegisterInfo>>,
    ) -> Result<Self> {
        // all register names, used for error messages
        let regs_names = || {
            format!(
                "{} registers {}",
                context.gen_register_fun_name(""),
                registers
                    .iter()
                    .map(|reg| reg.name())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        // generate the names
        let reg_name = dim_to_n(&registers[0].name().to_owned()).to_lowercase();
        // NOTE address offset is used, because diferent registers, that dont
        // overlap, from diferent pheripherals, that overlap, have the same name
        let reg_name = format!("{reg_name}{:x}", registers[0].address_offset);
        let name = context.gen_register_fun_name(&reg_name);
        let read_fun = properties
            .access
            .unwrap()
            .can_read()
            .then(|| format_ident!("{}_read", name));
        let write_fun = properties
            .access
            .unwrap()
            .can_write()
            .then(|| format_ident!("{}_write", name));

        // all register need to have the same reset_value
        let mut reset_value_mask_iter = registers.iter().map(|reg| {
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

        let mut fields: HashMap<u32, Vec<_>> = HashMap::new();
        for field in registers.iter().flat_map(|reg| reg.fields()) {
            fields
                .entry(field.bit_offset())
                .or_insert_with(|| vec![])
                .push(field);
        }
        let mut fields: Vec<_> = fields
            .into_values()
            .map(|fields| {
                // TODO compare field access with register access
                FieldAccess::new(
                    context,
                    &properties,
                    fields,
                    registers[0].is_array(),
                    &reg_name,
                    reset_value,
                    reset_mask,
                )
            })
            .collect::<Result<_, _>>()?;
        fields.sort_unstable_by_key(|field| field.fields[0].bit_offset());

        // TODO check for overlapping fields

        // remove the fields from the reset value
        let clean_value =
            fields
                .iter()
                .fold(reset_value & reset_mask, |clean, field| {
                    let bits =
                        u64::MAX >> (u64::BITS - field.fields[0].bit_width());
                    let mask = bits << field.fields[0].lsb();
                    clean & !mask
                });

        Ok(Self {
            read_fun,
            write_fun,
            registers,
            fields,
            clean_value,
            properties,
        })
    }

    pub fn implicit_field(&self) -> bool {
        self.fields.is_empty()
    }

    pub(crate) fn gen_fields_functions(
        &self,
        _peripheral: &PeripheralPage,
        tokens: &mut TokenStream,
    ) {
        if !self.implicit_field() {
            self.fields.iter().for_each(|x| x.to_tokens(tokens))
        } else {
            // implicit field, generate the pseudo field for this register
            let docs: Vec<_> = self
                .registers
                .iter()
                .filter_map(|reg| {
                    Some(format!(
                        "{}: {}\n",
                        reg.name(),
                        reg.description.as_ref()?,
                    ))
                })
                .collect();
            let docs = docs.join("\n\n");
            let name: Vec<_> = self
                .registers
                .iter()
                .map(|reg| format!("{}", reg.name()))
                .collect();
            let name = name.join(", ");
            helper::read_write_field(
                self.array().is_some(),
                &name,
                self.read_fun.as_ref(),
                self.write_fun.as_ref(),
                FieldData::from_bytes(self.properties.size.unwrap() / 8),
                self.properties.reset_value.unwrap_or(0),
                self.properties.reset_mask.unwrap_or(0),
                &docs,
                tokens,
            )
        }
    }

    pub(crate) fn gen_register_function(
        &self,
        peripheral: &PeripheralPage,
        tokens: &mut TokenStream,
    ) {
        let peripheral_field = &peripheral.field_name;
        let dim_declare = self.array().map(|_| quote! {_dim: usize,});
        let dim_use = self.array().map(|_| quote! {_dim,});
        let clean_value = Literal::u64_unsuffixed(self.clean_value);
        let bytes = self.properties.size.unwrap() / 8;
        let value_type = helper::DataType::from_bytes(bytes);
        if self.implicit_field() {
            self.gen_register_function_implicit_field(peripheral, tokens);
            return;
        }
        if bytes == 1 {
            if let Some(read) = self.read_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let lsb = field.fields[0].lsb();
                    let rotate = (lsb > 0).then(|| quote! { << #lsb});
                    Some(quote! {
                        self.#peripheral_field.#field_fun(#dim_use)? #rotate;
                    })
                });
                tokens.extend(quote! {
                    pub fn #read(
                        &mut self,
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
                    let lsb = field.fields[0].lsb();
                    let rotate = (lsb > 0).then(|| quote! { >> #lsb});
                    let mask =
                        u8::MAX >> (u8::BITS - field.fields[0].bit_width());
                    let dim = self.array().map(|_| quote! {_dim}).into_iter();
                    Some(quote! {
                        self.#peripheral_field.#field_fun(
                            #(#dim,)*
                            (_value #rotate) & #mask
                        )?;
                    })
                });
                tokens.extend(quote! {
                    pub fn #write(
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
                    let lsb = field.fields[0].lsb();
                    let rotate = (lsb > 0).then(|| quote! { << #lsb});
                    quote! {
                        _value |= #value_type::from(
                            self.#peripheral_field.#read(#dim_use)?
                        ) #rotate;
                    }
                });
                let clean_value = Literal::u64_unsuffixed(self.clean_value);
                tokens.extend(quote! {
                    // NOTE no comma on dim_declare
                    pub fn #read(&mut self, #dim_declare) -> MemResult<#value_type> {
                        let mut _value = #clean_value;
                        #(#fields)*
                        Ok(_value)
                    }
                });
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().map(|field| {
                    let field_start = field.fields[0].lsb() / 8;
                    let field_bytes = field.data.bytes();
                    let field_lsb = Literal::u32_unsuffixed(field.fields[0].lsb() % 8);
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
                                    let _i = (#field_start - _start) as usize;
                                    self.#peripheral_field.#write(
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
                                    self.#peripheral_field.#write(#dim_use _value)?;
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
                                    self.#peripheral_field.#write(#dim_use _extracted)?;
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
                    pub fn #write(
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

    fn gen_register_function_implicit_field(
        &self,
        peripheral: &PeripheralPage,
        tokens: &mut TokenStream,
    ) {
        let dim_use = self.array().map(|_| quote! {_dim});
        let peripheral_field = &peripheral.field_name;
        let dim_declare = self.array().map(|_| quote! {_dim: usize,});
        let bytes = self.properties.size.unwrap() / 8;
        let bytes_lit = Literal::u32_unsuffixed(bytes);
        let value_type = helper::DataType::from_bytes(bytes);
        if bytes == 1 {
            todo!("one byte register");
        }
        if let Some(read) = &self.read_fun {
            tokens.extend(quote! {
                pub fn #read(
                    &mut self,
                    #dim_declare
                ) -> MemResult<#value_type> {
                    self.#peripheral_field.#read(#dim_use)
                }
            })
        }
        if let Some(write) = &self.write_fun {
            let dim_use = dim_use.into_iter();
            // TODO implement byte endian here
            tokens.extend(quote! {
                pub fn #write(
                    &mut self,
                    #dim_declare
                    _start: u64,
                    _value: &[u8],
                ) -> MemResult<()> {
                    if _start != 0 || _value.len() != #bytes_lit {
                        return Err(MemError::WriteViolation);
                    }
                    self.#peripheral_field.#write(
                        #(#dim_use,)*
                        #value_type::from_ne_bytes(_value.try_into().unwrap()),
                    )
                }
            })
        }
    }
}

impl Dim for RegisterAccess<'_> {
    fn array(&self) -> Option<&DimElement> {
        self.registers[0].array()
    }
}
