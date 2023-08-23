use std::collections::HashMap;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::{
    self, ClusterInfo, DimElement, MaybeArray, ModifiedWriteValues, ReadAction,
    RegisterProperties, WriteConstraint,
};

use crate::enumeration::FieldRWType;
use crate::field::{FieldAccess, FieldData};
use crate::formater::{dim_to_n, snake_case};
use crate::helper::{self, str_to_doc, Dim, DisplayName, DataType};
use crate::memory::{
    ContextMemoryGen, Memory, MemoryChunks, MemoryThingCondensated,
    MemoryThingFinal,
};
use crate::peripheral::ContextCodeGen;

#[derive(Debug)]
pub struct ClusterAccess {
    pub bytes: u64,
    //pub clusters: Vec<&'a MaybeArray<ClusterInfo>>,
    pub address_offset: u32,
    pub dim: Option<(Ident, DimElement)>,
    pub memory: MemoryChunks<MemoryThingFinal>,
}

impl ClusterAccess {
    pub(crate) fn new<'a>(
        context: &mut ContextMemoryGen<'a, '_>,
        clusters: Vec<&'a MaybeArray<ClusterInfo>>,
        memory: MemoryChunks<MemoryThingCondensated<'a>>,
    ) -> Self {
        let mut bytes = memory.len();
        if let Some(dim) = clusters[0].array() {
            bytes = dim.dim as u64 * dim.dim_increment as u64;
        }

        context.clusters.push(clusters);
        let memory = memory.finalize(context);
        let clusters = context.clusters.pop().unwrap();

        let dim = clusters[0].array().cloned().map(|dim| {
            let name = snake_case(&dim_to_n(&clusters[0].name).to_lowercase());
            let ident = format_ident!("_{name}");
            (ident, dim)
        });
        Self {
            address_offset: clusters[0].address_offset,
            dim,
            bytes,
            memory,
        }
    }

    pub(crate) fn gen_register_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        let stream = self.memory.gen_register_fun(context);
        stream
    }

    pub(crate) fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
    ) -> TokenStream {
        let stream = self.memory.gen_fields_functions(context);
        stream
    }
}

#[derive(Debug)]
pub struct RegisterAccess {
    pub read_fun: Option<Ident>,
    pub write_fun: Option<Ident>,
    pub name: String,
    pub doc: String,
    pub fields: Vec<FieldAccess>,
    pub properties: RegisterProperties,
    pub address_offset: u32,
    pub array: Option<DimElement>,
    /// reset value, except for the fields (0s in there)
    pub clean_value: u64,
    pub modified_write_values: Option<ModifiedWriteValues>,
    pub write_constraint: Option<WriteConstraint>,
    pub read_action: Option<ReadAction>,
}

impl RegisterAccess {
    pub(crate) fn new<'a>(
        context: &mut ContextMemoryGen<'a, '_>,
        properties: RegisterProperties,
        registers: Vec<&'a svd::Register>,
    ) -> Self {
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
            panic!(
                "overlapping registers {} with diferent reset value/mask",
                regs_names(),
            )
        }

        // remove the fields from the reset value
        let clean_value = registers.iter().flat_map(|reg| reg.fields()).fold(
            reset_value & reset_mask,
            |clean, field| {
                let mut bit_width = field.bit_width();
                if let Some(array) = field.array() {
                    assert_eq!(array.dim_increment, bit_width);
                    bit_width *= array.dim;
                }
                let bits = u64::MAX >> (u64::BITS - bit_width);
                let mask = bits << field.lsb();
                clean & !mask
            },
        );

        let mut fields: HashMap<u32, Vec<_>> = HashMap::new();
        for field in registers.iter().flat_map(|reg| reg.fields()) {
            fields
                .entry(field.bit_offset())
                .or_insert_with(|| vec![])
                .push(field);
        }
        // HACK: field overlapping could happen, for now just remove the bigger
        // less generalized field
        for fields in fields.values_mut().filter(|fields| fields.len() > 1) {
            let min_len =
                fields.iter().map(|field| field.bit_width()).min().unwrap();
            fields.retain(|field| {
                let retain = field.bit_width() == min_len;
                if !retain {
                    let loc = context.gen_location();
                    let name = &field.name;
                    println!("Warning: removing general field {loc} {name}");
                }
                retain
            });
        }
        let mut fields: Vec<_> = fields
            .into_values()
            .map(|fields| {
                // TODO compare field access with register access
                FieldAccess::new(
                    context,
                    &properties,
                    fields,
                    &reg_name,
                    reset_value,
                    reset_mask,
                )
            })
            .collect();
        fields.sort_unstable_by_key(|field| field.lsb);

        // TODO check for overlapping fields

        let modified_write_values = helper::combine_modify_write_value(
            registers
                .iter()
                .filter_map(|register| register.modified_write_values),
        );
        let write_constraint = helper::combine_write_constraint(
            registers
                .iter()
                .filter_map(|register| register.write_constraint),
        );
        let read_action = helper::combine_read_actions(
            registers.iter().filter_map(|register| register.read_action),
        );

        let doc = registers
            .iter()
            .map(|reg| {
                let name = reg.doc_name();
                let description = reg
                    .description
                    .as_ref()
                    .map(String::as_str)
                    .map(str_to_doc)
                    .unwrap_or("No documentation".to_string());
                format!("{name}: {description}<br>")
            })
            .collect();

        Self {
            read_fun,
            write_fun,
            clean_value,
            properties,
            modified_write_values,
            write_constraint,
            read_action,
            address_offset: registers[0].address_offset,
            array: registers[0].array().cloned(),
            name,
            doc,
            fields,
        }
    }

    pub fn implicit_field(&self) -> bool {
        self.fields.is_empty()
    }

    pub(crate) fn gen_fields_functions<'a>(
        &'a self,
        context: &mut ContextCodeGen<'a>,
        tokens: &mut TokenStream,
    ) {
        if !self.implicit_field() {
            context.register = Some(self);
            self.fields
                .iter()
                .for_each(|x| x.gen_function(context, tokens));
            context.register = None;
        } else {
            // implicit field, generate the pseudo field for this register
            helper::read_write_field(
                context,
                None,
                &self.name,
                self.read_fun.as_ref(),
                self.write_fun.as_ref(),
                FieldData::from_bytes(self.properties.size.unwrap() / 8),
                self.properties.reset_value.unwrap_or(0),
                self.properties.reset_mask.unwrap_or(0),
                &self.doc,
                self.modified_write_values,
                self.write_constraint,
                self.read_action,
                &FieldRWType::Nothing,
                tokens,
            );
        }
    }

    pub(crate) fn gen_register_function(
        &self,
        context: &mut ContextCodeGen,
        tokens: &mut TokenStream,
    ) {
        if self.implicit_field() {
            self.gen_register_function_implicit_field(context, tokens);
            return;
        }
        let peripheral_instance_declare =
            (context.peripheral.instances.len() > 1).then(|| {
                quote! { _peripheral_instance: usize, }
            });
        let peripheral_struct_field = &context.peripheral.field_name;
        let peripheral_instance_index =
            (context.peripheral.instances.len() > 1).then(|| {
                quote! { [_peripheral_instance] }
            });
        let peripheral_field =
            quote! { #peripheral_struct_field #peripheral_instance_index };

        let cluster_instance_declare = context
            .clusters
            .iter()
            .filter_map(|clu| clu.dim.as_ref())
            .map(|(dim_name, _dim)| quote! {#dim_name: usize});
        let register_instance_declare =
            self.array().map(|_dim| quote! {_reg_array: usize});
        let dim_declare =
            cluster_instance_declare.chain(register_instance_declare);

        let cluster_instance_use = context
            .clusters
            .iter()
            .filter_map(|clu| clu.dim.as_ref())
            .map(|(dim_name, _dim)| quote! {#dim_name});
        let register_instance_use =
            self.array().map(|_dim| quote! {_reg_array});
        let dim_use = cluster_instance_use.chain(register_instance_use);

        let clean_value = Literal::u64_unsuffixed(self.clean_value);
        let bytes = self.properties.size.unwrap() / 8;
        let value_type = helper::DataType::from_bytes(bytes);
        let doc = |read| {
            format!(
                "{} {} from {}",
                if read { "Read" } else { "Write" },
                str_to_doc(&self.name),
                context.doc_peripheral(),
            )
        };
        if bytes == 1 {
            if let Some(read) = self.read_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let lsb = field.lsb;
                    let rotate = (lsb > 0).then(|| quote! { << #lsb});
                    let dim_use = dim_use.clone();
                    match field.enumerated_values {
                        FieldRWType::Nothing => Some(quote! {
                            self.#peripheral_field.#field_fun(
                                #(#dim_use,)*
                            )? #rotate;
                        }),
                        FieldRWType::ReadWrite { .. }
                        | FieldRWType::Separated { .. } => Some(quote! {
                            (self.#peripheral_field.#field_fun(
                                #(#dim_use,)*
                            )? as u8) #rotate;
                        }),
                    }
                });
                let dim_declare = dim_declare.clone();
                let doc = doc(true);
                tokens.extend(quote! {
                    #[doc = #doc]
                    #[inline]
                    pub(crate) fn #read(
                        &mut self,
                        #peripheral_instance_declare
                        #(#dim_declare,)*
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
                    let lsb = field.lsb;
                    let rotate = (lsb > 0).then(|| quote! { >> #lsb});
                    let mask = u8::MAX >> (u8::BITS - field.bit_width);
                    let dim_use = dim_use.clone();
                    match field.enumerated_values {
                        FieldRWType::Nothing => Some(quote! {
                            self.#peripheral_field.#field_fun(
                                #(#dim_use,)*
                                (_value #rotate) & #mask
                            )?;
                        }),
                        FieldRWType::ReadWrite { .. }
                        | FieldRWType::Separated { .. } => Some(quote! {
                            self.#peripheral_field.#field_fun(
                                #(#dim_use,)*
                                ((_value as u8) #rotate) & #mask
                            )?;
                        }),
                    }
                });
                let doc = doc(false);
                tokens.extend(quote! {
                    #[doc = #doc]
                    #[inline]
                    pub(crate) fn #write(
                        &mut self,
                        #peripheral_instance_declare
                        #(#dim_declare,)*
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
                    let field_type = DataType::from_bits(field.bit_width);
                    let read = field.read.as_ref().unwrap();
                    let lsb = field.lsb;
                    let rotate = (lsb > 0).then(|| quote! { << #lsb});
                    let dim_use = dim_use.clone();
                    match field.enumerated_values {
                        FieldRWType::Nothing => quote! {
                            _value |= #value_type::from(
                                self.#peripheral_field.#read(
                                    #(#dim_use,)*
                                )?
                            ) #rotate;
                        },
                        FieldRWType::ReadWrite { .. }
                        | FieldRWType::Separated { .. } => quote! {
                            _value |= #value_type::from(
                                self.#peripheral_field.#read(
                                    #(#dim_use,)*
                                )? as #field_type
                            ) #rotate;
                        },
                    }
                });
                let clean_value = Literal::u64_unsuffixed(self.clean_value);
                let dim_declare = dim_declare.clone();
                let doc = doc(true);
                tokens.extend(quote! {
                    // NOTE no comma on dim_declare
                    #[doc = #doc]
                    #[inline]
                    pub(crate) fn #read(
                        &mut self,
                        #peripheral_instance_declare
                        #(#dim_declare,)*
                    ) -> MemResult<#value_type> {
                        let mut _value = #clean_value;
                        #(#fields)*
                        Ok(_value)
                    }
                });
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().map(|field| {
                    let dim_use = dim_use.clone();
                    let field_start = field.lsb / 8;
                    let field_bytes = field.data.bytes();
                    let field_lsb = Literal::u32_unsuffixed(field.lsb % 8);
                    let field_end = Literal::u32_unsuffixed(field_start + field_bytes);
                    let field_start = Literal::u32_unsuffixed(field_start);

                    let write = field.write.as_ref().unwrap();

                    //TODO match agains FieldRWType
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
                                        #(#dim_use,)*
                                        ((_value[_i] >> #field_lsb) & #mask)
                                            .try_into()
                                            .map_err(|_| MemError::WriteViolation)?,
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
                                    self.#peripheral_field.#write(
                                        #(#dim_use,)*
                                        _value
                                            .try_into()
                                            .map_err(|_| MemError::WriteViolation)?,
                                    )?;
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
                                    self.#peripheral_field.#write(
                                        #(#dim_use,)*
                                        _extracted
                                            .try_into()
                                            .map_err(|_| MemError::WriteViolation)?,
                                    )?;
                                } else if (_start > #field_start && _start < #field_end)
                                    || (_end > #field_start && _end < #field_end) {
                                    return Err(MemError::WriteViolation);
                                }
                            }
                        }
                    }

                });
                // TODO implement byte endian here
                let doc = doc(false);
                tokens.extend(quote! {
                    // NOTE no comma on dim_declare
                    #[doc = #doc]
                    #[inline]
                    pub(crate) fn #write(
                        &mut self,
                        #peripheral_instance_declare
                        #(#dim_declare,)*
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
        context: &mut ContextCodeGen,
        tokens: &mut TokenStream,
    ) {
        let cluster_instance_declare = context
            .clusters
            .iter()
            .filter_map(|clu| clu.dim.as_ref())
            .map(|(dim_name, _dim)| quote! {#dim_name: usize});
        let register_instance_declare =
            self.array().map(|_dim| quote! {_reg_array: usize});
        let dim_declare =
            cluster_instance_declare.chain(register_instance_declare);

        let cluster_instance_use = context
            .clusters
            .iter()
            .filter_map(|clu| clu.dim.as_ref())
            .map(|(dim_name, _dim)| quote! {#dim_name});
        let register_instance_use =
            self.array().map(|_dim| quote! {_reg_array});
        let dim_use = cluster_instance_use.chain(register_instance_use);

        let peripheral_struct_field = &context.peripheral.field_name;
        let peripheral_instance_index =
            (context.peripheral.instances.len() > 1).then(|| {
                quote! { [_peripheral_instance] }
            });
        let peripheral_field =
            quote! { #peripheral_struct_field #peripheral_instance_index };

        let bytes = self.properties.size.unwrap() / 8;
        let bytes_lit = Literal::u32_unsuffixed(bytes);
        let value_type = helper::DataType::from_bytes(bytes);
        if bytes == 1 {
            todo!("one byte register");
        }
        let peripheral_instance_declare =
            (context.peripheral.instances.len() > 1).then(|| {
                quote! { _peripheral_instance: usize, }
            });
        let doc = |read| {
            format!(
                "{} {} from {}",
                if read { "Read" } else { "Write" },
                str_to_doc(&self.name),
                context.doc_peripheral(),
            )
        };
        if let Some(read) = &self.read_fun {
            let dim_declare = dim_declare.clone();
            let dim_use = dim_use.clone();
            let doc = doc(true);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub(crate) fn #read(
                    &mut self,
                    #peripheral_instance_declare
                    #(#dim_declare,)*
                ) -> MemResult<#value_type> {
                    self.#peripheral_field.#read(#(#dim_use,)*)
                }
            })
        }
        if let Some(write) = &self.write_fun {
            // TODO implement byte endian here
            let dim_declare = dim_declare.clone();
            let dim_use = dim_use.clone();
            let doc = doc(false);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub(crate) fn #write(
                    &mut self,
                    #peripheral_instance_declare
                    #(#dim_declare,)*
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

impl Dim for RegisterAccess {
    fn array(&self) -> Option<&DimElement> {
        self.array.as_ref()
    }
}
