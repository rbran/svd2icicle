use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use svd_parser::svd::{Device, EnumeratedValues, FieldInfo, MaybeArray};

use crate::{
    formater::{camel_case, dim_to_n},
    helper,
};

pub struct Enumeration<'a> {
    enum_name: Ident,
    bits: u32,
    values: &'a EnumeratedValues,
}

impl<'a> Enumeration<'a> {
    pub fn combine_all(svds: &'a [Device]) -> Vec<Enumeration<'a>> {
        svds.iter()
            .flat_map(|svd| svd.peripherals.iter())
            .flat_map(|per| per.all_registers())
            .flat_map(|reg| reg.fields())
            // we don't care about bools, most are just disable/enable
            .filter(|field| field.bit_width() > 1)
            .flat_map(|field| {
                std::iter::repeat(field)
                    .zip(field.enumerated_values.as_slice().iter())
            })
            // don't need the `derived_from`, just get the implementations
            .filter(|(_field, value)| value.derived_from.is_none())
            .enumerate()
            .map(|(i, (field, values))| Self::new(i, field, values))
            .collect()
    }

    fn new(
        i: usize,
        field: &'a MaybeArray<FieldInfo>,
        values: &'a EnumeratedValues,
    ) -> Self {
        let enum_name = if let Some(name) = &values.name {
            // fields are usualy uppercase
            format_ident!("E{i}{}", camel_case(&name.to_lowercase()))
        } else {
            format_ident!("E{i}{}", camel_case(&dim_to_n(&field.name.to_lowercase())))
        };
        let bits = field.bit_width();
        Self {
            enum_name,
            values,
            bits,
        }
    }

    pub fn gen_enum(&self) -> TokenStream {
        let data_type = helper::DataType::from_bits(self.bits);
        let name = &self.enum_name;
        let discriminants =
            self.values.values.iter().enumerate().map(|(i, value)| {
                let doc = value
                    .description
                    .as_ref()
                    .map(|doc| quote! {#[doc = #doc]});
                let field = format_ident!("D{i}{}", camel_case(&value.name));
                value.value.unwrap();
                let value = Literal::u64_unsuffixed(value.value.unwrap());
                quote! {
                    #doc
                    #field = #value,
                }
            });
        let values = self
            .values
            .values
            .iter()
            .map(|value| Literal::u64_unsuffixed(value.value.unwrap()));
        let fields =
            self.values.values.iter().enumerate().map(|(i, value)| {
                format_ident!("D{i}{}", camel_case(&value.name))
            });

        let impl_from = if 2usize.pow(self.bits) == self.values.values.len() {
            quote! {
                impl From<#data_type> for #name {
                    fn from(value: #data_type) -> #name {
                        match value {
                            #(#values => Self::#fields,)*
                            _ => unreachable!(),
                        }
                    }
                }
            }
        } else {
            quote! {
                impl TryFrom<#data_type> for #name {
                    type Error = ();
                    fn try_from(value: #data_type) -> Result<#name, Self::Error> {
                        match value {
                            #(#values => Ok(Self::#fields),)*
                            _ => Err(()),
                        }
                    }
                }
            }
        };
        quote! {
            #[derive(Debug, Clone, Copy)]
            #[repr(#data_type)]
            pub enum #name {
                #(#discriminants)*
            }
            #impl_from
        }
    }
}
