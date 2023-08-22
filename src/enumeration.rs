use std::collections::HashMap;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use svd_parser::svd;

use crate::formater::camel_case;
use crate::helper;
use crate::peripheral::EnumerationValuesId;

#[derive(Debug)]
pub struct EnumerationValues {
    // list of unique-ids for enumerations combined to create this struct.
    // For now the id is just the mem address, TODO use a more elegant id.
    pub ids: Vec<usize>,
    pub bits: u32,
    pub enum_name: Ident,
    pub values: Vec<FieldValue>,
}

#[derive(Debug, Default)]
pub enum FieldRWType {
    #[default]
    Nothing,
    ReadWrite {
        read_write: EnumerationValuesId,
    },
    Separated {
        read: EnumerationValuesId,
        write: EnumerationValuesId,
    },
}

#[derive(Debug)]
pub struct FieldValue {
    value: u64,
    name: Ident,
    doc: String,
}

impl FieldValue {
    pub(crate) fn combine_fields(
        values: &[&svd::EnumeratedValue],
    ) -> Vec<Self> {
        let mut field_values = HashMap::new();
        for field_value in values.iter() {
            let value = field_value.value.unwrap();
            field_values
                .entry(value)
                .or_insert(vec![])
                .push(field_value);
        }
        let mut values: Vec<_> = field_values
            .into_iter()
            .enumerate()
            .map(|(i, (value, fv))| {
                let docs: Vec<_> = fv
                    .iter()
                    .filter_map(|v| v.description.as_ref().map(String::as_str))
                    .collect();
                let name = camel_case(&fv[0].name.to_lowercase());
                Self {
                    value,
                    name: format_ident!("E{i}{name}"),
                    doc: docs.join("\n"),
                }
            })
            .collect();
        values.sort_unstable_by_key(|v| v.value);
        values
    }
}

impl EnumerationValues {
    pub fn gen_enum(&self) -> TokenStream {
        assert!(self.bits > 1);
        let data_type = helper::DataType::from_bits(self.bits);
        let name = &self.enum_name;
        let discriminants = self.values.iter().map(|value| {
            let doc = value.doc.as_str();
            let field = &value.name;
            let value = Literal::u64_unsuffixed(value.value);
            quote! {
                #[doc = #doc]
                #field = #value,
            }
        });
        let values = self
            .values
            .iter()
            .map(|value| Literal::u64_unsuffixed(value.value));
        let fields = self.values.iter().map(|value| &value.name);

        let impl_from = if 2usize.pow(self.bits) == self.values.len() {
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
