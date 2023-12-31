use std::collections::HashMap;

use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use svd_parser::svd;

use crate::formater::camel_case;
use crate::helper::{self, str_to_doc};
use crate::peripheral::EnumerationValuesId;

#[derive(Debug)]
pub(crate) struct EnumerationValues {
    // list of unique-ids for enumerations combined to create this struct.
    // For now the id is just the mem address, TODO use a more elegant id.
    pub ids: Vec<usize>,
    pub doc: String,
    pub bits: u32,
    pub enum_name: Ident,
    pub values: Vec<FieldValue>,
}

#[derive(Debug, Default, Clone)]
pub(crate) enum FieldRWType {
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
pub(crate) struct FieldValue {
    value: u64,
    name: Ident,
    doc: String,
}

impl FieldValue {
    pub fn combine_fields(
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
        // used to create predictable/reproduciable enum orders
        let mut values: Vec<_> = field_values.into_iter().collect();
        values.sort_unstable_by_key(|(v, _)| *v);
        let mut values: Vec<_> = values
            .into_iter()
            .enumerate()
            .map(|(i, (value, fv))| {
                let doc = fv
                    .iter()
                    .map(|v| {
                        let name = str_to_doc(&v.name);
                        let description = v
                            .description
                            .as_ref()
                            .map(String::as_str)
                            .map(str_to_doc)
                            .unwrap_or("No documentation".to_string());
                        format!("{name}: {description}<br>")
                    })
                    .collect();
                let name = camel_case(&fv[0].name.to_lowercase());
                Self {
                    value,
                    name: format_ident!("E{i}{name}"),
                    doc,
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
            quote! {
                #[doc = #doc]
                #field,
            }
        });
        let values = self
            .values
            .iter()
            .map(|value| Literal::u64_unsuffixed(value.value));
        let fields = self.values.iter().map(|value| &value.name);

        let fields2 = fields.clone();
        let values2 = values.clone();
        let impl_into = quote! {
            impl From<#name> for #data_type {
                fn from(value: #name) -> #data_type {
                    match value {
                        #(#name::#fields2 => #values2,)*
                    }
                }
            }
        };
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
        let doc = &self.doc;
        let doc = (!doc.is_empty()).then(|| quote! { #[doc = #doc] });
        quote! {
            #doc
            #[derive(Debug, Clone, Copy)]
            pub enum #name {
                #(#discriminants)*
            }
            #impl_into
            #impl_from
        }
    }
}
