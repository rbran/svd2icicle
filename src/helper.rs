use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};

use crate::field::FieldData;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DataType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl DataType {
    #[inline]
    pub fn from_bytes(bytes: u32) -> Self {
        Self::from_bits(bytes * 8)
    }
    pub fn from_bits(bits: u32) -> Self {
        match bits {
            0 => unreachable!(),
            1 => Self::Bool,
            ..=8 => Self::U8,
            ..=16 => Self::U16,
            ..=32 => Self::U32,
            ..=64 => Self::U64,
            ..=128 => Self::U128,
            _ => todo!(),
        }
    }
}

impl ToTokens for DataType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            DataType::Bool => quote! {bool},
            DataType::U8 => quote! {u8},
            DataType::U16 => quote! {u16},
            DataType::U32 => quote! {u32},
            DataType::U64 => quote! {u64},
            DataType::U128 => quote! {u128},
        })
    }
}

pub fn read_write_field(
    dim: bool,
    name: &str,
    read: Option<&Ident>,
    write: Option<&Ident>,
    data: FieldData,
    reset_value: u64,
    reset_mask: u64,
    doc: &str,
    tokens: &mut TokenStream,
) {
    let todo_msg = |read| {
        use std::fmt::Write;
        let mut output = format!(
            "{} {} reset value ",
            if read { "read" } else { "write" },
            name,
        );
        if data.bits() == 1 {
            if reset_value == 0 {
                output.push_str("false")
            } else {
                output.push_str("true")
            }
        } else {
            write!(output, "0x{:02x} mask 0x{:02x}", reset_value, reset_mask)
                .unwrap()
        }
        output
    };
    let dim = dim.then(|| quote! {_dim: usize});
    let value_type = DataType::from_bits(data.bits());
    if let Some(read) = read.as_ref() {
        let todo_msg = todo_msg(true);
        tokens.extend(quote! {
            #[doc = #doc]
            #[inline]
            pub(crate) fn #read(&self, #dim) -> MemResult<#value_type> {
                todo!(#todo_msg)
            }
        });
    }
    if let Some(write) = write.as_ref() {
        let dim = dim.into_iter();
        let todo_msg = todo_msg(false);
        tokens.extend(quote! {
            #[doc = #doc]
            #[inline]
            pub(crate) fn #write(&mut self, #(#dim,)* _value: #value_type) -> MemResult<()> {
                todo!(#todo_msg)
            }
        });
    }
}
