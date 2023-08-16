use anyhow::{bail, Result};
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use svd_parser::svd::{
    DimElement, MaybeArray, ModifiedWriteValues, ReadAction, WriteConstraint,
};

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
    is_array: bool,
    name: &str,
    read: Option<&Ident>,
    write: Option<&Ident>,
    data: FieldData,
    reset_value: u64,
    reset_mask: u64,
    doc: &str,
    modified_write_values: Option<ModifiedWriteValues>,
    write_constraint: Option<WriteConstraint>,
    read_action: Option<ReadAction>,
    tokens: &mut TokenStream,
) {
    let todo_msg = |read| {
        use std::fmt::Write;
        let mut output = format!(
            "{} {name} mwrite {modified_write_values:?} write {write_constraint:?} rac {read_action:?} reset value ",
            if read { "read" } else { "write" },
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
    let dim = is_array.then(|| quote! {_dim: usize});
    let value_type = DataType::from_bits(data.bits());
    if let Some(read) = read.as_ref() {
        let todo_msg = todo_msg(true);
        tokens.extend(quote! {
            #[doc = #doc]
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
            pub(crate) fn #write(&mut self, #(#dim,)* _value: #value_type) -> MemResult<()> {
                todo!(#todo_msg)
            }
        });
    }
}

pub trait Dim {
    fn array(&self) -> Option<&DimElement>;
    fn is_one(&self) -> bool {
        match self.array() {
            Some(dim) => dim.dim == 1,
            None => true,
        }
    }
}

impl<T> Dim for MaybeArray<T> {
    fn array(&self) -> Option<&DimElement> {
        match self {
            MaybeArray::Single(_) => None,
            MaybeArray::Array(_, dim) => Some(dim),
        }
    }
}

pub fn offsets_from_dim<'a>(
    dim: &'a DimElement,
) -> impl Iterator<Item = u32> + Clone + 'a {
    let mut acc = 0;
    (0..dim.dim).map(move |_i| {
        let next = acc;
        acc += dim.dim_increment;
        next
    })
}

pub fn combine_modify_write_value(
    modified_write_values: impl Iterator<Item = ModifiedWriteValues>,
) -> Result<Option<ModifiedWriteValues>> {
    let mut output = None;
    for modify in modified_write_values {
        //use ModifiedWriteValues::*;
        match (output, modify) {
            (None, new) => output = Some(new),
            (Some(old), new) if old == new => {}
            (Some(old), new) => {
                bail!("can't combine modify write value {old:?} and {new:?}")
            }
        }
    }
    Ok(output)
}

pub fn combine_write_constraint(
    modified_write_values: impl Iterator<Item = WriteConstraint>,
) -> Result<Option<WriteConstraint>> {
    let mut output = None;
    for modify in modified_write_values {
        //use ModifiedWriteValues::*;
        match (output, modify) {
            (None, new) => output = Some(new),
            (Some(old), new) if old == new => {}
            (Some(old), new) => {
                bail!("can't combine write constraints {old:?} and {new:?}")
            }
        }
    }
    Ok(output)
}

pub fn combine_read_actions(
    modified_write_values: impl Iterator<Item = ReadAction>,
) -> Result<Option<ReadAction>> {
    let mut output = None;
    for modify in modified_write_values {
        //use ModifiedWriteValues::*;
        match (output, modify) {
            (None, new) => output = Some(new),
            (Some(old), new) if old == new => {}
            (Some(old), new) => {
                bail!("can't combine write constraints {old:?} and {new:?}")
            }
        }
    }
    Ok(output)
}
