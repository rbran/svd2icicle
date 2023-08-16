use anyhow::{bail, Result};
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use svd_parser::svd::{
    DimElement, EnumeratedValues, MaybeArray, ModifiedWriteValues, ReadAction,
    WriteConstraint,
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
    enumerate_values: &[EnumeratedValues],
    tokens: &mut TokenStream,
) {
    let todo_msg = |read| {
        use std::fmt::Write;
        let mut output = format!(
            "{} {name} mwrite {modified_write_values:?} write {write_constraint:?} rac {read_action:?} reset value\nvalues: {enumerate_values:#?}\n",
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

fn stuff_need_to_be_equal<S: Eq + core::fmt::Debug>(
    mut stuff_iter: impl Iterator<Item = S>,
) -> Result<Option<S>> {
    let Some(output) = stuff_iter.next() else {
        return Ok(None)
    };
    for stuff in stuff_iter {
        if output != stuff {
            bail!("can't combine {output:?} and {stuff:?}")
        }
    }
    Ok(Some(output))
}

pub fn combine_modify_write_value(
    iter: impl Iterator<Item = ModifiedWriteValues>,
) -> Result<Option<ModifiedWriteValues>> {
    stuff_need_to_be_equal(iter)
}

pub fn combine_write_constraint(
    iter: impl Iterator<Item = WriteConstraint>,
) -> Result<Option<WriteConstraint>> {
    stuff_need_to_be_equal(iter)
}

pub fn combine_read_actions(
    iter: impl Iterator<Item = ReadAction>,
) -> Result<Option<ReadAction>> {
    stuff_need_to_be_equal(iter)
}

pub fn combine_enumerate_value<'a>(
    iter: impl Iterator<Item = &'a EnumeratedValues>,
) -> Result<Vec<EnumeratedValues>> {
    let mut output: Vec<EnumeratedValues> = vec![];
    for values in iter {
        if values.derived_from.is_some() {
            todo!("enumerated value derive {:?}", &values.derived_from);
        }
        if let Some(acc_value) =
            output.iter_mut().find(|o| o.usage() == values.usage())
        {
            acc_value.name = acc_value.name.take().or(values.name.clone());
            acc_value.usage = acc_value.usage.take().or(values.usage.clone());
            acc_value.values.extend(values.values.iter().cloned());
        } else {
            output.push(values.clone())
        }
    }
    Ok(output)
}
