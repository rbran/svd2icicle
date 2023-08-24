use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use svd_parser::svd::{
    self, DimElement, MaybeArray, ModifiedWriteValues, ReadAction,
    WriteConstraint,
};

use crate::{
    enumeration::FieldRWType, field::FieldData, peripheral::ContextCodeGen,
};

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

pub(crate) fn read_write_field(
    context: &mut ContextCodeGen,
    field_array: Option<&DimElement>,
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
    enumerate_values: &FieldRWType,
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
    let cluster_instance_declare = context
        .clusters
        .iter()
        .filter_map(|clu| clu.dim.as_ref())
        .map(|(dim_name, _dim)| quote! {#dim_name: usize});
    let register_instance_declare = context
        .register
        .and_then(|reg| reg.array())
        .map(|_dim| quote! {_reg_array: usize});
    let field_array_declare = field_array.map(|_| quote! {_dim: usize});
    let array_vars_declare = cluster_instance_declare
        .chain(register_instance_declare)
        .chain(field_array_declare);
    let value_type = |read_fun: bool| {
        let enum_type = match enumerate_values {
            FieldRWType::Nothing => {
                return DataType::from_bits(data.bits()).into_token_stream()
            }
            FieldRWType::ReadWrite { read_write } => {
                &context.device.get_enum(*read_write).enum_name
            }
            FieldRWType::Separated { read, write } => {
                let id = if read_fun { *read } else { *write };
                &context.device.get_enum(id).enum_name
            }
        };
        quote! { crate::peripheral::enums::#enum_type }
    };
    if let Some(read) = read.as_ref() {
        let value_type = value_type(true);
        let array_vars_declare = array_vars_declare.clone();
        let todo_msg = todo_msg(true);
        tokens.extend(quote! {
            #[doc = #doc]
            pub(crate) fn #read(
                &self,
                #(#array_vars_declare,)*
            ) -> MemResult<#value_type> {
                todo!(#todo_msg)
            }
        });
    }
    if let Some(write) = write.as_ref() {
        let value_type = value_type(false);
        let todo_msg = todo_msg(false);
        tokens.extend(quote! {
            #[doc = #doc]
            pub(crate) fn #write(
                &mut self,
                #(#array_vars_declare,)*
                _value: #value_type,
            ) -> MemResult<()> {
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

fn stuff_need_to_be_equal<S: Eq + core::fmt::Debug>(
    mut stuff_iter: impl Iterator<Item = S>,
) -> Option<S> {
    let output = stuff_iter.next()?;
    for stuff in stuff_iter {
        if output != stuff {
            panic!("can't combine {output:?} and {stuff:?}")
        }
    }
    Some(output)
}

pub fn combine_modify_write_value(
    iter: impl Iterator<Item = ModifiedWriteValues>,
) -> Option<ModifiedWriteValues> {
    stuff_need_to_be_equal(iter)
}

pub fn combine_write_constraint(
    iter: impl Iterator<Item = WriteConstraint>,
) -> Option<WriteConstraint> {
    stuff_need_to_be_equal(iter)
}

pub fn combine_read_actions(
    iter: impl Iterator<Item = ReadAction>,
) -> Option<ReadAction> {
    stuff_need_to_be_equal(iter)
}

pub fn str_to_doc(doc: &str) -> String {
    doc.replace('[', "\\[").replace(']', "\\]")
}

pub trait DisplayName {
    fn display_name(&self) -> Option<&str>;
    fn name(&self) -> &str;
    fn doc_name(&self) -> String {
        let name = str_to_doc(self.name());
        self.display_name()
            .map(|display_name| str_to_doc(display_name))
            .map(|display_name| format!("{display_name} ({name})"))
            .unwrap_or_else(|| name.to_string())
    }
}

impl DisplayName for svd::Peripheral {
    fn display_name(&self) -> Option<&str> {
        self.display_name.as_ref().map(String::as_ref)
    }
    fn name(&self) -> &str {
        self.name.as_str()
    }
}

impl DisplayName for svd::Register {
    fn display_name(&self) -> Option<&str> {
        self.display_name.as_ref().map(String::as_ref)
    }
    fn name(&self) -> &str {
        self.name.as_str()
    }
}
