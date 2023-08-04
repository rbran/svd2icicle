use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

use crate::field::FieldData;

fn byte_type(bits: u32) -> TokenStream {
    match bits {
        0 => unreachable!(),
        1 => quote! {bool},
        _ => quote! {u8},
    }
}

pub fn read_write_field(
    dim: u32,
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
    let dim = (dim > 1).then(|| quote! {_dim: usize});
    if let FieldData::Single(bits) = data {
        let value_type = byte_type(bits);
        if let Some(read) = read {
            let todo_msg = todo_msg(true);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub fn #read(&self, #dim) -> MemResult<#value_type> {
                    todo!(#todo_msg)
                }
            })
        }
        if let Some(write) = write.as_ref() {
            let dim = dim.into_iter();
            let todo_msg = todo_msg(false);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub fn #write(&mut self, #(#dim,)* _value: #value_type) -> MemResult<()> {
                    todo!(#todo_msg)
            }})
        }
    } else {
        let params: Box<[_]> = (0..data.bytes())
            .map(|i| format_ident!("_byte_{}", i))
            .collect();
        if let Some(read) = read.as_ref() {
            let declare_params =
                dim.clone().into_iter().chain(params.iter().map(|param| {
                    quote! { #param: &mut Option<&mut u8> }
                }));
            let todo_msg = todo_msg(true);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub fn #read(&self, #(#declare_params),*) -> MemResult<()> {
                    todo!(#todo_msg)
                }
            });
        }
        if let Some(write) = write.as_ref() {
            let declare_params =
                dim.into_iter().chain(params.iter().map(|param| {
                    quote! { #param: Option<&u8> }
                }));
            let todo_msg = todo_msg(false);
            tokens.extend(quote! {
                #[doc = #doc]
                #[inline]
                pub fn #write(&mut self, #(#declare_params),*) -> MemResult<()> {
                    todo!(#todo_msg)
                }
            });
        }
    }
}
