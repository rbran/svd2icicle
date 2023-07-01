use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, ToTokens};
use svd_parser::svd::{Access, Field};

use crate::{formater::*, helper};

/// Number of bits of the field, first byte bits, number of bytes and bits
/// on the last byte
#[derive(Debug, Clone, Copy)]
pub enum FieldData {
    Single(u32),
    Multiple { first: u32, bytes: u32, last: u32 },
}

#[derive(Debug)]
pub struct FieldAccess<'a> {
    pub dim: u32,
    pub read: Option<Ident>,
    pub write: Option<Ident>,
    pub data: FieldData,
    pub field: &'a Field,
    pub peripheral_index: usize,
}

impl<'a> FieldAccess<'a> {
    pub fn new(
        field: &'a Field,
        peripheral_index: usize,
        dim: u32,
        reg_name: &str,
        access: Access,
    ) -> Self {
        let first = match field.bit_offset() % 8 {
            // don't need a first non-complete byte
            0 => 0,
            x => (8 - x).min(field.bit_width()),
        };
        let last = (field.bit_width() - first) % 8;
        let bytes = (field.bit_width() - (last + first)) / 8;
        let data = match (first, bytes, last) {
            (0, 1, 0) => FieldData::Single(8),
            (x, 0, 0) | (0, 0, x) => FieldData::Single(x),
            (first, bytes, last) => FieldData::Multiple { first, bytes, last },
        };
        let field_name = snake_case(&field.name);
        let read = access
            .can_read()
            .then(|| format_ident!("read_{}_{}", reg_name, &field_name));
        let write = access
            .can_write()
            .then(|| format_ident!("write_{}_{}", reg_name, &field_name));
        Self {
            dim,
            read,
            write,
            data,
            field,
            peripheral_index,
        }
    }
}
impl ToTokens for FieldAccess<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let bytes = match self.data {
            FieldData::Single(_) => 1,
            FieldData::Multiple { first, bytes, last } => {
                bytes + (first != 0) as u32 + (last != 0) as u32
            }
        };
        helper::read_write_field(
            self.dim,
            self.read.as_ref(),
            self.write.as_ref(),
            bytes,
            tokens,
        );
    }
}
