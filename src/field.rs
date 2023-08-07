use anyhow::{bail, Result};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, ToTokens};
use svd_parser::svd::{Access, Field, Name, Peripheral, Register};

use crate::{formater::*, helper};

/// Number of bits of the field, first byte bits, number of bytes and bits
/// on the last byte
#[derive(Debug, Clone, Copy)]
pub enum FieldData {
    Single(u32),
    Multiple { first: u32, bytes: u32, last: u32 },
}

impl FieldData {
    pub fn from_bytes(bytes: u32) -> Self {
        match bytes {
            0 => unreachable!(),
            1 => Self::Single(8),
            bytes => Self::Multiple {
                first: 0,
                bytes,
                last: 0,
            },
        }
    }
    pub fn bits(&self) -> u32 {
        match *self {
            FieldData::Single(bits) => bits,
            FieldData::Multiple { first, bytes, last } => {
                first + (bytes * 8) + last
            }
        }
    }
    pub fn bytes(&self) -> u32 {
        match *self {
            FieldData::Single(bits) => (bits + 7) / 8,
            FieldData::Multiple { first, bytes, last } => {
                (first != 0) as u32 + bytes + (last != 0) as u32
            }
        }
    }
}

#[derive(Debug)]
pub struct FieldAccess<'a> {
    pub offset: u32,
    pub is_dim: bool,
    pub read: Option<Ident>,
    pub write: Option<Ident>,
    pub data: FieldData,
    pub reset_value: u64,
    pub reset_mask: u64,
    pub fields: Vec<(&'a Peripheral, &'a Register, &'a Field)>,
}

impl<'a> FieldAccess<'a> {
    pub fn new(
        fields: Vec<(&'a Peripheral, &'a Register, &'a Field)>,
        offset: u32,
        is_dim: bool,
        per_name: &str,
        reg_name: &str,
        default_access: Access,
        reg_reset_value: u64,
        reg_reset_mask: u64,
    ) -> Result<Self> {
        // all fields names, used for error messages
        let field_names = || {
            fields
                .iter()
                .map(|(per, reg, field)| -> String {
                    format!("{}::{}::{}", per.name(), reg.name(), field.name())
                })
                .collect::<Vec<_>>()
                .join(", ")
        };
        //TODO how to get the name?
        let name = snake_case(&fields[0].2.name);
        // all fields have the same offset and len
        // TODO check that all fields have the same len, the offset was checked
        // by register call
        let width = fields[0].2.bit_width();
        let first_byte_len = fields[0].2.bit_offset();
        let first_byte_len = match first_byte_len % 8 {
            0 => 0, // don't need align
            x => (8 - x).min(width),
        };
        let last_byte_len = (width - first_byte_len) % 8;
        let bytes = (width - (last_byte_len + first_byte_len)) / 8;
        let data = match (first_byte_len, bytes, last_byte_len) {
            (0, 1, 0) => FieldData::Single(8),
            (x, 0, 0) | (0, 0, x) => FieldData::Single(x),
            (first, bytes, last) => FieldData::Multiple { first, bytes, last },
        };

        // all fields need to have the same access permissions
        let mut access_iter = fields
            .iter()
            .map(|(_per, _reg, field)| field.access.unwrap_or(default_access));
        let access = access_iter.next().unwrap();
        if access_iter.any(|other_access| other_access != access) {
            bail!("fields {} with diferent permissions", field_names(),)
        }

        let read = access
            .can_read()
            .then(|| format_ident!("read_{}_{}_{}", per_name, reg_name, &name));
        let write = access.can_write().then(|| {
            format_ident!("write_{}_{}_{}", per_name, reg_name, &name)
        });

        let bits = u64::MAX >> (u64::BITS - width);
        let reset_value = (reg_reset_value >> fields[0].2.lsb()) & bits;
        let reset_mask = (reg_reset_mask >> fields[0].2.lsb()) & bits;
        Ok(Self {
            is_dim,
            offset,
            read,
            write,
            data,
            fields,
            reset_value,
            reset_mask,
        })
    }
}
impl ToTokens for FieldAccess<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let docs: Vec<_> = self
            .fields
            .iter()
            .filter_map(|(per, reg, field)| {
                Some(format!(
                    "{} {} {}: {}",
                    per.name(),
                    reg.name(),
                    field.name(),
                    field.description.as_ref()?,
                ))
            })
            .collect();
        let docs = docs.join("\n\n");
        let name: Vec<_> = self
            .fields
            .iter()
            .map(|(per, reg, field)| {
                format!("{} {} {}", per.name(), reg.name(), field.name())
            })
            .collect();
        let name = name.join(", ");
        helper::read_write_field(
            self.is_dim,
            &name,
            self.read.as_ref(),
            self.write.as_ref(),
            self.data,
            self.reset_value,
            self.reset_mask,
            &docs,
            tokens,
        );
    }
}
