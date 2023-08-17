use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, ToTokens};
use svd_parser::svd::{
    Access, Device, EnumeratedValues, Field, ModifiedWriteValues, Name,
    ReadAction, RegisterProperties, WriteConstraint,
};

use crate::{helper, memory::Context};

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
    pub is_array: bool,
    pub read: Option<Ident>,
    pub write: Option<Ident>,
    pub data: FieldData,
    pub fields: Vec<&'a Field>,
    pub reset_value: u64,
    pub reset_mask: u64,
    pub modified_write_values: Option<ModifiedWriteValues>,
    pub write_constraint: Option<WriteConstraint>,
    pub read_action: Option<ReadAction>,
    pub enumerated_values: Vec<EnumeratedValues>,
}

impl<'a> FieldAccess<'a> {
    pub(crate) fn new(
        context: &Context,
        device: &'a Device,
        properties: &RegisterProperties,
        fields: Vec<&'a Field>,
        is_array: bool,
        register_name: &str,
        reg_reset_value: u64,
        reg_reset_mask: u64,
    ) -> Self {
        // all fields names, used for error messages
        let field_names = || {
            let register_name = register_name.to_lowercase();
            fields
                .iter()
                .map(|field| -> String {
                    let field_name = field.name().to_lowercase();
                    context.gen_field_fun_name(&register_name, &field_name)
                })
                .collect::<Vec<_>>()
                .join(", ")
        };
        //TODO how to get the name?
        let name = context.gen_field_fun_name(
            &register_name,
            &fields[0].name().to_lowercase(),
        );
        // all fields have the same offset and len
        // TODO check that all fields have the same len, the offset was checked
        // by register call
        let width = fields[0].bit_width();
        let first_byte_len = fields[0].bit_offset();
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

        // fields can't have more permissions then the regs that contains it
        let access = properties
            .access
            .into_iter()
            .chain(fields.iter().filter_map(|field| field.access))
            .reduce(combine_access)
            .unwrap_or(Access::ReadWrite);
        if matches!(properties.access, Some(reg_access) if reg_access != access)
        {
            panic!("fields {} with diferent permissions", field_names())
        }

        let read = access.can_read().then(|| format_ident!("{}_read", name));
        let write = access.can_write().then(|| format_ident!("{}_write", name));

        let bits = u64::MAX >> (u64::BITS - width);
        let reset_value = (reg_reset_value >> fields[0].lsb()) & bits;
        let reset_mask = (reg_reset_mask >> fields[0].lsb()) & bits;

        let modified_write_values = helper::combine_modify_write_value(
            fields
                .iter()
                .filter_map(|field| field.modified_write_values),
        );
        let write_constraint = helper::combine_write_constraint(
            fields.iter().filter_map(|field| field.write_constraint),
        );
        let read_action = helper::combine_read_actions(
            fields.iter().filter_map(|field| field.read_action),
        );
        let enumerated_values =
            helper::combine_enumerate_value(fields.iter().flat_map(|field| {
                field.enumerated_values.iter().map(|values| {
                    if let Some(derived) = values.derived_from.as_ref() {
                        device
                            .peripherals
                            .iter()
                            .flat_map(|per| per.registers())
                            .flat_map(|reg| reg.fields())
                            .find_map(|field| {
                                field.enumerated_values.iter().find(|values| {
                                    values.name.as_ref() == Some(derived)
                                })
                            })
                            .unwrap()
                    } else {
                        values
                    }
                })
            }));
        Self {
            is_array,
            read,
            write,
            data,
            fields,
            reset_value,
            reset_mask,
            modified_write_values,
            write_constraint,
            read_action,
            enumerated_values,
        }
    }
}
impl ToTokens for FieldAccess<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let docs: Vec<_> = self
            .fields
            .iter()
            .filter_map(|field| {
                Some(format!(
                    "{}: {}",
                    field.name(),
                    field.description.as_ref()?,
                ))
            })
            .collect();
        let docs = docs.join("\n\n");
        let name: Vec<_> = self
            .fields
            .iter()
            .map(|field| format!("{}", field.name()))
            .collect();
        let name = name.join(", ");
        helper::read_write_field(
            self.is_array,
            &name,
            self.read.as_ref(),
            self.write.as_ref(),
            self.data,
            self.reset_value,
            self.reset_mask,
            &docs,
            self.modified_write_values,
            self.write_constraint,
            self.read_action,
            &self.enumerated_values,
            tokens,
        );
    }
}

fn combine_access(a: Access, b: Access) -> Access {
    use Access::*;
    match (a, b) {
        (ReadWrite, _) | (_, ReadWrite) => ReadWrite,
        (ReadOnly, WriteOnly) | (WriteOnly, ReadOnly) => ReadWrite,
        (ReadWriteOnce, WriteOnly) | (WriteOnly, ReadWriteOnce) => ReadWrite,
        (ReadWriteOnce, _) | (_, ReadWriteOnce) => ReadWriteOnce,
        (ReadOnly, ReadOnly) => ReadOnly,
        (WriteOnce, WriteOnce) => WriteOnce,
        (WriteOnce, WriteOnly)
        | (WriteOnly, WriteOnce)
        | (WriteOnly, WriteOnly) => WriteOnly,
        (ReadOnly, WriteOnce) | (WriteOnce, ReadOnly) => ReadWriteOnce,
    }
}
