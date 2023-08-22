use proc_macro2::{Ident, TokenStream};
use quote::format_ident;
use svd_parser::svd::{
    self, Access, Device, FieldInfo, MaybeArray, ModifiedWriteValues, Name,
    ReadAction, RegisterProperties, WriteConstraint, DimElement,
};

use crate::enumeration::{EnumerationValues, FieldRWType, FieldValue};
use crate::formater::camel_case;
use crate::helper;
use crate::memory::ContextMemoryGen;
use crate::peripheral::{ContextCodeGen, EnumerationValuesId};

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

//TODO field could be an array
#[derive(Debug)]
pub struct FieldAccess {
    pub array: Option<DimElement>,
    pub name_doc: String,
    pub read: Option<Ident>,
    pub write: Option<Ident>,
    pub doc: String,
    pub lsb: u32,
    pub bit_width: u32,
    pub data: FieldData,
    //pub fields: Vec<&'a MaybeArray<FieldInfo>>,
    pub reset_value: u64,
    pub reset_mask: u64,
    pub modified_write_values: Option<ModifiedWriteValues>,
    pub write_constraint: Option<WriteConstraint>,
    pub read_action: Option<ReadAction>,
    pub enumerated_values: FieldRWType,
}

impl FieldAccess {
    pub(crate) fn new<'a>(
        context: &mut ContextMemoryGen<'a, '_>,
        properties: &RegisterProperties,
        fields: Vec<&'a MaybeArray<FieldInfo>>,
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
        let array = None;
        if fields.iter().any(|f| f.is_array()) {
            todo!("array field");
        }
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
            get_or_create_field_type(context, &name, &fields);
        let docs: Vec<_> = fields
            .iter()
            .filter_map(|field| {
                Some(format!(
                    "{}: {}",
                    field.name(),
                    field.description.as_ref()?,
                ))
            })
            .collect();
        let name_docs: Vec<_> =
            fields.iter().map(|field| field.name()).collect();
        Self {
            array,
            read,
            write,
            data,
            reset_value,
            reset_mask,
            modified_write_values,
            write_constraint,
            read_action,
            enumerated_values,
            lsb: fields[0].lsb(),
            bit_width: fields[0].bit_width(),
            name_doc: name_docs.join(", "),
            doc: docs.join("\n\n"),
        }
    }
    pub(crate) fn gen_function(
        &self,
        context: &mut ContextCodeGen,
        tokens: &mut TokenStream,
    ) {
        helper::read_write_field(
            context,
            self.array.as_ref(),
            &self.name_doc,
            self.read.as_ref(),
            self.write.as_ref(),
            self.data,
            self.reset_value,
            self.reset_mask,
            &self.doc,
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

fn get_enumerated_value_id(
    context: &ContextMemoryGen,
    fields: &[usize],
) -> Option<EnumerationValuesId> {
    context
        .enumerated_values
        .iter()
        .position(|ev| {
            if ev.ids.len() != fields.len() {
                return false;
            }
            ev.ids.iter().all(|id| fields.contains(&id))
        })
        .map(EnumerationValuesId)
}

fn find_enumerated_values<'a>(
    svds: &'a [Device],
    name: &str,
) -> Option<&'a svd::EnumeratedValues> {
    svds.iter()
        .flat_map(|svd| svd.peripherals.iter())
        .flat_map(|per| per.registers())
        .flat_map(|reg| reg.fields())
        .find_map(|field| {
            field.enumerated_values.iter().find(|values| {
                values.name.as_ref().map(String::as_str) == Some(name)
            })
        })
}

enum EnumerateFieldType<T> {
    Single(Vec<T>),
    Separated(Vec<T>, Vec<T>),
}

impl<T> EnumerateFieldType<T> {
    fn unify(&mut self) -> &mut Vec<T> {
        let (mut r, w) = match self {
            Self::Single(s) => return s,
            Self::Separated(r, w) => (core::mem::take(r), core::mem::take(w)),
        };
        r.extend(w);
        *self = Self::Single(r);
        let Self::Single(r) = self else {
            unreachable!();
        };
        r
    }
    fn len(&self) -> usize {
        match self {
            EnumerateFieldType::Single(x) => x.len(),
            EnumerateFieldType::Separated(r, w) => r.len() + w.len(),
        }
    }
}

impl<T> FromIterator<(Option<svd::Usage>, T)> for EnumerateFieldType<T> {
    fn from_iter<I: IntoIterator<Item = (Option<svd::Usage>, T)>>(
        iter: I,
    ) -> Self {
        use svd::Usage::*;
        use EnumerateFieldType::*;
        let mut output = Separated(vec![], vec![]);
        for (usage, item) in iter {
            let e = match (&mut output, usage) {
                // unify the fields
                (Separated(_, _), Some(ReadWrite) | None) => output.unify(),
                // get the correct one
                (Single(e), _)
                | (Separated(e, _), Some(Read))
                | (Separated(_, e), Some(Write)) => e,
            };
            e.push(item)
        }
        output
    }
}

// create unique ids for values in this field
fn enumerated_field_type<'a>(
    context: &ContextMemoryGen<'a, '_>,
    fields: &[&'a svd::Field],
) -> EnumerateFieldType<&'a svd::EnumeratedValue> {
    fields
        .iter()
        .flat_map(|f| f.enumerated_values.iter())
        .flat_map(|mut ev| {
            if let Some(derived) = &ev.derived_from {
                let derived = find_enumerated_values(&context.svds, derived)
                    .expect("Unable to find enumerated_value derived");
                assert!(
                    derived.derived_from.is_none(),
                    "enumerated_value deriving other derivation"
                );
                ev = derived;
            }
            std::iter::repeat(ev.usage).zip(ev.values.iter())
        })
        .collect()
}

fn get_or_create_enumerated_values<'a>(
    context: &mut ContextMemoryGen<'a, '_>,
    bits: u32,
    field_name: &str,
    fields: &[&'a svd::EnumeratedValue],
) -> EnumerationValuesId {
    //TODO use the pointer as an id?
    let field_name = camel_case(field_name);
    let fields_ids: Vec<_> = fields
        .into_iter()
        .map(|value| (*value as *const svd::EnumeratedValue) as usize)
        .collect();
    get_enumerated_value_id(context, &fields_ids).unwrap_or_else(|| {
        let new_id = context.enumerated_values.len();
        context.enumerated_values.push(EnumerationValues {
            ids: fields_ids,
            bits,
            enum_name: format_ident!("E{new_id}{field_name}"),
            values: FieldValue::combine_fields(fields),
        });
        EnumerationValuesId(new_id)
    })
}

fn get_or_create_field_type<'a>(
    context: &mut ContextMemoryGen<'a, '_>,
    name: &str,
    fields: &[&'a svd::Field],
) -> FieldRWType {
    if fields[0].bit_width() == 1 {
        // don't create enum for bool values
        return FieldRWType::Nothing;
    }
    let fields_values = enumerated_field_type(context, fields);
    if fields_values.len() == 0 {
        return FieldRWType::Nothing;
    }
    match fields_values {
        EnumerateFieldType::Single(values) => FieldRWType::ReadWrite {
            read_write: get_or_create_enumerated_values(
                context,
                fields[0].bit_width(),
                name,
                &values,
            ),
        },
        EnumerateFieldType::Separated(r, w) => FieldRWType::Separated {
            read: get_or_create_enumerated_values(
                context,
                fields[0].bit_width(),
                name,
                &r,
            ),
            write: get_or_create_enumerated_values(
                context,
                fields[0].bit_width(),
                name,
                &w,
            ),
        },
    }
}
