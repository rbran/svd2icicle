use proc_macro2::Ident;

use crate::field::FieldFunctions;

pub struct PeripheralStruct {
    mod_name: Ident,
    struct_name: Ident,
    fields: FieldFunctions,
}
