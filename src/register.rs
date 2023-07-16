use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use svd_parser::svd::{Name, Register};

use crate::field::{FieldAccess, FieldData};
use crate::formater::{dim_to_n, snake_case};
use crate::helper;
use crate::peripheral::Peripherals;

#[derive(Debug)]
pub struct RegisterFunctions<'a> {
    pub name: String,
    pub read_fun: Option<Ident>,
    pub write_fun: Option<Ident>,
    pub dim: u32,
    pub bytes: u32,
    pub regs: Vec<(usize, &'a Register)>,
    pub fields: Vec<FieldAccess<'a>>,
}

impl<'a> RegisterFunctions<'a> {
    pub fn new_empty(bytes: u32, reg: &'a Register) -> Self {
        let name = snake_case(&dim_to_n(&reg.name));
        let dim = match reg {
            svd_parser::svd::MaybeArray::Single(_reg) => 1,
            svd_parser::svd::MaybeArray::Array(_reg, dim) => dim.dim,
        };
        let read_fun = reg
            .properties
            .access
            .unwrap_or_default()
            .can_read()
            .then(|| format_ident!("read_{}", &name));
        let write_fun = reg
            .properties
            .access
            .unwrap_or_default()
            .can_read()
            .then(|| format_ident!("write_{}", &name));
        Self {
            name,
            dim,
            bytes,
            read_fun,
            write_fun,
            regs: vec![],
            fields: vec![],
        }
    }

    pub fn add(
        &mut self,
        bytes: u32,
        per_i: usize,
        reg: &'a Register,
        per_name: &str,
    ) {
        #[cfg(debug_assertions)]
        if let Some(last) = self.regs.last() {
            // Allowed: NRF51 TWI0 EVENTS_READY and EVENTS_RXDREADY are equal
            //if last.name() != reg.name() {
            //    panic!(
            //        "{} diff regs: {} != {}",
            //        per_name,
            //        last.name(),
            //        reg.name()
            //    );
            //}
            if self.bytes != bytes {
                panic!(
                    "{} diff regs len {}: {} != {}",
                    per_name,
                    self.bytes,
                    bytes,
                    last.1.name()
                );
            }
        }

        self.regs.push((per_i, reg));
        for field in reg.fields() {
            let field = FieldAccess::new(
                field,
                per_i,
                self.dim,
                &self.name,
                field.access.unwrap_or_default(),
            );
            self.fields.push(field);
        }
    }

    // HACK register with no fields are fields on thenselves
    #[cfg(debug_assertions)]
    pub fn check(&mut self) -> bool {
        let mut problem = true;
        //#[cfg(debug_assertions)]
        // TODO check if the fields overlap or goes outside the register

        if self.fields.len() == 0 {
            let first_per = self.regs.first().unwrap().0;
            if self.regs[1..].iter().any(|(per_i, _)| first_per != *per_i) {
                let regs: Vec<_> =
                    self.regs.iter().map(|reg| reg.1.name.as_str()).collect();
                if !regs.is_empty() {
                    problem = false;
                    println!("reg empty {}, multiple per", regs.join(", "));
                }
            }
        }
        problem
    }

    pub fn functions<'b>(
        &'b self,
        peripherals: &'b Peripherals,
    ) -> impl ToTokens + 'b {
        struct Tokens<'a, 'b: 'a>(
            &'b RegisterFunctions<'a>,
            &'b Peripherals<'a>,
        );
        impl ToTokens for Tokens<'_, '_> {
            fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                self.0.gen_functions(self.1, tokens)
            }
        }
        Tokens(self, peripherals)
    }

    fn gen_functions(
        &self,
        peripherals: &Peripherals,
        tokens: &mut proc_macro2::TokenStream,
    ) {
        let dim_declare = (self.dim > 1).then(|| quote! {_dim: usize,});
        let dim_use = (self.dim > 1).then(|| quote! {_dim,});
        if self.bytes == 1 {
            if let Some(read) = self.read_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let per_mod = &peripherals.peripheral_structs
                        [field.peripheral_index]
                        .mod_name;
                    let lsb = field.field.lsb();
                    Some(quote! {
                        self
                            .0
                            .lock()
                            .unwrap()
                            .#per_mod
                            .#field_fun(#dim_use)? << #lsb;
                    })
                });
                tokens.extend(quote! {
                    pub fn #read(
                        &self,
                        #dim_declare
                    ) -> icicle_vm::cpu::mem::MemResult<u8> {
                        let mut _value = 0;
                        #(_value |= #fields)*
                        Ok(value)
                    }
                })
            }
            if let Some(write) = self.write_fun.as_ref() {
                let fields = self.fields.iter().filter_map(|field| {
                    let field_fun = field.read.as_ref()?;
                    let per_mod = &peripherals.peripheral_structs
                        [field.peripheral_index]
                        .mod_name;
                    let lsb = field.field.lsb();
                    let mask = u8::MAX >> (u8::BITS - field.field.bit_width());
                    let dim = (self.dim > 1).then(|| quote! {_dim}).into_iter();
                    Some(quote! {
                        self.0.lock().unwrap().#per_mod.#field_fun(
                            #(#dim,)*
                            (_value >> #lsb) #mask
                        )?;
                    })
                });
                tokens.extend(quote! {
                    pub fn #write(
                        &self,
                        #dim_declare
                        _value: u8
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                })
            }
        } else {
            let params: Box<[_]> = (0..self.bytes)
                .into_iter()
                .map(|i| format_ident!("_byte_{}", i))
                .collect();
            if let Some(read) = self.read_fun.as_ref() {
                let declare_params = params.iter().map(|param| {
                    quote! { mut #param: Option<&mut u8> }
                });
                let fields = self.fields.iter().filter_map(|field| {
                    self.gen_field_register(
                        peripherals,
                        &params,
                        field.read.as_ref()?,
                        field,
                        |byte_match, per_mod, field_fun, lsb| quote! {
                            if let Some(byte) = &mut #byte_match {
                                **byte |= self
                                    .0
                                    .lock()
                                    .unwrap()
                                    .#per_mod
                                    .#field_fun(#dim_use)? << #lsb;
                            }
                        },
                        |per_mod, field_fun, params| quote! {
                            if #(#params.is_some())||* {
                                self
                                    .0
                                    .lock()
                                    .unwrap()
                                    .#per_mod
                                    .#field_fun(#dim_use #(&mut #params),*)?;
                            }
                        }
                    )
                });
                tokens.extend(quote! {
                    pub fn #read(
                        &self,
                        #dim_declare #(#declare_params),*
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                });
            }
            if let Some(write) = self.write_fun.as_ref() {
                let declare_params = params.iter().map(|param| {
                    quote! { #param: Option<&u8> }
                });
                let fields = self.fields.iter().filter_map(|field| {
                    self.gen_field_register(
                        peripherals,
                        &params,
                        field.write.as_ref()?,
                        field,
                        |byte_match, per_mod, field_fun, lsb| {
                            let mask = Literal::u8_unsuffixed(
                                u8::MAX >> (u8::BITS - field.field.bit_width()),
                            );
                            quote! {
                                if let Some(byte) = #byte_match {
                                    self
                                        .0
                                        .lock()
                                        .unwrap()
                                        .#per_mod
                                        .#field_fun(
                                            #dim_use (*byte >> #lsb) & #mask
                                        )?;
                                }
                            }
                        },
                        |per_mod, field_fun, params| {
                            quote! {
                                if #(#params.is_some())||* {
                                    self
                                        .0
                                        .lock()
                                        .unwrap()
                                        .#per_mod
                                        .#field_fun(#dim_use #(#params,)*)?;
                                }
                            }
                        },
                    )
                });
                tokens.extend(quote! {
                    pub fn #write(
                        &self,
                        #dim_declare #(#declare_params),*
                    ) -> icicle_vm::cpu::mem::MemResult<()> {
                        #(#fields)*
                        Ok(())
                    }
                });
            }
        }
    }

    fn gen_field_register<S, M>(
        &self,
        peripherals: &Peripherals,
        params: &[Ident],
        field_fun: &Ident,
        field: &FieldAccess,
        mut caller_single: S,
        mut caller_multiple: M,
    ) -> Option<TokenStream>
    where
        S: FnMut(&Ident, &Ident, &Ident, Literal) -> TokenStream,
        M: FnMut(&Ident, &Ident, &[Ident]) -> TokenStream,
    {
        let per_mod =
            &peripherals.peripheral_structs[field.peripheral_index].mod_name;
        let lsb = field.field.lsb();
        let first_byte = (lsb / 8) as usize;
        match field.data {
            FieldData::Single(_bits) => {
                let byte_match = &params[first_byte];
                let lsb = Literal::u32_unsuffixed(lsb);
                Some(caller_single(byte_match, per_mod, field_fun, lsb))
            }
            FieldData::Multiple { first, bytes, last } => {
                let last_byte = first_byte
                    + (first != 0) as usize
                    + (last != 0) as usize
                    + bytes as usize;
                let params = &params[first_byte..last_byte];
                Some(caller_multiple(per_mod, field_fun, params))
            }
        }
    }

    pub fn gen_function_call<I>(
        &self,
        read: bool,
        dim: Option<Literal>,
        params: I,
    ) -> Option<TokenStream>
    where
        I: Iterator<Item = TokenStream>,
    {
        let fun = if read {
            self.read_fun.as_ref()?
        } else {
            &self.write_fun.as_ref()?
        };
        let dim = dim.into_iter();
        Some(quote! { self.#fun(#(#dim,)* #(#params),*)?; })
    }

    pub fn gen_fields_functions<'b>(
        &'b self,
        peripheral_index: usize,
    ) -> impl Iterator<Item = TokenStream> + 'b {
        self.fields
            .iter()
            .filter(move |field| field.peripheral_index == peripheral_index)
            .map(|x| x.into_token_stream())
            .chain(
                self.fields
                    .is_empty()
                    .then(|| {
                        let mut tokens = TokenStream::new();
                        helper::read_write_field(
                            self.dim,
                            self.read_fun.as_ref(),
                            self.write_fun.as_ref(),
                            self.bytes,
                            &mut tokens,
                        );
                        tokens
                    })
                    .into_iter(),
            )
    }
}
