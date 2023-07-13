use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};

pub fn read_write_field(
    dim: u32,
    read: Option<&Ident>,
    write: Option<&Ident>,
    bytes: u32,
    tokens: &mut TokenStream,
) {
    let dim = (dim > 1).then(|| quote! {_dim: usize});
    if bytes == 1 {
        if let Some(read) = read {
            tokens.extend(quote! { pub fn #read(&self, #dim) -> icicle_vm::cpu::mem::MemResult<u8> {
                todo!()
            }})
        }
        if let Some(write) = write.as_ref() {
            let dim = dim.into_iter();
            tokens.extend(quote! {
                pub fn #write(&self, #(#dim,)* _value: u8) -> icicle_vm::cpu::mem::MemResult<()> {
                    todo!()
            }})
        }
    } else {
        let params: Box<[_]> = (0..bytes)
            .into_iter()
            .map(|i| format_ident!("_byte_{}", i))
            .collect();
        if let Some(read) = read.as_ref() {
            let body = read_write_generic_body(&params);
            let declare_params =
                dim.clone().into_iter().chain(params.iter().map(|param| {
                    quote! { #param: &mut Option<&mut u8> }
                }));
            tokens.extend(quote! {
                pub fn #read(&self, #(#declare_params),*) -> icicle_vm::cpu::mem::MemResult<()> {
                    #body
                }
            });
        }
        if let Some(write) = write.as_ref() {
            let body = read_write_generic_body(&params);
            let declare_params =
                dim.into_iter().chain(params.iter().map(|param| {
                    quote! { #param: Option<&u8> }
                }));
            tokens.extend(quote! {
                pub fn #write(&self, #(#declare_params),*) -> icicle_vm::cpu::mem::MemResult<()> {
                    #body
                }
            });
        }
    }
}

pub fn read_write_generic_body<'a>(params: &'a [Ident]) -> impl ToTokens + 'a {
    struct Idents<'a>(&'a [Ident]);
    impl ToTokens for Idents<'_> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let params = &self.0;
            let none_params = params.iter().map(|_| {
                quote! { None }
            });
            let some_params = params.iter().map(|param| {
                quote! { Some(#param) }
            });
            tokens.extend(quote! {
                let (#(#params),*) = match (#(#params),*) {
                    (#(#none_params),*) => unreachable!(),
                    (#(#some_params),*) => (#(#params),*),
                    _ => todo!(),
                };
                todo!();
            });
        }
    }
    Idents(params)
}

pub fn gen_function_write_from_buffer(tokens: &mut TokenStream) {
    tokens.extend(quote!{
        pub fn buffer_mut(
            _start: u64,
            _end: u64,
            _byte: u64,
            _buf: &[u8],
        ) -> Option<&mut u8> {
            if _start > _byte || _end <= _byte {
                return None;
            }
            let addr = _buf.as_ptr() as usize + (_byte - _start) as usize;
            Some(unsafe { std::mem::transmute(addr) })
        }
    });
}

pub fn gen_function_read_from_buffer(tokens: &mut TokenStream) {
    tokens.extend(quote!{
        pub fn buffer_const(
            _start: u64,
            _end: u64,
            _byte: u64,
            _buf: &[u8],
        ) -> Option<&u8> {
            if _start > _byte || _end <= _byte {
                return None;
            }
            Some(&_buf[(_byte - _start) as usize])
        }
    });
}
