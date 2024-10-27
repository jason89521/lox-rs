use proc_macro::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, Data, Error};

pub fn impl_new_macro(ast: &syn::DeriveInput) -> TokenStream {
    if let Data::Struct(struct_data) = &ast.data {
        let (impl_generics, ty_generics, ..) = ast.generics.split_for_impl();
        let name = &ast.ident;
        let fields = struct_data
            .fields
            .iter()
            .map(|f| (f.ident.as_ref().unwrap(), &f.ty));
        let params = fields.clone().map(|(i, t)| {
            quote! {
              #i: #t
            }
        });
        let body = fields.clone().map(|(i, ..)| {
            quote! {
              #i
            }
        });
        let gen = quote! {
            impl #impl_generics #name #ty_generics {
                pub fn new(
                  #(#params),*
                ) -> Self {
                  Self {
                    #(#body),*
                  }
                }
            }
        };
        gen.into()
    } else {
        return TokenStream::from(
            Error::new(ast.span(), "Only support struct.").to_compile_error(),
        );
    }
}
