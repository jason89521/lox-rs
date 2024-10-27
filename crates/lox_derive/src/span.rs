use proc_macro::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, DataEnum, DeriveInput, Error};

pub fn impl_span_macro(ast: &DeriveInput) -> TokenStream {
    match &ast.data {
        syn::Data::Struct(_) => impl_span_struct_macro(ast),
        syn::Data::Enum(data_enum) => impl_span_enum_macro(ast, data_enum),
        syn::Data::Union(_) => TokenStream::from(
            Error::new(ast.span(), "Only allow struct or enum").to_compile_error(),
        ),
    }
}

fn impl_span_struct_macro(ast: &syn::DeriveInput) -> TokenStream {
    let (impl_generics, ty_generics, ..) = ast.generics.split_for_impl();
    let name = &ast.ident;
    let gen = quote! {
        impl #impl_generics lox_span::GetSpan for #name #ty_generics {
            fn span(&self) -> lox_span::Span {
                self.span
            }
        }
    };
    gen.into()
}

fn impl_span_enum_macro(ast: &DeriveInput, data_enum: &DataEnum) -> TokenStream {
    let (impl_generics, ty_generics, ..) = ast.generics.split_for_impl();
    let enum_ident = &ast.ident;
    let branches = data_enum.variants.iter().map(|var| {
        let variant_ident = &var.ident;
        quote! {
          #enum_ident::#variant_ident(expr) => expr.span()
        }
    });

    quote! {
      impl #impl_generics lox_span::GetSpan for #enum_ident #ty_generics {
        fn span(&self) -> lox_span::Span {
          match self {
            #(#branches),*
          }
        }
      }
    }
    .into()
}
