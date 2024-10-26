use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Span)]
pub fn hello_macro_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_hello_macro(&ast)
}

fn impl_hello_macro(ast: &syn::DeriveInput) -> TokenStream {
    let (impl_generics, ty_generics, ..) = ast.generics.split_for_impl();
    let name = &ast.ident;
    let gen = quote! {
        impl #impl_generics span::GetSpan for #name #ty_generics {
            fn span(&self) -> Span {
                self.span
            }
        }
    };
    gen.into()
}
