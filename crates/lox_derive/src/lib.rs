use proc_macro::TokenStream;

mod new;
mod span;

use new::impl_new_macro;
use span::impl_span_macro;

#[proc_macro_derive(Span)]
pub fn span_macro_derive(input: TokenStream) -> TokenStream {
    // Construct a representation of Rust code as a syntax tree
    // that we can manipulate
    let ast = syn::parse(input).unwrap();

    // Build the trait implementation
    impl_span_macro(&ast)
}

#[proc_macro_derive(New)]
pub fn new_macro_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();
    impl_new_macro(&ast)
}
