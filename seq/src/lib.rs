use quote::quote;

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let _ = input;

    let expand = quote! {};

    proc_macro::TokenStream::from(expand)
}
