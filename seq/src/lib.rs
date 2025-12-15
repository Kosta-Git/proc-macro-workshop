use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse::Parse, parse_macro_input};

#[derive(Debug)]
struct Seq {
    ident: syn::Ident,
    start: usize,
    end: usize,
    block: TokenStream,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: syn::Ident = input.parse()?;
        input.parse::<syn::Token![in]>()?;
        let start_lit: syn::LitInt = input.parse()?;
        input.parse::<syn::Token![..]>()?;
        let end_lit: syn::LitInt = input.parse()?;

        let content;
        syn::braced!(content in input);
        let block = content.parse::<TokenStream>()?;

        let start = start_lit.base10_parse::<usize>()?;
        let end = end_lit.base10_parse::<usize>()?;

        Ok(Self {
            ident,
            start,
            end,
            block,
        })
    }
}

fn replace_ident(tokens: TokenStream, target: &syn::Ident, replacement: usize) -> TokenStream {
    tokens
        .into_iter()
        .map(|token| {
            use proc_macro2::TokenTree;
            match token {
                TokenTree::Ident(ref ident) if ident == target => {
                    let lit = syn::LitInt::new(&replacement.to_string(), ident.span());
                    TokenTree::Literal(lit.token())
                }
                TokenTree::Group(ref group) => {
                    let stream = replace_ident(group.stream(), target, replacement);
                    TokenTree::Group(proc_macro2::Group::new(group.delimiter(), stream))
                }
                other => other,
            }
        })
        .collect()
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Seq);

    let expand = (input.start..input.end)
        .map(|i| replace_ident(input.block.clone(), &input.ident, i))
        .collect::<Vec<_>>();

    proc_macro::TokenStream::from(quote! {
        #(#expand)*
    })
}
