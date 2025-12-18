use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{format_ident, quote};
use syn::{parse::Parse, parse_macro_input, LitInt};

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
        let start_lit: LitInt = input.parse()?;
        input.parse::<syn::Token![..]>()?;
        let incluse_range = input.parse::<syn::Token![=]>().ok(); // optional =
        let end_lit: LitInt = input.parse()?;

        let content;
        syn::braced!(content in input);
        let block = content.parse::<TokenStream>()?;

        let start = start_lit.base10_parse::<usize>()?;
        let end =
            end_lit.base10_parse::<usize>().map(
                |v| {
                    if incluse_range.is_some() {
                        v + 1
                    } else {
                        v
                    }
                },
            )?;

        Ok(Self {
            ident,
            start,
            end,
            block,
        })
    }
}

fn walk_groups(
    tokens: &TokenStream,
    f: &mut dyn FnMut(&TokenStream) -> TokenStream,
) -> TokenStream {
    f(tokens)
        .clone()
        .into_iter()
        .map(|token| {
            use proc_macro2::TokenTree;
            match token {
                TokenTree::Group(ref group) => {
                    let stream = walk_groups(&group.stream(), f);
                    let new_stream = f(&stream);
                    TokenTree::Group(proc_macro2::Group::new(group.delimiter(), new_stream))
                }
                other => other,
            }
        })
        .collect()
}

fn replace_ident(tokens: &TokenStream, target: &syn::Ident, replacement: usize) -> TokenStream {
    let mut tokens = Vec::from_iter(tokens.clone());

    let mut i = 0;
    while i < tokens.len() {
        if i + 2 < tokens.len() {
            // Check for pattern: Ident ~ Ident
            let is_paste_pattern = match (&tokens[i], &tokens[i + 1], &tokens[i + 2]) {
                // Prefix
                (TokenTree::Ident(ident1), TokenTree::Punct(punct), TokenTree::Ident(ident2))
                    if punct.as_char() == '~' && ident2 == target =>
                {
                    Some(format_ident!("{}{}", ident1, replacement))
                }
                // Suffix
                (TokenTree::Ident(ident1), TokenTree::Punct(punct), TokenTree::Ident(ident2))
                    if punct.as_char() == '~' =>
                {
                    Some(format_ident!("{}{}", ident1, ident2))
                }
                _ => None,
            };

            if let Some(new_ident) = is_paste_pattern {
                tokens.splice(i..i + 3, std::iter::once(TokenTree::Ident(new_ident)));
                continue;
            }
        }

        let is_target_ident = matches!(&tokens[i], TokenTree::Ident(ident) if ident == target);
        if is_target_ident {
            let lit = LitInt::new(&replacement.to_string(), Span::call_site());
            tokens[i] = TokenTree::Literal(lit.token());
        }

        i += 1;
    }

    TokenStream::from_iter(tokens)
}

fn repeat(seq: &Seq, tokens: &TokenStream) -> Vec<TokenStream> {
    (seq.start..seq.end)
        .map(|i| {
            walk_groups(tokens, &mut |stream: &TokenStream| {
                replace_ident(stream, &seq.ident, i)
            })
        })
        .collect()
}

fn walk_repetitions(
    tokens: &TokenStream,
    transform: &mut dyn FnMut(&TokenStream) -> TokenStream,
) -> (TokenStream, u32) {
    let mut tokens = Vec::from_iter(tokens.clone());
    let mut occurence_count = 0;
    let mut i = 0;
    while i < tokens.len() {
        if is_repeat_group(&tokens[i..]) {
            occurence_count += 1;
            if let TokenTree::Group(group) = &tokens[i + 1] {
                let (updated_stream, recursive_occ) = walk_repetitions(&group.stream(), transform);
                let transformed = transform(&updated_stream);
                tokens.splice(i..i + 3, transformed.into_iter());
                occurence_count += recursive_occ;
                continue;
            }
        }

        if let TokenTree::Group(group) = &tokens[i] {
            let (updated_stream, recursive_occ) = walk_repetitions(&group.stream(), transform);
            occurence_count += recursive_occ;
            tokens[i] =
                TokenTree::Group(proc_macro2::Group::new(group.delimiter(), updated_stream));
        }

        i += 1;
    }
    (TokenStream::from_iter(tokens), occurence_count)
}

fn is_repeat_group(tokens: &[TokenTree]) -> bool {
    if tokens.len() < 3 {
        return false;
    }

    matches!((&tokens[0], &tokens[1], &tokens[2]), (TokenTree::Punct(p1), TokenTree::Group(_), TokenTree::Punct(p2)) if p1.as_char() == '#' && p2.as_char() == '*')
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as Seq);

    let (processed_block, repetition_count) =
        walk_repetitions(&input.block, &mut |inner_tokens: &TokenStream| {
            let repeated = repeat(&input, inner_tokens);
            TokenStream::from_iter(repeated)
        });

    let output = if repetition_count > 0 {
        processed_block
    } else {
        let expand = repeat(&input, &input.block);
        quote! {
            #(#expand)*
        }
    };

    proc_macro::TokenStream::from(output)
}
