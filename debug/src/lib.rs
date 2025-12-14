use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, Error, GenericParam, Generics,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let result = data_struct_from_derive_input(&input);
    if let Err(e) = result {
        return e.into();
    }
    let data_struct = result.expect("data_struct_from_derive_input returned Ok");

    let debug_fields = data_struct
        .fields
        .iter()
        .map(DebugField::new)
        .collect::<Vec<_>>();

    if debug_fields.iter().any(|res| res.is_err()) {
        let errors = debug_fields
            .iter()
            .filter_map(|res| res.as_ref().err())
            .cloned()
            .collect::<Vec<Error>>();

        let combined_error = errors
            .into_iter()
            .fold(None, |acc: Option<Error>, err| match acc {
                Some(acc_err) => Some(Error::new(acc_err.span(), format!("{}\n{}", acc_err, err))),
                None => Some(err),
            })
            .expect("at least one error exists");

        return combined_error.into_compile_error().into();
    }

    let last_debug_field_index = debug_fields.len().saturating_sub(1);
    let is_last = |i: usize| i == last_debug_field_index;
    let debug_fields = debug_fields
        .into_iter()
        .map(|res| res.expect("all debug fields are Ok"))
        .enumerate()
        .map(|(i, debug_field)| debug_field.as_token_stream(is_last(i)))
        .collect::<Vec<TokenStream>>();

    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, stringify!(#name))?;
                write!(f, " {{ ")?;
                #(#debug_fields)*
                write!(f, " }}")
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn data_struct_from_derive_input(
    derive_input: &DeriveInput,
) -> Result<&syn::DataStruct, TokenStream> {
    match &derive_input.data {
        syn::Data::Struct(ref data_struct) => Ok(data_struct),
        syn::Data::Enum(_) => Err(struct_error(derive_input, "enum")),
        syn::Data::Union(_) => Err(struct_error(derive_input, "union")),
    }
}

fn struct_error(derive_input: &DeriveInput, got: &str) -> TokenStream {
    syn::Error::new(
        derive_input.span(),
        format!("expected `struct` got `{}`", got),
    )
    .into_compile_error()
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

struct DebugField {
    ident: syn::Ident,
    format: Option<String>,
}

impl DebugField {
    fn new(field: &syn::Field) -> Result<Self, Error> {
        let ident = field
            .ident
            .clone()
            .ok_or(syn::Error::new(field.span(), "expected named field"))?;

        let format = if Self::has_debug_attribute(field) {
            let attr = field
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("debug"))
                .expect("attribute exists");
            Some(Self::parse_debug_attribute(attr)?)
        } else {
            None
        };

        Ok(Self { ident, format })
    }

    fn has_debug_attribute(field: &syn::Field) -> bool {
        field.attrs.iter().any(|attr| attr.path().is_ident("debug"))
    }

    fn parse_debug_attribute(attr: &syn::Attribute) -> syn::Result<String> {
        let name_value = attr.meta.require_name_value()?;

        match &name_value.value {
            syn::Expr::Lit(expr_lit) => {
                if let syn::Lit::Str(lit_str) = &expr_lit.lit {
                    Ok(lit_str.value())
                } else {
                    Err(syn::Error::new_spanned(
                        expr_lit,
                        format!(
                            "expected string literal, found '{}'\n\
                            help: use a format string like `#[debug = \"{{format}}\"]`",
                            expr_lit.to_token_stream()
                        ),
                    ))
                }
            }
            _ => Err(syn::Error::new_spanned(
                name_value,
                "expected string literal\n\
                    help: use a format string like `#[debug = \"{{:?}}\"]`",
            )),
        }
    }

    pub fn as_token_stream(&self, is_last: bool) -> TokenStream {
        let ident = &self.ident;
        let format = self.format.clone().unwrap_or("{:?}".to_owned());

        let postfix = if is_last { "" } else { ", " };
        let format_str = format!("{{}}: {}{}", format, postfix);

        quote! {
            write!(
                f,
                #format_str,
                stringify!(#ident),
                &self.#ident
            )?;
        }
    }
}
