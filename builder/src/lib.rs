use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Fields, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    let expanded = match input.data {
        Data::Struct(data_struct) => {
            let builder_function = generate_builder_function(&name, &data_struct);
            let builder = generate_builder_struct(&name, &data_struct);
            let builder_impl = generate_builder_impl(&name, &data_struct);

            quote! {
                use std::error::Error;

                #builder_function

                #builder

                #builder_impl
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    proc_macro::TokenStream::from(expanded)
}

fn generate_builder_function(struct_name: &Ident, data_struct: &DataStruct) -> TokenStream {
    let builder_name = builder_name(struct_name);
    let fields = get_field_structs(data_struct)
        .iter()
        .map(|(name, _)| {
            quote! {
                #name: None
            }
        })
        .collect::<Vec<_>>();

    quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#fields),*
                }
            }
        }
    }
}

fn generate_builder_struct(struct_name: &Ident, data_struct: &DataStruct) -> TokenStream {
    let builder_name = builder_name(struct_name);

    let fields = get_field_structs(data_struct)
        .iter()
        .map(|(name, ty)| {
            quote! {
                #name: Option<#ty>
            }
        })
        .collect::<Vec<_>>();

    quote! {
        pub struct #builder_name {
            #(#fields),*
        }
    }
}

fn generate_builder_impl(struct_name: &Ident, data_struct: &DataStruct) -> TokenStream {
    let builder_name = builder_name(struct_name);

    let setters = get_field_structs(data_struct)
        .iter()
        .map(|(name, ty)| {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        })
        .collect::<Vec<_>>();

    let struct_fields = get_field_structs(data_struct)
        .iter()
        .map(|(name, _)| {
            let err_msg = format!("Field {} is not set", name);
            quote! {
                #name: self.#name.ok_or(#err_msg.into())?
            }
        })
        .collect::<Vec<_>>();

    quote! {
        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
                #struct_name {
                    #(#struct_fields),*
                }
            }
        }
    }
}

fn builder_name(struct_name: &Ident) -> Ident {
    format_ident!("{}Builder", struct_name)
}

fn get_field_structs(data_struct: &DataStruct) -> Vec<(&Ident, &syn::Type)> {
    match &data_struct.fields {
        Fields::Named(fields_named) => fields_named
            .named
            .iter()
            .map(|f| (f.ident.as_ref().unwrap(), &f.ty))
            .collect(),
        Fields::Unnamed(_) | Fields::Unit => vec![],
    }
}
