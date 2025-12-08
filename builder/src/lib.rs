use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Error, Field, Fields, Ident, LitStr, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;

    let expanded = match input.data {
        Data::Struct(data_struct) => {
            let fields = match get_field_structs(&data_struct) {
                Ok(fields) => fields,
                Err(error) => return error.into_compile_error().into(),
            };

            let builder_function = generate_builder_function(&name, &fields);
            let builder = generate_builder_struct(&name, &fields);
            let builder_impl = generate_builder_impl(&name, &fields);

            quote! {
                #builder_function

                #builder

                #builder_impl
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    proc_macro::TokenStream::from(expanded)
}

fn generate_builder_function(struct_name: &Ident, fields: &[BuilderField]) -> TokenStream {
    let builder_name = builder_name(struct_name);
    let fields = fields
        .iter()
        .map(|field| {
            let name = field.name.clone();
            if field.is_vec {
                quote! { #name: std::vec::Vec::new() }
            } else {
                quote! { #name: std::option::Option::None }
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

fn generate_builder_struct(struct_name: &Ident, fields: &[BuilderField]) -> TokenStream {
    let builder_name = builder_name(struct_name);
    let fields = fields
        .iter()
        .map(|field| {
            let name = field.name.clone();
            let ty = field.ty.clone();

            if field.optional || field.is_vec {
                quote! { #name: #ty }
            } else {
                quote! { #name: std::option::Option<#ty> }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        pub struct #builder_name {
            #(#fields),*
        }
    }
}

fn generate_builder_impl(struct_name: &Ident, fields: &[BuilderField]) -> TokenStream {
    let builder_name = builder_name(struct_name);

    let setters = fields.iter().map(builder_setter).collect::<Vec<_>>();

    let struct_fields = fields
        .iter()
        .map(|field| {
            let name = &field.name;
            if field.is_vec {
                quote! {
                    #name: self.#name.clone()
                }
            } else if field.optional {
                quote! {
                    #name: self.#name.take()
                }
            } else {
                let err_msg = format!("Field {} is not set", name);
                quote! {
                    #name: self.#name.take().ok_or_else(|| std::boxed::Box::<dyn std::error::Error>::from(#err_msg))?
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                Ok(#struct_name {
                    #(#struct_fields),*
                })
            }
        }
    }
}

fn builder_setter(field: &BuilderField) -> TokenStream {
    let name = &field.name;
    let ty = &field.ty;

    let should_generate_default = if let Some(each_name) = &field.each {
        each_name != &field.name.to_string()
    } else {
        true
    };

    let default_impl = if should_generate_default {
        if let Some(inner_ty) = &field.inner_type {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else if field.is_vec {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        }
    } else {
        quote! {}
    };

    let each_impl = if let Some(each_name) = &field.each {
        let each_ident = format_ident!("{}", each_name);

        let inner_ty =
            extract_vec_inner_type(ty).expect("each attribute should only be used on Vec<T>");

        quote! {
            pub fn #each_ident(&mut self, #each_ident: #inner_ty) -> &mut Self {
                self.#name.push(#each_ident);
                self
            }
        }
    } else {
        quote! {}
    };

    quote! {
        #default_impl

        #each_impl
    }
}

fn builder_name(struct_name: &Ident) -> Ident {
    format_ident!("{}Builder", struct_name)
}

fn get_field_structs(data_struct: &DataStruct) -> Result<Vec<BuilderField>, Error> {
    match &data_struct.fields {
        Fields::Named(fields_named) => {
            let mut builder_fields = Vec::new();
            for field in &fields_named.named {
                let builder_field = BuilderField::new(field)?;
                builder_fields.push(builder_field);
            }
            Ok(builder_fields)
        }
        Fields::Unnamed(_) | Fields::Unit => Ok(vec![]),
    }
}

struct BuilderField {
    name: Ident,
    ty: Type,
    inner_type: Option<Type>,
    each: Option<String>,
    is_vec: bool,
    optional: bool,
}

impl BuilderField {
    fn new(field: &Field) -> Result<Self, Error> {
        let name = field.ident.clone().unwrap();
        let ty = field.ty.clone();
        let inner_type = extract_option_inner_type(&ty).cloned();
        let optional = extract_option_inner_type(&ty).is_some();

        let mut each: Option<String> = None;
        for attr in &field.attrs {
            if attr.path().is_ident("builder") {
                attr.parse_nested_meta(|meta| {
                    if meta.path.is_ident("each") {
                        let value = meta.value()?;
                        let lit_str: LitStr = value.parse()?;
                        each = Some(lit_str.value());
                        Ok(())
                    } else {
                        Err(meta.error("expected `builder(each = \"...\")`"))
                    }
                })?;
            }
        }

        let is_vec = extract_vec_inner_type(&ty).is_some();

        Ok(BuilderField {
            name,
            ty,
            inner_type,
            each,
            is_vec,
            optional,
        })
    }
}

fn extract_option_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if type_path.qself.is_none() {
            if let Some(segment) = type_path.path.segments.last() {
                if segment.ident == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                            return Some(inner_ty);
                        }
                    }
                }
            }
        }
    }
    None
}

fn extract_vec_inner_type(ty: &Type) -> Option<&Type> {
    if let Type::Path(type_path) = ty {
        if type_path.qself.is_none() {
            if let Some(segment) = type_path.path.segments.last() {
                if segment.ident == "Vec" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                            return Some(inner_ty);
                        }
                    }
                }
            }
        }
    }
    None
}
