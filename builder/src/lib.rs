use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn ty_inner_type<'a>(wrapper: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type("Option", ty).is_some() || builder_of(f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name:  std::option::Option<#ty>}
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        let set_method = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote!(
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            )
        } else if builder_of(f).is_some() {
            quote!(
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            )
        } else {
            quote!(
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            )
        };

        match extend_method(f) {
            Some((true, extend_method)) => extend_method,
            Some((false, extend_method)) => quote! {
                #set_method
                #extend_method
            },
            None => set_method,
        }
    });

    fn builder_of(f: &syn::Field) -> Option<&syn::MetaList> {
        for attr in &f.attrs {
            if attr.path().segments.len() == 1 && attr.path().segments[0].ident == "builder" {
                return Some(attr.meta.require_list().unwrap());
            }
        }
        None
    }

    fn extend_method(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
        let name = f.ident.as_ref().unwrap();
        let l = builder_of(f)?;

        for token in l.tokens.clone() {
            match token {
                TokenTree::Ident(ref i) => {
                    assert_eq!(i, "each", "expected 'each', found {}", i)
                }
                TokenTree::Punct(ref p) => {
                    assert_eq!(p.as_char(), '=', "expected '=', found {}", p)
                }
                TokenTree::Group(g) => panic!("unexpected group {}", g),
                TokenTree::Literal(ref l) => match syn::Lit::new(l.clone()) {
                    syn::Lit::Str(s) => {
                        let arg = syn::Ident::new(&s.value(), s.span());
                        let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
                        let method = quote! {
                            pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                                self.#name.push(#arg);
                                self
                            }
                        };
                        return Some((&arg == name, method));
                    }
                    lit => panic!("expected string, found {:?}", lit),
                },
            }
        }
        None
    }

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;

        if ty_inner_type("Option", &f.ty).is_some() || builder_of(f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;
        if builder_of(f).is_some() {
            quote! { #name: Vec::new() }
        } else {
            quote! { #name: None }
        }
    });

    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }

        impl #bident {
            #(#methods)*

            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(
                    #name {
                        #(#build_fields,)*
                    }
                )
            }
        }

        impl #name {
            fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };

    expanded.into()
}
