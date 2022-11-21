use crate::CRATE;
use proc_macro::{Diagnostic, Level};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Data, GenericParam, Generics, Token};

struct TypeGenericsWithStaticLifetime<'a>(&'a Generics);

// modified from syn
impl<'a> ToTokens for TypeGenericsWithStaticLifetime<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.0.params.is_empty() {
            return;
        }
        self.0.lt_token.as_ref().unwrap().to_tokens(tokens);

        let mut trailing_or_empty = true;
        for param in self.0.params.pairs() {
            if let GenericParam::Lifetime(_) = *param.value() {
                // replace all lifetimes with 'static
                quote!('static).to_tokens(tokens);
                param.punct().to_tokens(tokens);
                trailing_or_empty = param.punct().is_some();
            }
        }
        for param in self.0.params.pairs() {
            if let GenericParam::Lifetime(_) = **param.value() {
                continue;
            }
            if !trailing_or_empty {
                <Token![,]>::default().to_tokens(tokens);
                trailing_or_empty = true;
            }
            match *param.value() {
                GenericParam::Lifetime(_) => unreachable!(),
                GenericParam::Type(param) => {
                    // Leave off the type parameter defaults
                    param.ident.to_tokens(tokens);
                }
                GenericParam::Const(param) => {
                    // Leave off the const parameter defaults
                    param.ident.to_tokens(tokens);
                }
            }
            param.punct().to_tokens(tokens);
        }

        self.0.gt_token.as_ref().unwrap().to_tokens(tokens);
    }
}

pub(crate) fn derive_arena_any_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    // filter unions.
    match input.data {
        Data::Union(_) => {
            Diagnostic::spanned(
                input.span().unwrap(),
                Level::Error,
                "`ArenaAny` can only be derived on structs or enums",
            )
            .emit();
            return quote! {}.into();
        }
        _ => {}
    };

    // Check that there's at most one lifetime parameter, which will be used as the arena lifetime.
    if input.generics.lifetimes().count() > 1 {
        Diagnostic::spanned(
            input.span().unwrap(),
            Level::Error,
            "More than one lifetime parameter on type",
        )
        .note("`ArenaAny` can only be derived on types with at most one lifetime parameter")
        .emit();
        return quote! {}.into();
    }

    let ident = input.ident;
    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();
    let type_generics_with_static_lifetime = TypeGenericsWithStaticLifetime(&input.generics);

    let generated = if let Some(lt) = input.generics.lifetimes().next() {
        // the type has the arena lifetime parameter 'lt
        let lt = &lt.lifetime;
        quote! {
            // single lifetime: OK
            unsafe impl #impl_generics #CRATE::utils::ArenaAny<#lt> for #ident #type_generics #where_clause {
                fn static_type_id() -> ::std::any::TypeId {
                    ::std::any::TypeId::of::<#ident #type_generics_with_static_lifetime >()
                }

                fn type_id(&self) -> ::std::any::TypeId {
                    ::std::any::TypeId::of::<#ident #type_generics_with_static_lifetime>()
                }

                fn as_any(&self) -> &dyn #CRATE::utils::ArenaAny<#lt> {
                    self
                }
            }
        }
    } else {
        // no lifetime parameters on the type
        quote! {
            unsafe impl<'a> #impl_generics #CRATE::utils::ArenaAny<'a> for #ident #type_generics #where_clause {
                fn static_type_id() -> ::std::any::TypeId {
                    // should be 'static since there's no lifetime parameter
                    ::std::any::TypeId::of::<Self>()
                }

                fn type_id(&self) -> ::std::any::TypeId {
                    ::std::any::TypeId::of::<Self>()
                }

                fn as_any(&self) -> &dyn #CRATE::utils::ArenaAny<'a> {
                    self
                }
            };
        }
    };

    generated.into()
}
