use crate::CRATE;
use proc_macro::{Diagnostic, Level};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Data, GenericParam, Generics, Token};


pub(crate) fn derive_operation_pattern_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();


    quote!(
      impl<'hir, const DIALECT: u16, const OPCODE: u16> OperationPattern<'hir> for OpcodeM<DIALECT, OPCODE> {
            fn match_op(ctxt: &HirCtxt<'hir>, op: OperationId) -> Option<Self> {
                if ctxt.ops[op].data.opcode == Opcode(DIALECT, OPCODE) {
                    Some(OpcodeM)
                } else {
                    None
                }
            }
        }
    );

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
