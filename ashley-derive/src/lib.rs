#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span)]
extern crate proc_macro;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};

mod hir_type;

//--------------------------------------------------------------------------------------------------
struct CrateName;
const CRATE: CrateName = CrateName;

impl ToTokens for CrateName {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append(syn::Ident::new("ashley", Span::call_site()))
    }
}

#[proc_macro_derive(HirType)]
pub fn hir_type_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    hir_type::derive(input).into()
}
