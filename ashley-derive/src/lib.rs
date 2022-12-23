#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span)]
extern crate proc_macro;
use proc_macro2::Span;
use quote::{ToTokens, TokenStreamExt};

mod arena_any;
mod constraints;

use crate::{arena_any::derive_arena_any_impl, constraints::operation_constraint_match_body_impl};

//--------------------------------------------------------------------------------------------------
struct CrateName;
const CRATE: CrateName = CrateName;

impl ToTokens for CrateName {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.append(syn::Ident::new("ashley", Span::call_site()))
    }
}

#[proc_macro_derive(ArenaAny)]
pub fn derive_arena_any(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_arena_any_impl(input)
}

#[proc_macro]
pub fn operation_constraint_match_body(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    operation_constraint_match_body_impl(input)
}
