use proc_macro2::{Ident, Span};

pub fn to_ident(ident: impl AsRef<str>) -> Ident {
    Ident::new(ident.as_ref(), Span::call_site())
}
