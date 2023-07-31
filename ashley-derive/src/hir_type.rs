use crate::CRATE;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_quote, spanned::Spanned, GenericParam, Generics};

/// Checks that the derive input has a repr(C) attribute.
pub(crate) fn has_repr_c_attr(ast: &syn::DeriveInput) -> bool {
    ast.attrs.iter().any(|attr| match attr.parse_meta() {
        Ok(meta) => match meta {
            syn::Meta::List(list) => {
                (list.path.get_ident().map_or(false, |i| i.to_string() == "repr"))
                    && list.nested.iter().next().map_or(false, |n| match n {
                        syn::NestedMeta::Meta(syn::Meta::Path(ref path)) => {
                            path.get_ident().map_or(false, |i| i.to_string() == "C")
                        }
                        _ => false,
                    })
            }
            _ => false,
        },
        Err(_) => false,
    })
}

/// Utility function to generate `const fn layout() -> [(usize,usize); N]`, which returns the offsets and sizes of each
/// field of a repr(C) struct.
///
/// This assumes that the derive input is a repr(C) struct.
pub(crate) fn generate_repr_c_layout_const_fn(derive_input: &syn::DeriveInput) -> Result<TokenStream, syn::Error> {
    let fields = match derive_input.data {
        syn::Data::Struct(ref s) => &s.fields,
        _ => return Err(syn::Error::new(derive_input.span(), "Expected a struct item")),
    };

    // generate code to recursively compute field offsets and sizes according to C layout rules
    let compute_offset_size_stmts: Vec<_> = fields
        .iter()
        .enumerate()
        .map(|(i, f)| {
            let field_ty = &f.ty;
            if i == 0 {
                quote! {
                    offsets[0] = 0;
                    sizes[0] = ::std::mem::size_of::<#field_ty>();
                }
            } else {
                let i0 = i - 1;
                let i1 = i;
                quote! {
                    offsets[#i1] =
                        (offsets[#i0]+sizes[#i0])
                        + (::std::mem::align_of::<#field_ty>() -
                                (offsets[#i0]+sizes[#i0])
                                    % ::std::mem::align_of::<#field_ty>())
                          % ::std::mem::align_of::<#field_ty>();
                    sizes[#i1] = ::std::mem::size_of::<#field_ty>();
                }
            }
        })
        .collect();

    let offset_size_array_elems = fields.iter().enumerate().map(|(i, _f)| {
        quote! { (offsets[#i], sizes[#i]) }
    });

    let num_fields = fields.len();

    let layout_const_fn = quote! {
        const fn layout() -> [(usize, usize); #num_fields] {
            let mut offsets = [0usize; #num_fields];
            let mut sizes = [0usize; #num_fields];
            #(#compute_offset_size_stmts)*
            [ #(#offset_size_array_elems,)* ]
        }
    };

    Ok(layout_const_fn)
}

/// Adds a bound `T: HirType` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(#CRATE::utils::HirType));
        }
    }
    generics
}

/// Derives `HirType`
pub fn derive(input: proc_macro::TokenStream) -> TokenStream {
    let derive_input: syn::DeriveInput = match syn::parse(input) {
        Ok(input) => input,
        Err(e) => return e.into_compile_error(),
    };

    // verify that the input is a struct
    let fields = match derive_input.data {
        syn::Data::Struct(ref struct_data) => &struct_data.fields,
        _ => {
            return syn::Error::new(
                derive_input.span(),
                "`HirType` can only be automatically derived on structs",
            )
            .into_compile_error()
        }
    };

    // check for `#[repr(C)]`
    let repr_c_check = if !has_repr_c_attr(&derive_input) {
        syn::Error::new(
            derive_input.span(),
            format!("`HirType` can only be automatically derived on `repr(C)` structs"),
        )
        .into_compile_error()
    } else {
        quote! {}
    };

    // generate field offset constants
    let layout_const_fn = match generate_repr_c_layout_const_fn(&derive_input) {
        Ok(f) => f,
        Err(e) => return e.into_compile_error(),
    };

    // add the HirType bound on generic params
    let generics = add_trait_bounds(derive_input.generics);

    // generate code to describe each field in HIR
    let mut fields_desc = vec![];
    let mut fields_desc_idents = vec![];
    for (i, f) in fields.iter().enumerate() {
        let field_ty = &f.ty;
        let ident = f.ident.clone().unwrap_or(format_ident!("__{i}"));
        fields_desc.push(quote! {
            let #ident = #CRATE::hir::types::Field {
                ty: <#field_ty>::hir_repr(m),
                name: Some(std::stringify!(#ident).into()),
                offset: Some(Self::FIELDS[#i].0)
            };
        });
        fields_desc_idents.push(ident);
    }

    // generate impls
    let struct_name = &derive_input.ident;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        #repr_c_check

        // field layout
        impl #impl_generics #struct_name #ty_generics #where_clause {
            #layout_const_fn
            const FIELDS: &'static [(usize,usize)] = Self::layout().as_slice();
        }

        // trait impl
        impl #impl_generics #CRATE::utils::HirType for #struct_name #ty_generics #where_clause {
            fn hir_repr(m: &mut #CRATE::hir::Module) -> #CRATE::hir::Type {
                #(#fields_desc)*
                m.define_type(#CRATE::hir::TypeData::Struct(#CRATE::hir::types::StructType {
                    fields: vec![
                        #(#fields_desc_idents,)*
                    ].into(),
                    name: Some(std::stringify!(#struct_name).into())
                }))
            }
        }
    }
}
