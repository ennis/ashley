use crate::CRATE;
use proc_macro::{Diagnostic, Level};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use std::collections::HashSet;
use syn::{braced, bracketed, parenthesized, parse::{Parse, ParseStream}, parse_macro_input, parse_quote, punctuated::Punctuated, spanned::Spanned, token, token::{Brace, Paren, Token}, AngleBracketedGenericArguments, Block, Data, GenericParam, Generics, Ident, LitStr, ParenthesizedGenericArguments, Pat, PatIdent, PatTuple, PatWild, Path, Token, ExprClosure};

struct GenCtxt {
    bound_vars: HashSet<Ident>,
}

//--------------------------------------------------------------------------------------------------

enum AttributeConstraint {
    Type(syn::Type),
    Value(syn::Expr)
}

/// An attribute pattern.
///
/// Used to constrain the type of an attribute `attribute: Type`, or both its type and its value:
/// `attribute = expr`
struct AttributePattern {
    name: Ident,
    constraint: Option<AttributeConstraint>
}

impl Parse for AttributePattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

//--------------------------------------------------------------------------------------------------


/// A variable name or a placeholder.
///
/// # Examples
/// `result`, `T`, `_`
enum VariableOrPlaceholder {
    Ident(Ident),
    Placeholder(Token![_]),
}

impl ToTokens for VariableOrPlaceholder {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            VariableOrPlaceholder::Ident(ident) => ident.to_tokens(tokens),
            VariableOrPlaceholder::Placeholder(wild) => wild.to_tokens(tokens),
        }
    }
}

/// A item in a slice pattern.
///
/// # Examples
///
/// * bind an element in the slice to a variable named `result`: `result`
/// * same but also match a pattern: `result: Pattern`
/// * match a pattern on one element w/o binding it to a variable: `_:Pattern`
/// * bind the rest of the elements in the slice to a variable named `operands`: `..operands`
/// * matches the rest of the elements in the slice: `..`
enum SliceElementPattern {
    /// `ident:Pattern`
    Element(BoundPattern),
    /// `..ident` or `..`
    Rest(Option<Ident>),
}

impl Parse for SliceElementPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![..]) {
            let _: Token![..] = input.parse()?;
            if input.peek(Ident) {
                let ident: Ident = input.parse()?;
                Ok(SliceElementPattern::Rest(Some(ident)))
            } else {
                Ok(SliceElementPattern::Rest(None))
            }
        } else {
            let p = input.parse()?;
            Ok(SliceElementPattern::Element(p))
        }
    }
}


struct OperationPattern {
    mnemonic: syn::LitStr,
    attributes: Option<Punctuated<SliceElementPattern, Token![,]>>,
    operands: Option<Punctuated<SliceElementPattern, Token![,]>>,
    regions: Option<Punctuated<SliceElementPattern, Token![,]>>,
    results: Option<Punctuated<SliceElementPattern, Token![,]>>,
}

impl Parse for OperationPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mnemonic = input.parse()?;
        let attributes = if input.peek(Token![<]) {
            let _: Token![<] = input.parse()?;
            let mut attributes = Punctuated::new();
            loop {
                if input.peek(Token![>]) {
                    break;
                }
                let value = input.parse()?;
                attributes.push_value(value);
                if input.peek(Token![>]) {
                    break;
                }
                let punct = input.parse()?;
                attributes.push_punct(punct);
            }
            Some(attributes)
        } else {
            None
        };
        let operands = if input.peek(Paren) {
            let contents;
            parenthesized!(contents in input);
            Some(Punctuated::parse_terminated(&contents)?)
        } else {
            None
        };
        let regions = if input.peek(Brace) {
            let contents;
            braced!(contents in input);
            Some(Punctuated::parse_terminated(&contents)?)
        } else {
            None
        };
        let results_arrow: Option<Token![->]> = input.parse()?;
        let results = if results_arrow.is_some() {
            let contents;
            parenthesized!(contents in input);
            Some(Punctuated::parse_terminated(&contents)?)
        } else {
            None
        };

        Ok(OperationPattern {
            mnemonic,
            attributes,
            operands,
            regions,
            results,
        })
    }
}

fn generate_slice_patterns(
    ctxt: &mut GenCtxt,
    list: &Option<Punctuated<SliceElementPattern, Token![,]>>,
    scrutinee: &Ident,
    item_kind: &str,
) -> TokenStream {
    let mut slice_patterns = vec![];
    let mut inner_patterns = vec![];
    if let Some(list) = list {
        for (i, p) in list.iter().enumerate() {
            let tmp_var = format_ident!("{item_kind}__{i}");
            match p {
                SliceElementPattern::Element(p) => {
                    slice_patterns.push(quote!(#tmp_var));
                    inner_patterns.push(p.generate(ctxt, &tmp_var));
                }
                SliceElementPattern::Rest(ident) => {
                    slice_patterns.push(quote!(ref #ident @ ..));
                    // TODO variadic operand patterns
                }
            }
        }

        let method = format_ident!("{}", item_kind);
        quote! {
            let &[#(#slice_patterns),*] = #scrutinee.#method(b__) else { return b__.failure(::std::concat!(#item_kind, " pattern mismatch")); };
            #(#inner_patterns)*
        }
    } else {
        quote!()
    }
}

impl OperationPattern {
    /// Returns the minimum number of operands expected by the operation pattern.
    ///
    /// If the pattern has no variadic operands, then this is the exact number of operands that the operation should have.
    fn minimum_operand_count(&self) -> usize {
        self.operands
            .iter()
            .flatten()
            .filter(|&op| matches!(op, SliceElementPattern::Element(_)))
            .count()
    }

    fn is_variadic(&self) -> bool {
        self.operands
            .iter()
            .flatten()
            .any(|op| matches!(op, SliceElementPattern::Rest(_)))
    }

    fn generate(&self, ctxt: &mut GenCtxt, scrutinee: &Ident, bound_to: &VariableOrPlaceholder) -> TokenStream {
        let mnemonic = self.mnemonic.value();

        let check_mnemonic = if mnemonic.is_empty() {
            quote! {}
        } else {
            quote! {
                if #scrutinee.mnemonic(b__) != #mnemonic {
                    return b__.failure("opcode mismatch");
                }
            }
        };

        let min_num_operands = self.minimum_operand_count();
        let check_num_operands = if self.is_variadic() {
            quote! {
                if #scrutinee.operands(b__).len() != #min_num_operands {
                    return b__.failure("operand count mismatch");
                }
            }
        } else {
            // TODO same code? used to be different error messages though
            quote! {
                if #scrutinee.operands(b__).len() != #min_num_operands {
                    return b__.failure("operand count mismatch");
                }
            }
        };

        let attributes_section = generate_slice_patterns(ctxt, &self.attributes, scrutinee, "attributes");
        let operands_section = generate_slice_patterns(ctxt, &self.operands, scrutinee, "operands");
        let regions_section = generate_slice_patterns(ctxt, &self.regions, scrutinee, "regions");
        let results_section = generate_slice_patterns(ctxt, &self.results, scrutinee, "results");

        // attributes are treated differently from operands and results:
        // we don't match a pattern,

        quote! {
            #check_mnemonic
            #check_num_operands
            #attributes_section
            #operands_section
            #regions_section
            #results_section
        }
    }
}

//--------------------------------------------------------------------------------------------------

/// Struct-like patterns.
///
/// There are three kinds of struct-like patterns, similar to rust structs:
/// * unit patterns: `Pattern`, `GenericPattern::<T>`
/// * tuple patterns: `Pattern(a,b,c)`, `Pattern(_,x,_)`
/// * named-field patterns: `Pattern { lhs = a, rhs = b, .. }`
///
/// # Example
/// * named-field pattern with subpatterns: `BinaryOperation { lhs = lhs : Integer, rhs = rhs : Constant, result : Integer, .. }`
struct StructLikePattern {
    path: Path,
    field_names: Vec<syn::Member>,
    patterns: Punctuated<BoundPattern, Token![,]>,
}

impl Parse for StructLikePattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        if input.peek(Paren) {
            let content;
            parenthesized!(content in input);
            let patterns = Punctuated::parse_terminated(&content)?;
            let field_names = patterns
                .iter()
                .enumerate()
                .map(|(i, _)| syn::Member::from(i))
                .collect::<Vec<_>>();
            Ok(StructLikePattern {
                path,
                field_names,
                patterns,
            })
        } else if input.peek(Brace) {
            let content;
            braced!(content in input);

            let mut field_names = vec![];
            let mut patterns = Punctuated::new();
            while !content.is_empty() {
                let lookahead = content.lookahead1();
                if lookahead.peek(Ident) {
                    let member = content.parse()?;
                    let _eq: Token![=] = content.parse()?;
                    let bound_pattern = content.parse()?;
                    field_names.push(member);
                    patterns.push(bound_pattern);
                } else if lookahead.peek(Token![..]) {
                    let _: Token![..] = content.parse()?;
                    if !content.is_empty() {
                        return Err(content.error("unexpected tokens following `..`"));
                    }
                } else {
                    return Err(lookahead.error());
                }

                if !content.is_empty() {
                    let _: Token![,] = content.parse()?;
                }
            }

            Ok(StructLikePattern {
                path,
                field_names,
                patterns,
            })
        } else {
            Ok(StructLikePattern {
                path,
                patterns: Default::default(),
                field_names: Default::default(),
            })
        }
    }
}

impl StructLikePattern {
    fn generate(&self, ctxt: &mut GenCtxt, scrutinee: &Ident, bound_to: &VariableOrPlaceholder) -> TokenStream {
        let path = &self.path;
        let tmp_bindings = self
            .patterns
            .iter()
            .enumerate()
            .map(|(i, f)| format_ident!("pat__{}", i))
            .collect::<Vec<_>>();

        let match_stmt = if !tmp_bindings.is_empty() {
            let tmp_bindings = &tmp_bindings;
            let field_names = &self.field_names;
            quote! {
                let Some(#path { #(#field_names: #tmp_bindings),* } ) = #scrutinee.pmatch(b__) else {
                    return b__.failure(::std::concat!("failed to match pattern `", ::std::stringify!(#bound_to: #path(#(#tmp_bindings),*)), "`"));
                };
            }
        } else {
            quote! {
                let Some(#path { .. }) = #scrutinee.pmatch(b__) else {
                    return b__.failure(::std::concat!("failed to match pattern `", ::std::stringify!(#bound_to: #path), "`"));
                };
            }
        };

        let subpattern_tokens = self
            .patterns
            .iter()
            .zip(tmp_bindings.iter())
            .map(|(f, v)| f.generate(ctxt, v));

        quote! {
            #match_stmt
            #(#subpattern_tokens)*
        }
    }
}

//--------------------------------------------------------------------------------------------------


/// Patterns.
enum Pattern {
    /// An operation pattern.
    ///
    /// See `OperationPattern`.
    Operation(OperationPattern),
    /// A struct-like pattern.
    StructLike(StructLikePattern),
    /// A pattern of the form `!BoundPattern`, short-hand for `ValueType(BoundPattern)`
    ValueOfType(Box<BoundPattern>),
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) {
            Ok(Pattern::StructLike(input.parse()?))
        } else if lookahead.peek(LitStr) {
            Ok(Pattern::Operation(input.parse()?))
        } else if lookahead.peek(Token![!]) {
            let _: Token![!] = input.parse()?;
            Ok(Pattern::ValueOfType(Box::new(input.parse()?)))
        } else {
            Err(lookahead.error())
        }
    }
}

impl Pattern {
    /// Generates the code to match this pattern.
    /// `bound_to` is for debugging purposes only, and should be the variable of the `BoundPattern` that owns this pattern.
    ///
    /// TODO: more debugging info on the scrutinee (path to the field in nested patterns)
    fn generate(&self, ctxt: &mut GenCtxt, scrutinee: &Ident, bound_to: &VariableOrPlaceholder) -> TokenStream {
        match self {
            Pattern::Operation(p) => p.generate(ctxt, scrutinee, bound_to),
            Pattern::ValueOfType(p) => {
                let type_pattern_tokens = p.generate(ctxt, &format_ident!("ty__0"));
                quote! {
                    let Some(#CRATE::hir::constraint::ValueType(ty__0)) = #scrutinee.pmatch(b__) else {
                        return b__.failure(::std::concat!("failed to constrain value type on `", ::std::stringify!(#bound_to) ,"`"));
                    };
                    #type_pattern_tokens
                }
            }
            Pattern::StructLike(p) => p.generate(ctxt, scrutinee, bound_to),
        }
    }
}


/// Represents a variable bound to a pattern, generally of the form `variable : Pattern`.
///
/// The pattern is optional. If the scrutinee doesn't need to be bound to a variable,
/// it's possible to use a placeholder instead (`_`).
///
/// # Examples
/// * bind to a variable without a pattern: `result`
/// * bind to a variable and match a pattern: `lhs : ArithmeticType`
/// * with a placeholder instead: `_ : ArithmeticType`
struct BoundPattern {
    var: VariableOrPlaceholder,
    pattern: Option<Pattern>,
}

impl Parse for BoundPattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        let var = if lookahead.peek(Token![_]) {
            VariableOrPlaceholder::Placeholder(input.parse()?)
        } else if lookahead.peek(Ident) {
            VariableOrPlaceholder::Ident(input.parse()?)
        } else if lookahead.peek(LitStr) {
            // unbound operation pattern
            let var = VariableOrPlaceholder::Placeholder(parse_quote!(_));
            let op_pattern = input.parse()?;
            return Ok(BoundPattern {
                var,
                pattern: Some(Pattern::Operation(op_pattern)),
            });
        } else {
            return Err(lookahead.error());
        };

        let pattern = if input.peek(Token![:]) {
            // got a pattern
            let _: Token![:] = input.parse()?;
            Some(input.parse()?)
        } else {
            None
        };
        Ok(BoundPattern { var, pattern })
    }
}

impl BoundPattern {
    fn generate(&self, ctxt: &mut GenCtxt, scrutinee: &Ident) -> TokenStream {
        let pat_stmts = if let Some(ref pattern) = self.pattern {
            pattern.generate(ctxt, scrutinee, &self.var)
        } else {
            quote!()
        };

        if let VariableOrPlaceholder::Ident(ref var) = self.var {
            if ctxt.bound_vars.contains(&var) {
                quote! {
                    #pat_stmts
                    if #var != #scrutinee {
                        return b__.failure(::std::concat!("equality constraint failed on variable `", ::std::stringify!(#var), "`"));
                    }
                }
            } else {
                ctxt.bound_vars.insert(var.clone());
                quote! {
                    #pat_stmts
                    let #var = #scrutinee;
                }
            }
        } else {
            pat_stmts
        }
    }
}

//--------------------------------------------------------------------------------------------------
struct PatternWithWhereClause {
    pattern: BoundPattern,
}

impl Parse for PatternWithWhereClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pattern = input.parse()?;
        Ok(PatternWithWhereClause { pattern })
    }
}

impl PatternWithWhereClause {
    fn generate(&self, ctxt: &mut GenCtxt, matched: &Ident) -> TokenStream {
        self.pattern.generate(ctxt, matched)
    }
}

//--------------------------------------------------------------------------------------------------

enum WhereClauseItem {
    /// `var : <Pattern>`. Specifies that the variable must match the specified pattern constraint.
    Pattern { var: Ident, pat: Pattern },
    /// `<closure>`. Specifies a boolean predicate on pattern variables.
    Predicate(ExprClosure),
}

impl Parse for WhereClauseItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![|]) {
            Ok(WhereClauseItem::Predicate(input.parse()?))
        } else {
            let var = input.parse()?;
            let _: Token![:] = input.parse()?;
            let pat = input.parse()?;
            Ok(WhereClauseItem::Pattern { var, pat })
        }
    }
}

struct WhereClause {
    _where: Token![where],
    predicates: Punctuated<WhereClauseItem, Token![,]>,
}

impl Parse for WhereClause {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(WhereClause {
            _where: input.parse()?,
            predicates: Punctuated::parse_terminated(input)?,
        })
    }
}

//--------------------------------------------------------------------------------------------------
struct ConstraintBody {
    struct_name: Ident,
    captured_vars: Punctuated<Ident, Token![,]>,
    root_pattern: BoundPattern,
    where_clause: Option<WhereClause>,
}

impl Parse for ConstraintBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let struct_name = input.parse()?;
        let content;
        braced!(content in input);
        let captured_vars = Punctuated::parse_terminated(&content)?;
        let root_pattern = input.parse()?;
        let where_clause = if input.peek(Token![where]) {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(ConstraintBody {
            struct_name,
            captured_vars,
            root_pattern,
            where_clause,
        })
    }
}

impl ConstraintBody {
    fn generate(&self) -> TokenStream {
        let mut ctxt = GenCtxt {
            bound_vars: Default::default(),
        };
        let root_pattern_tokens = self.root_pattern.generate(&mut ctxt, &format_ident!("op__"));

        let mut pat_tokens = vec![];
        if let Some(ref where_clause) = self.where_clause {
            for where_pred in where_clause.predicates.iter() {
                match where_pred {
                    WhereClauseItem::Pattern { var, pat } => {
                        pat_tokens.push(pat.generate(&mut ctxt, var, &VariableOrPlaceholder::Ident(var.clone())));
                    }
                    WhereClauseItem::Predicate(closure) => {
                        pat_tokens.push(quote!{
                            let pred = #closure;
                            if !pred(b__) {
                                return b__.failure(::std::concat!("failed to match predicate: `", ::std::stringify!(#closure), "`"));
                            }
                        });
                    }
                }
            }
        }

        let struct_name = &self.struct_name;
        let captured_vars = self.captured_vars.iter();

        quote! {
            {
                #root_pattern_tokens
                #(#pat_tokens)*

                Some(#struct_name {
                    #(#captured_vars),*
                })
            }
        }
    }
}

pub(crate) fn operation_constraint_match_body_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let pattern_body = parse_macro_input!(input as ConstraintBody);

    pattern_body.generate().into()
}
