//! Known attributes
use crate::{
    diagnostic::{AsSourceLocation, Diagnostics},
    hir,
    hir::{Interpolation, InterpolationKind, InterpolationSampling},
    session::AttributeRegistration,
    syntax::{
        ast,
        ast::{AstNode, AttrItem, Expr, LiteralKind},
        SyntaxNode,
    },
    tast::{layout::StdLayoutRules, ParseFrom},
    utils::write_list,
};
use bitflags::{bitflags, Flags};
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    marker::PhantomData,
    num::ParseIntError,
};

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Error parsing an attribute.
///
/// TODO replace by a unit struct? callers don't care about why parsing failed, since the failure is
/// already reported to the user with diagnostics
#[derive(thiserror::Error, Debug, Copy, Clone)]
pub enum AttrParseError {
    #[error("syntax error in attribute")]
    SyntaxError,
    #[error("not enough arguments")]
    NotEnoughArguments,
}

impl<T: ParseFrom<AttrItem>> ParseFrom<Option<AttrItem>> for Option<T> {
    fn parse_from(value: Option<AttrItem>, diag: &mut Diagnostics) -> Result<Self, ()> {
        match value {
            None => Ok(None),
            Some(item) => T::parse_from(item, diag).map(Some),
        }
    }
}

impl<T: ParseFrom<AttrItem>> ParseFrom<Option<AttrItem>> for T {
    fn parse_from(value: Option<AttrItem>, diag: &mut Diagnostics) -> Result<Self, ()> {
        match value {
            None => {
                // FIXME this is not the right place for emitting this diagnostic since we have no location
                diag.error("missing attribute parameter").emit();
                return Err(());
            }
            Some(item) => T::parse_from(item, diag),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

bitflags! {
    /// Describes the items to which an attribute may apply.
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct AttributeTarget: u32 {
        const FUNCTION = (1 << 0);
        const MEMBER = (1 << 1);
        const GLOBAL = (1 << 2);
        const ARGUMENT = (1 << 3);
        const RETURN_VALUE = (1 << 4);
        const STRUCTURE = (1 << 5);
    }
}

/// How many times an attribute may appear on an item.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum AttributeMultiplicity {
    /// Attribute cannot appear more than once.
    Single,
    /// Attribute may appear more than once.
    Multi,
}

/// Describes the type of an attribute.
pub trait Attribute: ParseFrom<ast::Attribute> {}

pub(crate) trait AttributeChecker {
    //fn name(&self) -> &str;
    //fn targets(&self) -> AttributeTarget;
    fn check(&self, ast: &ast::Attribute, diag: &mut Diagnostics) -> Result<(), ()>;
}

pub(crate) struct AttributeCheckerImpl<T: Attribute>(pub(crate) PhantomData<fn() -> T>);

impl<T: Attribute> AttributeChecker for AttributeCheckerImpl<T> {
    fn check(&self, ast: &ast::Attribute, diag: &mut Diagnostics) -> Result<(), ()> {
        T::parse_from(ast.clone(), diag).map(|_| ())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl ParseFrom<AttrItem> for StdLayoutRules {
    fn parse_from(item: ast::AttrItem, diag: &mut Diagnostics) -> Result<Self, ()> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(())?;
                match ident.text() {
                    "std140" => Ok(StdLayoutRules::Std140),
                    "std430" => Ok(StdLayoutRules::Std430),
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
    }
}

impl ParseFrom<AttrItem> for InterpolationKind {
    fn parse_from(item: ast::AttrItem, diag: &mut Diagnostics) -> Result<Self, ()> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(())?;
                match ident.text() {
                    "flat" => Ok(InterpolationKind::Flat),
                    "smooth" => Ok(InterpolationKind::Smooth),
                    "noperspective" => Ok(InterpolationKind::NoPerspective),
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
    }
}

impl ParseFrom<AttrItem> for InterpolationSampling {
    fn parse_from(item: AttrItem, diag: &mut Diagnostics) -> Result<Self, ()> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(())?;
                match ident.text() {
                    "center" => Ok(InterpolationSampling::Center),
                    "sample" => Ok(InterpolationSampling::Sample),
                    "centroid" => Ok(InterpolationSampling::Centroid),
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
    }
}

/// Parse u32 from attribute argument.
impl ParseFrom<AttrItem> for u32 {
    fn parse_from(item: AttrItem, diag: &mut Diagnostics) -> Result<Self, ()> {
        match item {
            AttrItem::AttrLiteral(lit) => match lit.expr() {
                Some(lit) => match lit.kind() {
                    LiteralKind::IntNumber(value) => match value.value() {
                        Ok(value) => match u32::try_from(value) {
                            Ok(value) => Ok(value),
                            Err(err) => {
                                diag.error(format!("invalid integer constant: {err}"))
                                    .location(lit)
                                    .emit();
                                Err(())
                            }
                        },
                        Err(err) => {
                            diag.error("invalid integer constant").location(lit).emit();
                            Err(())
                        }
                    },
                    _ => {
                        diag.error("expected integer literal").location(lit).emit();
                        Err(())
                    }
                },
                _ => {
                    diag.bug("expected integer literal").location(lit).emit();
                    Err(())
                }
            },
            _ => {
                diag.error("syntax error").location(item).emit();
                Err(())
            }
        }
    }
}

/// Macro to define a parseable attribute.
// TODO expose this as a public API
// TODO use a proc-macro instead? derive attribute parsing from enum type
macro_rules! define_attribute {
    ($(#[$m:meta])* $v:vis $struct_name:ident ( $($param_vis:vis $param_name:ident: $param_ty:ty),* )  ) => {
        $(#[$m])*
        $v struct $struct_name { $($param_vis $param_name: $param_ty),* }

        impl ParseFrom<ast::Attribute> for $struct_name {
            fn parse_from(attr: ast::Attribute, diag: &mut Diagnostics) -> Result<Self, ()> {
                let attr_name = std::stringify!($struct_name);
                /*if attr.name().ok_or(AttrParseError::SyntaxError)?.text() != attr_name {
                    diag.error(format!("expected `@{}` attribute", attr_name))
                        .location(attr)
                        .emit();
                    return Err(AttrParseError::SyntaxError);
                }*/

                let mut args = attr.args().into_iter().map(|args| args.args()).flatten();

                $(let $param_name : $param_ty = ParseFrom::parse_from(args.next(), diag)?;)*
                if let Some(arg) = args.next() {
                    diag.error(format!("extra arguments on `@{}` attribute", attr_name))
                        .location(arg)
                        .emit();
                    return Err(());
                }
                Ok($struct_name {
                    $($param_name),*
                })
            }
        }
    };
}

#[derive(Clone, Debug)]
pub enum KnownAttributeKind {
    Interpolate {
        kind: hir::InterpolationKind,
        sampling: Option<hir::InterpolationSampling>,
    },
    Location(u32),
    Offset(u32),
    Align(u32),
    Position,
    PointSize,
    Vertex,
    Fragment,
}

fn diag_attribute_invalid_for_target(
    diag: &mut Diagnostics,
    name: &str,
    attr: &ast::Attribute,
    target: AttributeTarget,
    attr_valid_targets: AttributeTarget,
) {
    let target_desc = match target {
        AttributeTarget::MEMBER => "members",
        AttributeTarget::FUNCTION => "functions",
        AttributeTarget::RETURN_VALUE => "return values",
        AttributeTarget::GLOBAL => "globals",
        AttributeTarget::ARGUMENT => "arguments",
        AttributeTarget::STRUCTURE => "structures",
        _ => panic!("invalid target"),
    };

    let mut valid_targets = vec![];
    if attr_valid_targets.contains(AttributeTarget::MEMBER) {
        valid_targets.push("members");
    }
    if attr_valid_targets.contains(AttributeTarget::FUNCTION) {
        valid_targets.push("functions");
    }
    if attr_valid_targets.contains(AttributeTarget::GLOBAL) {
        valid_targets.push("global variables");
    }
    if attr_valid_targets.contains(AttributeTarget::ARGUMENT) {
        valid_targets.push("arguments");
    }
    if attr_valid_targets.contains(AttributeTarget::RETURN_VALUE) {
        valid_targets.push("arguments");
    }
    if attr_valid_targets.contains(AttributeTarget::STRUCTURE) {
        valid_targets.push("structures");
    }

    let mut valid_targets_desc = String::new();
    write_list!(valid_targets_desc, t in valid_targets => {
         write!(valid_targets_desc, "{}", t).unwrap();
    });

    diag.error(format!("attribute `{name}` cannot be used on {target_desc}"))
        .location(&attr)
        .note(format!(
            "attribute can be used on the following targets: {valid_targets_desc}"
        ))
        .emit()
}

fn diag_attribute_appears_more_than_once(diag: &mut Diagnostics, name: &str, attr: &ast::Attribute) {
    diag.error(format!("attribute `{name}` appears more than once"))
        .location(&attr)
        .emit()
}

fn diag_attribute_mutually_exclusive_conflict(diag: &mut Diagnostics, name: &str, other: &str, attr: &ast::Attribute) {
    // TODO better error message
    diag.error(format!("attribute `{name}` cannot appear alongside `{other}`"))
        .location(&attr)
        .emit()
}

macro_rules! impl_known_attributes {
    ($($attr_name:ident [$attr_valid_targets:expr] $([$($mutually_exclusive:ident),*])? $multiplicity:ident $(( $($param_name:ident: $param_ty:ty),* ))? => $e:expr; )*) => {
        fn parse_known_attribute(name: &str, attr: &ast::Attribute, target: AttributeTarget, already_seen: &mut HashSet<String>, diag: &mut Diagnostics) -> Result<Option<KnownAttributeKind>, ()>
        {
            $( define_attribute!{ #[allow(non_camel_case_types)] $attr_name $( ( $($param_name: $param_ty),* ) )? } )*
            match name {
                $( std::stringify!($attr_name) => {
                    let attr_name_str = std::stringify!($attr_name);
                    if !$attr_valid_targets.contains(target) {
                        diag_attribute_invalid_for_target(diag, attr_name_str, attr, target, $attr_valid_targets);
                    }
                    if AttributeMultiplicity::$multiplicity == AttributeMultiplicity::Multi && already_seen.contains(attr_name_str) {
                        diag_attribute_appears_more_than_once(diag, attr_name_str, attr);
                    }
                    $(for excl in [$(std::stringify!($mutually_exclusive)),*] {
                        if already_seen.contains(excl) {
                            diag_attribute_mutually_exclusive_conflict(diag, attr_name_str, excl, attr);
                        }
                    })?

                    already_seen.insert(attr_name_str.to_string());
                    match $attr_name::parse_from(attr.clone(), diag) {
                        Ok($attr_name $( { $($param_name),* })?) => { Ok(Some($e)) },
                        Err(err) => {
                            Err(err)
                        },
                    }
                } )*
                _ => {
                    Ok(None)
                }
            }
        }
    };
}

// Short-hands for impl_known_attributes
const M: AttributeTarget = AttributeTarget::MEMBER;
const G: AttributeTarget = AttributeTarget::GLOBAL;
const A: AttributeTarget = AttributeTarget::ARGUMENT;
const R: AttributeTarget = AttributeTarget::RETURN_VALUE;
const F: AttributeTarget = AttributeTarget::FUNCTION;

impl_known_attributes!(
     interpolate [M | G | A | R] []         Single (kind: hir::InterpolationKind, sampling: Option<hir::InterpolationSampling>) => KnownAttributeKind::Interpolate { kind, sampling };
     location    [M | G | A | R] []         Single (location: u32) => KnownAttributeKind::Location(location);
     offset      [M]             []         Single (offset: u32)   => KnownAttributeKind::Offset(offset);
     align       [M]             []         Single (align: u32)    => KnownAttributeKind::Align(align);
     position    [M | G | A | R] []         Single ()              => KnownAttributeKind::Position;
     point_size  [M | G | A]     []         Single ()              => KnownAttributeKind::PointSize;
     vertex      [F]             [fragment] Single ()              => KnownAttributeKind::Vertex;
     fragment    [F]             [vertex]   Single ()              => KnownAttributeKind::Fragment;
);

pub(crate) struct KnownAttribute {
    pub(crate) syntax: Option<SyntaxNode>,
    pub(crate) kind: KnownAttributeKind,
}

pub(crate) fn check_attributes(
    custom_attributes: &HashMap<String, AttributeRegistration>,
    target: AttributeTarget,
    attrs: impl Iterator<Item = ast::Attribute>,
    diag: &mut Diagnostics,
) -> Vec<KnownAttribute> {
    let mut known = vec![];
    let mut already_seen = HashSet::new();

    for attr in attrs {
        let Some(ident) = attr.name() else {
            diag.bug("syntax error").location(&attr).emit();
            continue
        };
        let name = ident.text();
        match parse_known_attribute(name, &attr, target, &mut already_seen, diag) {
            Ok(Some(kind)) => {
                // well-known attribute
                known.push(KnownAttribute {
                    syntax: Some(attr.syntax().clone()),
                    kind,
                });
            }
            Ok(None) => {
                // possibly a custom attribute
                match custom_attributes.get(name) {
                    Some(reg) => {
                        // yes, check its allowed targets and syntax
                        if !reg.valid_targets.contains(target) {
                            diag_attribute_invalid_for_target(diag, name, &attr, target, reg.valid_targets);
                        }
                        if reg.multiplicity == AttributeMultiplicity::Single && already_seen.contains(name) {
                            diag_attribute_appears_more_than_once(diag, name, &attr);
                        }
                        already_seen.insert(name.to_string());
                        // the checker is free to emit a more precise diagnostic
                        match reg.checker.check(&attr, diag) {
                            Ok(()) => {}
                            Err(()) => diag.error("parsing custom attribute failed").location(&attr).emit(),
                        }
                    }
                    None => diag.error(format!("unknown attribute `{name}`")).location(&attr).emit(),
                }
            }
            Err(()) => {}
        }
    }

    known
}
