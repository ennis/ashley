//! Known attributes
use crate::{
    diagnostic::{AsSourceLocation, Diagnostics},
    hir,
    hir::{Interpolation, InterpolationKind, InterpolationSampling},
    syntax::{
        ast,
        ast::{AstNode, AttrItem, Attribute, Expr, LiteralKind},
        SyntaxNode,
    },
    tast::layout::StdLayoutRules,
};
use bitflags::bitflags;
use std::num::ParseIntError;

////////////////////////////////////////////////////////////////////////////////////////////////////

/// Error parsing an attribute.
#[derive(thiserror::Error, Debug, Copy, Clone)]
pub enum AttrParseError {
    #[error("syntax error in attribute")]
    SyntaxError,
    #[error("not enough arguments")]
    NotEnoughArguments,
}

/// Utility trait to parse a value with access to the diagnostics output.
trait ParseFrom<T>: Sized {
    type Error;

    fn parse_from(value: T, diag: &mut Diagnostics) -> Result<Self, Self::Error>;
}

impl<T: ParseFrom<AttrItem, Error = AttrParseError>> ParseFrom<Option<AttrItem>> for Option<T> {
    type Error = AttrParseError;

    fn parse_from(value: Option<AttrItem>, diag: &mut Diagnostics) -> Result<Self, AttrParseError> {
        match value {
            None => Ok(None),
            Some(item) => T::parse_from(item, diag).map(Some),
        }
    }
}

impl<T: ParseFrom<AttrItem, Error = AttrParseError>> ParseFrom<Option<AttrItem>> for T {
    type Error = AttrParseError;

    fn parse_from(value: Option<AttrItem>, diag: &mut Diagnostics) -> Result<Self, AttrParseError> {
        match value {
            None => return Err(AttrParseError::NotEnoughArguments),
            Some(item) => T::parse_from(item, diag),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////

impl ParseFrom<AttrItem> for StdLayoutRules {
    type Error = AttrParseError;

    fn parse_from(item: ast::AttrItem, diag: &mut Diagnostics) -> Result<Self, Self::Error> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(AttrParseError::SyntaxError)?;
                match ident.text() {
                    "std140" => Ok(StdLayoutRules::Std140),
                    "std430" => Ok(StdLayoutRules::Std430),
                    _ => Err(AttrParseError::SyntaxError),
                }
            }
            _ => Err(AttrParseError::SyntaxError),
        }
    }
}

impl ParseFrom<AttrItem> for InterpolationKind {
    type Error = AttrParseError;

    fn parse_from(item: ast::AttrItem, diag: &mut Diagnostics) -> Result<Self, AttrParseError> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(AttrParseError::SyntaxError)?;
                match ident.text() {
                    "flat" => Ok(InterpolationKind::Flat),
                    "smooth" => Ok(InterpolationKind::Smooth),
                    "noperspective" => Ok(InterpolationKind::NoPerspective),
                    _ => Err(AttrParseError::SyntaxError),
                }
            }
            _ => Err(AttrParseError::SyntaxError),
        }
    }
}

impl ParseFrom<AttrItem> for InterpolationSampling {
    type Error = AttrParseError;

    fn parse_from(item: AttrItem, diag: &mut Diagnostics) -> Result<Self, AttrParseError> {
        match item {
            AttrItem::AttrIdent(ident) => {
                let ident = ident.ident().ok_or(AttrParseError::SyntaxError)?;
                match ident.text() {
                    "center" => Ok(InterpolationSampling::Center),
                    "sample" => Ok(InterpolationSampling::Sample),
                    "centroid" => Ok(InterpolationSampling::Centroid),
                    _ => Err(AttrParseError::SyntaxError),
                }
            }
            _ => Err(AttrParseError::SyntaxError),
        }
    }
}

/// Parse u32 from attribute argument.
impl ParseFrom<AttrItem> for u32 {
    type Error = AttrParseError;

    fn parse_from(item: AttrItem, diag: &mut Diagnostics) -> Result<Self, AttrParseError> {
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
                                Err(AttrParseError::SyntaxError)
                            }
                        },
                        Err(err) => {
                            diag.error("invalid integer constant").location(lit).emit();
                            Err(AttrParseError::SyntaxError)
                        }
                    },
                    _ => {
                        diag.error("expected integer literal").location(lit).emit();
                        Err(AttrParseError::SyntaxError)
                    }
                },
                _ => {
                    diag.error("expected integer literal").location(lit).emit();
                    Err(AttrParseError::SyntaxError)
                }
            },
            _ => {
                diag.error("syntax error").location(item).emit();
                Err(AttrParseError::SyntaxError)
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

        impl ParseFrom<Attribute> for $struct_name {
            type Error = AttrParseError;
            fn parse_from(attr: ast::Attribute, diag: &mut Diagnostics) -> Result<Self, Self::Error> {
                let attr_name = std::stringify!($attr_name);
                if attr.name().ok_or(AttrParseError::SyntaxError)?.text() != attr_name {
                    diag.error(format!("expected `@{}` attribute", attr_name))
                        .location(attr)
                        .emit();
                    return Err(AttrParseError::SyntaxError);
                }

                let mut args = attr.args();
                $(let mut $param_name : $param_ty = ParseFrom::parse_from(args.next(), diag)?;)*
                if let Some(arg) = args.next() {
                    diag.error(format!("extra arguments on `@{}` attribute", attr_name))
                        .location(arg)
                        .emit();
                    return Err(AttrParseError::SyntaxError);
                }
                Ok($struct_name {
                    $($param_name),*
                })
            }
        }
    };
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct AttributeTarget: u32 {
        const FUNCTION = (1 << 0);
        const MEMBER = (1 << 1);
        const ARGUMENT = (1 << 2);
        const STRUCTURE = (1 << 3);
    }
}

#[derive(Clone, Debug)]
pub enum KnownAttributeKind {
    //#[attribute(interpolate)]
    Interpolate {
        kind: hir::InterpolationKind,
        sampling: Option<hir::InterpolationSampling>,
    },
    //#[attribute(location)]
    Location(u32),
    //#[attribute(offset)]
    Offset(u32),
    //#[attribute(align)]
    Align(u32),
    //#[attribute(position)]
    Position,
    //#[attribute(point_size)]
    PointSize,
}

macro_rules! parse_known_attributes {
    ($attr:expr, $diag:expr; $($attr_name:ident $(( $($param_name:ident: $param_ty:ty),* ))? => $e:expr; )*) => {
        {
            $( define_attribute!{ $attr_name $( ( $($param_name: $param_ty),* ) )? } )*
            match $attr.name() {
                Some(ident) => match ident.text() {
                    $( std::stringify!($attr_name) => {
                        match $attr_name::parse_from($attr, $diag) {
                            Ok($attr_name $( { $($param_name),* })?) => { Ok(Some($e)) },
                            Err(err) => { Err(err) },
                        }
                    } )*
                    _ => {
                        Ok(None)
                    }
                },
                None => {
                    $diag.bug("syntax error").location($attr).emit();
                    Err(AttrParseError::SyntaxError)
                }
            }
        }
    };
}

pub(crate) fn parse_attributes(
    attrs: impl Iterator<Item = Attribute>,
    target: AttributeTarget,
    diag: &mut Diagnostics,
) -> (Vec<KnownAttribute>, Vec<Attribute>) {
    let mut known = vec![];
    let mut unknown = vec![];

    for attr in attrs {
        let r = parse_known_attributes!(attr.clone(), diag;
             interpolate(kind: hir::InterpolationKind, sampling: Option<hir::InterpolationSampling>) => KnownAttributeKind::Interpolate { kind, sampling };
             location(location: u32) => KnownAttributeKind::Location(location);
             offset(offset: u32) => KnownAttributeKind::Offset(offset);
             align(align: u32) => KnownAttributeKind::Align(align);
             position() => KnownAttributeKind::Position;
             point_size() => KnownAttributeKind::PointSize;
        );
        match r {
            Ok(Some(kind)) => {
                known.push(KnownAttribute {
                    syntax: Some(attr.syntax().clone()),
                    kind,
                });
            }
            Ok(None) => {
                unknown.push(attr);
            }
            _ => {}
        }
    }

    (known, unknown)
}

pub struct KnownAttribute {
    pub syntax: Option<SyntaxNode>,
    pub kind: KnownAttributeKind,
}
