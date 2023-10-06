use crate::{
    def,
    def::{AstId, ConstExprId, ConstExprLoc, ParentScopeId, RawAstId},
    diagnostic::{Diagnostic, Severity},
    syntax::{ast, SyntaxNodePtr},
    utils::DynEq,
    CompilerDb,
};
use ashley_data_structures::Id;
use bitflags::bitflags;
use ordered_float::OrderedFloat;
use std::any::Any;

/// The syntax kind of an attribute argument.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgumentSyntaxKind<'a> {
    /// Type syntax (`SyntaxKind::TYPE`, like `int`, `float[16]`).
    Type,
    /// Expression syntax (`SyntaxKind::EXPR`).
    Expr,
    /// Identifier (`SyntaxKind::IDENT`).
    Ident,
    /// Integer literal (`SyntaxKind::INT_NUMBER`).
    IntNumber,
    /// Floating-point literal (`SyntaxKind::FLOAT_NUMBER`).
    FloatNumber,
    /// String literal (`SyntaxKind::STRING`).
    String,
    /// Nested attribute syntax (`SyntaxKind::ATTR_NESTED`).
    Nested(&'a [AttributeArgument]),
    // TODO: arbitrary tokens?
}

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AttributeArgument {
    /// If not `None`, this defines the key of a key-value argument. Otherwise this is a positional attribute.
    key: Option<&'static str>,
    /// Whether the argument is optional.
    ///
    /// Positional optional arguments must appear after all other positional arguments.
    optional: bool,
    /// Syntax kind.
    syntax: ArgumentSyntaxKind<'static>,
}

/// Describes the syntax of the arguments of an attribute.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct AttributeSyntax {
    args: &'static [AttributeArgument],
}

/// Represents an attribute.
///
/// It describes the syntax of its arguments.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Attribute {
    /// Name of the attribute.
    name: &'static str,
    /// The kinds of items on which this attribute may appear.
    target: AttributeTarget,
    /// Syntax of the attribute.
    syntax: AttributeSyntax,
    /// Whether the attribute can appear multiple times on an item.
    allow_multiple: bool,
}

macro_rules! attribute_syntax {

    (@parse_syntax_kind float) => { $crate::attribute::ArgumentSyntaxKind::FloatNumber };
    (@parse_syntax_kind int) => { $crate::attribute::ArgumentSyntaxKind::IntNumber };
    (@parse_syntax_kind ty) => { $crate::attribute::ArgumentSyntaxKind::Type };
    (@parse_syntax_kind ident) => { $crate::attribute::ArgumentSyntaxKind::Ident };
    (@parse_syntax_kind expr) => { $crate::attribute::ArgumentSyntaxKind::Expr };
    (@parse_syntax_kind string) => { $crate::attribute::ArgumentSyntaxKind::String };

    (@parse_args () ($($r:tt)*) ) => {
        &[$($r)*]
    };

    (@parse_args ($nested_attr:ident ($($nested_syntax:tt)*) ? $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: Some(std::stringify!($nested_attr)),
                optional: true,
                syntax: $crate::attribute::ArgumentSyntaxKind::Nested(attribute_syntax!(@parse_args ($($nested_syntax)*) ()))
            },)
        )
    };

    (@parse_args ($nested_attr:ident ($($nested_syntax:tt)*) $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: Some(std::stringify!($nested_attr)),
                optional: false,
                syntax: $crate::attribute::ArgumentSyntaxKind::Nested(attribute_syntax!(@parse_args ($($nested_syntax)*) ()))
            },)
        )
    };

    (@parse_args ($key:ident = $syntax:ident? $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: Some(std::stringify!($key)),
                optional: true,
                syntax: attribute_syntax!(@parse_syntax_kind $syntax)
            },)
        )
    };

    (@parse_args ($key:ident = $syntax:ident $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: Some(std::stringify!($key)),
                optional: false,
                syntax: attribute_syntax!(@parse_syntax_kind $syntax)
            },)
        )
    };

    (@parse_args ($syntax:ident ? $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: None,
                optional: true,
                syntax: attribute_syntax!(@parse_syntax_kind $syntax)
            },)
        )
    };

    (@parse_args ($syntax:ident $(, $($rest:tt)*)?) ($($r:tt)*) ) => {
        attribute_syntax!(@parse_args
            ($($($rest)*)?)
            ($($r)* $crate::attribute::AttributeArgument {
                key: None,
                optional: false,
                syntax: attribute_syntax!(@parse_syntax_kind $syntax)
            },)
        )
    };

    ($name:ident ( $($args:tt)* ) ) => {
        {
            static SYNTAX: $crate::attribute::AttributeSyntax = $crate::attribute::AttributeSyntax { args: attribute_syntax!(@parse_args ($($args)*) ()) };
            SYNTAX
        }
    };
}

static LOCATION_ATTRIBUTE: Attribute = Attribute {
    name: "location",
    target: AttributeTarget::from_bits_truncate(
        AttributeTarget::GLOBAL.bits()
            | AttributeTarget::MEMBER.bits()
            | AttributeTarget::RETURN_VALUE.bits()
            | AttributeTarget::ARGUMENT.bits(),
    ),
    syntax: attribute_syntax!(location(int)),
    allow_multiple: false,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AttributeArgumentNode {
    /// Type syntax (`SyntaxKind::TYPE`, like `int`, `float[16]`).
    Type(def::Type),
    /// Expression syntax (`SyntaxKind::EXPR`).
    Expr(ConstExprId),
    /// Identifier (`SyntaxKind::IDENT`).
    Ident { ast_id: AstId<ast::AttrItem>, text: String },
    /// Integer literal (`SyntaxKind::INT_NUMBER`).
    IntNumber { ast_id: AstId<ast::AttrItem>, value: i64 },
    /// Floating-point literal (`SyntaxKind::FLOAT_NUMBER`).
    FloatNumber {
        ast_id: AstId<ast::AttrItem>,
        value: OrderedFloat<f64>,
    },
    /// String literal (`SyntaxKind::STRING`).
    String { ast_id: AstId<ast::AttrItem>, text: String },
    /// Nested attribute syntax (`SyntaxKind::ATTR_NESTED`).
    Nested {
        ast_id: AstId<ast::AttrItem>,
        nodes: Box<[AttributeArgumentNode]>,
    },
}

/// A diagnostic that can be "resolved" to a generic `Diagnostic` object.
///
/// We don't emit `Diagnostic` objects directly in compiler queries, because
/// that would make them depend on the AST directly, and their results
/// would be invalidated on every trivial change to the source file.
///
/// Instead we store diagnostics that refer to source file locations via
/// stable `AstId`s, which are more-or-less stable across source file changes.
/// It's only when the diagnostic is to be displayed (or sent to a LSP client, etc.)
/// that those IDs are mapped back to concrete `Span`s (with `AstMap`s),
/// resulting in a concrete `Diagnostic`. This happens in the `resolve` method.
///
/// Note that most query diagnostics don't implement this trait, but they usually have a `resolve` method that
/// has a similar signature.
/// This trait is there specifically to allow type-erased diagnostics (`dyn ResolvableDiagnostics`)
/// to be emitted by extension points to the compiler (such as `AttributeVerifier`s).
///
/// TODO: actually use that somewhere?
pub trait ResolvableDiagnostic: Any + Eq + PartialEq + DynEq {
    fn render(&self, db: &dyn CompilerDb, parent_scope_id: ParentScopeId) -> Diagnostic;
    fn as_any(&self) -> &dyn Any;
}

/// Like `Diagnostic`, but represents location with an AstId, which makes it relative
/// to a parent scope.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GenericDiagnostic {
    raw_ast_id: Option<RawAstId>,
    severity: Severity,
    message: String,
    notes: Vec<String>,
}

impl GenericDiagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        GenericDiagnostic {
            raw_ast_id: None,
            severity: Severity::Error,
            message: message.into(),
            notes: vec![],
        }
    }

    pub fn warning(message: impl Into<String>) -> Self {
        GenericDiagnostic {
            raw_ast_id: None,
            severity: Severity::Warning,
            message: message.into(),
            notes: vec![],
        }
    }

    pub fn location(mut self, ast_id: Id<SyntaxNodePtr>) -> Self {
        self.raw_ast_id = Some(ast_id);
        self
    }

    pub fn note(mut self, message: impl Into<String>) -> Self {
        self.notes.push(message.into());
        self
    }
}

impl GenericDiagnostic {
    pub fn render(self, db: &dyn CompilerDb, parent_scope_id: ParentScopeId) -> Diagnostic {
        let span = self.raw_ast_id.map(|id| parent_scope_id.span(db, id));
        let mut diag = Diagnostic::new(self.severity, self.message);
        if let Some(span) = span {
            diag = diag.span(span);
        }
        diag.notes = self.notes;
        diag
    }
}

pub struct AttributeCtxt {
    diagnostics: Vec<GenericDiagnostic>,
}

impl AttributeCtxt {
    pub fn emit_diagnostic(&mut self, diag: GenericDiagnostic) {
        self.diagnostics.push(diag)
    }
}

pub struct LocationAttribute(u32);

pub trait AttributeVerifier: Any {
    fn verify(
        &self,
        ctxt: &mut AttributeCtxt,
        db: &dyn CompilerDb,
        parent_scope: ParentScopeId,
        arguments: &[AttributeArgumentNode],
    ) -> Option<Box<Self>>;
}

impl AttributeVerifier for LocationAttribute {
    fn verify(
        &self,
        ctxt: &mut AttributeCtxt,
        db: &dyn CompilerDb,
        parent_scope: ParentScopeId,
        arguments: &[AttributeArgumentNode],
    ) -> Option<Box<Self>> {
        let [AttributeArgumentNode::IntNumber { ast_id, value }] = arguments else {
            panic!("invalid argument syntax")
        };

        if *value < 0 {
            ctxt.diagnostics.push(
                GenericDiagnostic::error("invalid location number: expected a number between 0 and 0xFFFF_FFFF")
                    .location((*ast_id).into()),
            );
            return None;
        }

        todo!()
    }
}
