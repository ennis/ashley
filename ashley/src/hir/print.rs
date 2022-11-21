use crate::hir::{Attribute, HirCtxt, Operand, Operation, OperationId, RegionId, Type, TypeOrAttr, ValueId};
use indexmap::IndexSet;
use std::{
    collections::HashMap,
    fmt,
    fmt::{Formatter, Write},
    mem,
};

/// Trait for HIR entities that can be printed to an `OperationPrinter`
pub trait IRPrintable<'hir> {
    // Option: given a Type
    // 1. print it inline
    // 2. define

    /// Whether the type or attribute that this entity represents should be printed inline, instead
    /// of creating an alias for it in the textual representation.
    fn is_inline(&self) -> bool {
        false
    }

    /// Prints this value to the specified `OperationPrinter`.
    fn print_hir(&self, printer: &mut dyn IRPrinter<'hir>);
}

#[macro_export]
macro_rules! write_ir {
    ($printer:expr, $($t:expr),*) => {
        $printer.emit(&[
            $($crate::hir::IRSyntaxElem::from($t),)*
        ])
    };
}
pub use write_ir;

pub enum IRSyntaxElem<'a, 'hir> {
    Token(&'a str),
    UInt(u32),
    Int(i32),
    Type(Type<'hir>),
    TypeDef(Type<'hir>),
    Attribute(Attribute<'hir>),
    AttributeDef(Attribute<'hir>),
    Value(ValueId),
    Region(RegionId),
}

impl<'a, 'hir> From<Type<'hir>> for IRSyntaxElem<'a, 'hir> {
    fn from(value: Type<'hir>) -> Self {
        IRSyntaxElem::Type(value)
    }
}

impl<'a, 'hir> From<Attribute<'hir>> for IRSyntaxElem<'a, 'hir> {
    fn from(value: Attribute<'hir>) -> Self {
        IRSyntaxElem::Attribute(value)
    }
}

impl<'a, 'hir> From<&'a str> for IRSyntaxElem<'a, 'hir> {
    fn from(value: &'a str) -> Self {
        IRSyntaxElem::Token(value)
    }
}

impl<'a, 'hir> From<u8> for IRSyntaxElem<'a, 'hir> {
    fn from(value: u8) -> Self {
        IRSyntaxElem::UInt(value as u32)
    }
}
impl<'a, 'hir> From<u16> for IRSyntaxElem<'a, 'hir> {
    fn from(value: u16) -> Self {
        IRSyntaxElem::UInt(value as u32)
    }
}
impl<'a, 'hir> From<u32> for IRSyntaxElem<'a, 'hir> {
    fn from(value: u32) -> Self {
        IRSyntaxElem::UInt(value)
    }
}
impl<'a, 'hir> From<i8> for IRSyntaxElem<'a, 'hir> {
    fn from(value: i8) -> Self {
        IRSyntaxElem::Int(value as i32)
    }
}
impl<'a, 'hir> From<i16> for IRSyntaxElem<'a, 'hir> {
    fn from(value: i16) -> Self {
        IRSyntaxElem::Int(value as i32)
    }
}
impl<'a, 'hir> From<i32> for IRSyntaxElem<'a, 'hir> {
    fn from(value: i32) -> Self {
        IRSyntaxElem::Int(value)
    }
}

pub trait IRPrinter<'hir> {
    fn emit(&mut self, elem: &[IRSyntaxElem<'_, 'hir>]);
    fn token(&mut self, token: &str) {
        self.emit(&[IRSyntaxElem::Token(token)])
    }
    fn ty(&mut self, ty: Type<'hir>) {
        self.emit(&[IRSyntaxElem::Type(ty)])
    }
    fn attr(&mut self, attr: Attribute<'hir>) {
        self.emit(&[IRSyntaxElem::Attribute(attr)])
    }
}

//impl<>

// attribute parsing & printing pattern:
// - token
// - print inner value
// - type refs
//

struct HirHtmlPrintCtxt<'a, 'hir> {
    hir: &'a HirCtxt<'hir>,
    indent: u32,
    w: &'a mut dyn Write,
    // Map TypeOrAttr -> Index
    types_and_attrs: HashMap<TypeOrAttr<'hir>, usize>,
}

impl<'a, 'hir> HirHtmlPrintCtxt<'a, 'hir> {
    fn new(hir: &'a HirCtxt<'hir>, w: &'a mut dyn Write) -> HirHtmlPrintCtxt<'a, 'hir> {
        let mut types_and_attrs = HashMap::new();
        for (i, ty_or_attr) in hir.types_and_attributes.iter().enumerate() {
            types_and_attrs.insert(*ty_or_attr, i);
        }
        HirHtmlPrintCtxt {
            hir,
            indent: 0,
            w,
            types_and_attrs,
        }
    }
}

// implemented as a macro to avoid borrowing woes
macro_rules! write_list {
    ($w:expr, $i:ident in $coll:expr => $b:block) => {
        let mut first = true;
        for $i in $coll {
            if !first {
                write!($w, ",").unwrap();
            }
            $b
            first = false;
        }
    };
}

impl<'a, 'hir> IRPrinter<'hir> for HirHtmlPrintCtxt<'a, 'hir> {
    fn emit(&mut self, elems: &[IRSyntaxElem<'_, 'hir>]) {
        for elem in elems {
            match elem {
                IRSyntaxElem::Token(str) => {
                    write!(self.w, "{}", html_escape::encode_text(str)).unwrap();
                }
                IRSyntaxElem::UInt(v) => {
                    write!(self.w, "{v}").unwrap();
                }
                IRSyntaxElem::Int(v) => {
                    write!(self.w, "{v}").unwrap();
                }
                IRSyntaxElem::Type(ty) => {
                    if ty.0.is_inline() {
                        ty.0.print_hir(self);
                    } else {
                        let i = self.types_and_attrs[&TypeOrAttr::Type(*ty)];
                        write!(self.w, "<a class=\"val\" href=\"#t{i}\">t<sub>{i}</sub></a>").unwrap();
                    }
                }
                IRSyntaxElem::TypeDef(ty) => {
                    ty.0.print_hir(self);
                }
                IRSyntaxElem::Attribute(attr) => {
                    if attr.0.is_inline() {
                        attr.0.print_hir(self);
                    } else {
                        let i = self.types_and_attrs[&TypeOrAttr::Attribute(*attr)];
                        write!(self.w, "<a class=\"attr\" href=\"#a{i}\">#{i}</a>").unwrap();
                    }
                }
                IRSyntaxElem::AttributeDef(attr) => {
                    attr.0.print_hir(self);
                }
                IRSyntaxElem::Value(v) => {
                    let i = v.index();
                    write!(self.w, "<a class=\"val\" href=\"#v{i}\">v<sub>{i}</sub></a>").unwrap();
                }
                IRSyntaxElem::Region(r) => {
                    let index = r.index();
                    write!(self.w, "<a class=\"region\" href=\"#r{index}\">r{index}</a>").unwrap();
                }
            }
        }
    }
}

impl<'a, 'hir> HirHtmlPrintCtxt<'a, 'hir> {
    fn print_indent(&mut self) -> fmt::Result {
        for _ in 0..(self.indent * 2) {
            write!(self.w, " ")?;
        }
        Ok(())
    }

    /// NOTE: must be called first to setup type_and_attrs
    fn print_attribute_and_type_definitions(&mut self) -> fmt::Result {
        write!(self.w, "<p class=\"attrs-and-types\">")?;
        for (i, ty_or_attr) in self.hir.types_and_attributes.iter().enumerate() {
            match ty_or_attr {
                TypeOrAttr::Type(ty) => {
                    write!(self.w, "<span class=\"deftype\">")?;
                    write!(self.w, "<span class=\"type\" id=\"t{i}\">t<sub>{i}</sub></span> = ")?;
                    ty.0.print_hir(self);
                    write!(self.w, "</span><br>")?;
                }
                TypeOrAttr::Attribute(attr) => {
                    write!(self.w, "<span class=\"defattr\">")?;
                    write!(self.w, "<span class=\"attr\" id=\"a{i}\">#{i}</span> = ")?;
                    attr.0.print_hir(self);
                    write!(self.w, "</span><br>")?;
                }
            }
        }
        write!(self.w, "</p>")?;
        Ok(())
    }

    fn print_region(&mut self, region: RegionId) -> fmt::Result {
        let i = region.index();
        let region = &self.hir.regions[region].data;
        if !region.arguments.is_empty() {
            write_list!(self.w, arg in region.arguments => {
                let i = arg.index();
                write!(self.w, "<span class=\"value\" id=\"v{i}\">v<sub>{i}</sub></span>")?;
            });
            write!(self.w, " -> ")?;
        }
        write!(self.w, "<ul class=\"oplist\">")?;
        let mut operation = region.ops.first();

        while let Some(op) = operation {
            self.print_operation(op)?;
            operation = self.hir.ops[op].next;
        }
        write!(self.w, "</ul>")?;
        Ok(())
    }

    fn print_operation(&mut self, op: OperationId) -> fmt::Result {
        self.print_indent()?;
        let op = &self.hir.ops[op].data;
        write!(self.w, "<li class=\"op\">")?;
        if !op.results.is_empty() {
            write!(self.w, "<span class=\"results\">")?;
            write_list!(self.w, v in op.results => {
                let i = v.index();
                write!(self.w, "<span class=\"value\" id=\"v{i}\">v<sub>{i}</sub></span>")?;
            });
            write!(self.w, "</span> = ")?;
        }
        let mnemonic = self.hir.op_def_map[&op.opcode].mnemonic;
        write!(self.w, "<span class=\"mnemonic\">{mnemonic}</span> ")?;
        if !op.attributes.is_empty() {
            write!(self.w, "<span class=\"attrs\">&lt;")?;
            write_list!(self.w, attr in op.attributes => {
                self.emit(&[(*attr).into()]);
            });
            write!(self.w, "&gt;</span>")?;
        }
        write!(self.w, "<span class=\"operands\">")?;
        write_list!(self.w, operand in op.operands => {
            let index = operand.index();
            write!(self.w, "<a class=\"operand\" href=\"#v{index}\">v<sub>{index}</sub></a>")?;
        });
        write!(self.w, "</span>")?;

        let mut region = op.regions.first();
        while let Some(r) = region {
            let i = r.index();
            write!(self.w, " {{")?;
            self.print_region(r)?;
            write!(self.w, "}}")?;
            region = self.hir.regions[r].next;
        }

        write!(self.w, "</li>")?;

        Ok(())
    }
}

/// Prints a region as an a HTML-formatted document.
pub fn print_region_as_html(hir: &HirCtxt, title: &str, region: RegionId, out: &mut dyn Write) -> fmt::Result {
    // write the HTML body
    let mut output = String::new();
    let mut html = HirHtmlPrintCtxt::new(hir, &mut output);
    html.print_attribute_and_type_definitions()?;
    html.print_region(region)?;

    // combine the HTML for types+attribs and regions into the final HTML document
    write!(
        out,
        r#"<!DOCTYPE html>
<html lang="en">
<style type="text/css">
body {{
    font-family: monospace;
}}
.oplist {{
    list-style-type: none;
    padding-left: 2em;
}}
.mnemonic {{
    font-weight: bold;
    color: #0022DD;
}}
.value {{
    color: #AA1100;
}}
</style>
<head>
    <title>{title}</title>
    <meta charset="UTF-8">
</head>
<body>"#
    )?;
    out.write_str(&output)?;
    write!(out, "</body></html>")?;
    Ok(())
}
