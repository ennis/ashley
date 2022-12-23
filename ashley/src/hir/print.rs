use crate::{
    diagnostic::{SourceFileProvider, SourceId, SourceLocation},
    hir::{Attr, HirCtxt, Location, Operation, OperationId, RegionId, ValueId},
};
use codespan_reporting::files::Files;
use indexmap::IndexSet;
use std::{
    collections::HashMap,
    fmt,
    fmt::{Formatter, Write},
    fs::File,
    mem,
    path::Path,
};

/// Trait for HIR entities that can be printed to an `OperationPrinter`
pub trait IRPrintable<'a> {
    /// Whether the type or attribute that this entity represents should be printed inline, instead
    /// of creating an alias for it in the textual representation.
    fn is_inline(&self) -> bool {
        false
    }

    /// Prints this value to the specified `OperationPrinter`.
    fn print_hir(&self, printer: &mut dyn IRPrinter<'a>);
}

#[macro_export]
macro_rules! write_ir {
    ($printer:expr, $($t:expr),*) => {
        $printer.emit(&[
            $($crate::hir::ToIRSyntax::to_ir_syntax(&$t),)*
        ])
    };
}
pub use write_ir;

pub enum IRSyntaxElem<'a, 'hir> {
    Token(&'a str),
    UInt(u64),
    Int(i64),
    SourceId(SourceId),
    SourceLocation(SourceLocation),
    Type(Attr<'hir>),
    TypeDef(Attr<'hir>),
    Attribute(Attr<'hir>),
    AttributeDef(Attr<'hir>),
    Value(ValueId),
    Region(RegionId),
}

pub trait ToIRSyntax<'hir> {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir>;
}

impl<'hir> ToIRSyntax<'hir> for Attr<'hir> {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Attribute(self.clone())
    }
}
impl<'hir> ToIRSyntax<'hir> for str {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Token(self)
    }
}
impl<'hir> ToIRSyntax<'hir> for SourceId {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::SourceId(*self)
    }
}
impl<'hir> ToIRSyntax<'hir> for SourceLocation {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::SourceLocation(*self)
    }
}
impl<'hir> ToIRSyntax<'hir> for u8 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::UInt(*self as u64)
    }
}
impl<'hir> ToIRSyntax<'hir> for u16 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::UInt(*self as u64)
    }
}
impl<'hir> ToIRSyntax<'hir> for u32 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::UInt(*self as u64)
    }
}
impl<'hir> ToIRSyntax<'hir> for u64 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::UInt(*self)
    }
}
impl<'hir> ToIRSyntax<'hir> for i8 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Int(*self as i64)
    }
}
impl<'hir> ToIRSyntax<'hir> for i16 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Int(*self as i64)
    }
}
impl<'hir> ToIRSyntax<'hir> for i32 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Int(*self as i64)
    }
}
impl<'hir> ToIRSyntax<'hir> for i64 {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Int(*self)
    }
}
impl<'hir> ToIRSyntax<'hir> for String {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        IRSyntaxElem::Token(self)
    }
}

impl<'hir, T> ToIRSyntax<'hir> for &T where T: ToIRSyntax<'hir> + ?Sized {
    fn to_ir_syntax<'a>(&'a self) -> IRSyntaxElem<'a, 'hir> {
        ToIRSyntax::to_ir_syntax(&**self)
    }
}

pub trait IRPrinter<'hir> {
    fn emit(&mut self, elem: &[IRSyntaxElem<'_, 'hir>]);
    fn token(&mut self, token: &str) {
        self.emit(&[IRSyntaxElem::Token(token)])
    }
    /*fn ty(&mut self, ty: Type<'hir>) {
        self.emit(&[IRSyntaxElem::Type(ty)])
    }*/
    fn attr(&mut self, attr: Attr<'hir>) {
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
    source_files: SourceFileProvider,
    w: &'a mut dyn Write,
    // Map TypeOrAttr -> Index
    attrs: HashMap<Attr<'hir>, usize>,
}

impl<'a, 'hir> HirHtmlPrintCtxt<'a, 'hir> {
    fn new(
        hir: &'a HirCtxt<'hir>,
        source_files: SourceFileProvider,
        w: &'a mut dyn Write,
    ) -> HirHtmlPrintCtxt<'a, 'hir> {
        let mut attrs = HashMap::new();
        for (i, attr) in hir.attributes.iter().enumerate() {
            attrs.insert(*attr, i);
        }
        HirHtmlPrintCtxt {
            hir,
            source_files,
            indent: 0,
            w,
            attrs,
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
                        let i = self.attrs[ty];
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
                        let i = self.attrs[attr];
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
                IRSyntaxElem::SourceId(_) => {}
                IRSyntaxElem::SourceLocation(_) => {}
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
        for (i, attr) in self.hir.attributes.iter().enumerate() {
            write!(self.w, "<span class=\"defattr\">")?;
            write!(self.w, "<span class=\"attr\" id=\"a{i}\">#{i}</span> = ")?;
            attr.0.print_hir(self);
            write!(self.w, "</span><br>")?;
        }
        write!(self.w, "</p>")?;
        Ok(())
    }

    fn print_region(&mut self, region: RegionId) -> fmt::Result {
        let i = region.index();
        let region = &self.hir.regions[region];
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
        let mnemonic = op.opcode;
        write!(self.w, "<span class=\"mnemonic\">{mnemonic}</span> ")?;
        if !op.attributes.is_empty() {
            write!(self.w, "<span class=\"attrs\">&lt;")?;
            write_list!(self.w, attr in op.attributes => {
                self.attr(*attr);
            });
            write!(self.w, "&gt;</span>")?;
        }
        write!(self.w, "<span class=\"operands\">")?;
        write_list!(self.w, operand in op.operands => {
            let index = operand.index();
            write!(self.w, "<a class=\"operand\" href=\"#v{index}\">v<sub>{index}</sub></a>")?;
        });
        write!(self.w, "</span>")?;

        if let Location::Source(src_loc) = op.location {
            let name = self
                .source_files
                .name(src_loc.file)
                .unwrap_or_else(|_| "???".to_string());
            let start_line = self
                .source_files
                .line_index(src_loc.file, src_loc.range.start().into())
                .unwrap_or(1);
            let start_col = self
                .source_files
                .column_number(src_loc.file, start_line, src_loc.range.start().into())
                .unwrap_or(1);
            //let end_line = self.source_files.line_index(src_loc.file, src_loc.range.end().into()).unwrap_or(1);
            //let end_col = self.source_files.line_index(src_loc.file, end_line, src_loc.range.end().into()).unwrap_or(1);
            write!(
                self.w,
                "<a class=\"loc\" href=\"file://{name}\">{name}:{start_line}:{start_col}</a>"
            )?;
        }

        for r in op.regions {
            let i = r.index();
            write!(self.w, " {{")?;
            self.print_region(*r)?;
            write!(self.w, "}}")?;
        }

        write!(self.w, "</li>")?;

        Ok(())
    }
}

/// Prints a region as an a HTML-formatted document.
pub fn print_hir_region_html(
    hir: &HirCtxt,
    title: &str,
    region: RegionId,
    source_files: SourceFileProvider,
    out: &mut dyn Write,
) -> fmt::Result {
    // write the HTML body
    let mut output = String::new();
    let mut html = HirHtmlPrintCtxt::new(hir, source_files, &mut output);
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
.loc {{
    color: #AAAAAA;
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

/// Writes HIR to the specified HTML file
pub fn write_hir_html_file(ctxt: &HirCtxt, path: &str, region: RegionId, source_files: SourceFileProvider) {
    let mut s = String::new();
    print_hir_region_html(
        ctxt,
        Path::new(path).file_name().unwrap().to_str().unwrap(),
        region,
        source_files,
        &mut s,
    )
    .unwrap();

    let mut file = File::create(path).unwrap();
    use std::io::Write as IoWrite;
    file.write(s.as_bytes()).unwrap();
}
