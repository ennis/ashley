use crate::{
    diagnostic::{SourceFileProvider, SourceId, SourceLocation},
    hir::{
        types::{ImageDimension, ImageType, SampledImageType, ScalarType, StructType, FunctionType},
        HirCtxt, Location, Operation, InstructionData, OperationId, RegionId, Type, TypeData,
        Value,
    },
};
use codespan_reporting::files::Files;
use indexmap::IndexSet;
use std::{
    collections::HashMap,
    fmt,
    fmt::{Display, Formatter, Write},
    fs::File,
    mem,
    path::Path,
};
use std::slice::SliceIndex;

struct HirHtmlPrintCtxt<'a, 'hir> {
    indent: u32,
    source_files: SourceFileProvider,
    w: &'a mut dyn Write,
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

pub struct DisplayOperandHtml(Value);

impl Display for DisplayOperandHtml {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let index = self.0.index();
        write!(f, "<a class=\"operand\" href=\"#v{index}\">v<sub>{index}</sub></a>")
    }
}

impl Value {
    fn display_html(&self) -> DisplayOperandHtml {
        DisplayOperandHtml(*self)
    }

    fn display_result_html(&self) -> DisplayOperandHtml {
        DisplayOperandHtml(*self)
    }
}

impl<'a> Op<'a> {
    fn mnmemonic(&self) -> &'static str {
        match self {
            Op::BinaryOp(op) => match op {
                BinaryOp::Add => "add",
                BinaryOp::Sub => "sub",
                BinaryOp::Mul => "mul",
                BinaryOp::Div => "div",
            },
            Op::Select => "select",
            Op::Loop(_) => "loop",
            Op::Call(_) => "call",
            Op::Interp(_) => "interp",
            _ => {}
        }
    }
}

struct DisplayScalarType(ScalarType);
struct DisplayVectorType(VectorType);
struct DisplayMatrixType(MatrixType);
struct DisplayArrayType(ArrayType);
struct DisplayImageType(ImageType);
struct DisplaySampledImageType(SampledImageType);

impl Display for DisplayScalarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 .0 {
            ScalarType::Int => write!(f, "int"),
            ScalarType::UnsignedInt => write!(f, "uint"),
            ScalarType::Float => write!(f, "float"),
            ScalarType::Double => write!(f, "double"),
            ScalarType::Bool => write!(f, "bool"),
        }
    }
}

impl Display for DisplayVectorType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let n = self.0 .1;
        match self.0 .0 {
            ScalarType::Int => write!(f, "ivec{n}"),
            ScalarType::UnsignedInt => write!(f, "uvec{n}"),
            ScalarType::Float => write!(f, "vec{n}"),
            ScalarType::Double => write!(f, "dvec{n}"),
            ScalarType::Bool => write!(f, "bvec{n}"),
        }
    }
}

impl Display for DisplayMatrixType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (r, c) = (self.0 .1, self.0 .2);
        match self.0 .0 {
            ScalarType::Int => write!(f, "imat{r}x{c}"),
            ScalarType::UnsignedInt => write!(f, "umat{r}x{c}"),
            ScalarType::Float => write!(f, "mat{r}x{c}"),
            ScalarType::Double => write!(f, "dmat{r}x{c}"),
            ScalarType::Bool => write!(f, "bmat{r}x{c}"),
        }
    }
}

impl Display for DisplaySampledImageType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.sampled_ty.0 {
            ScalarType::Int => write!(f, "i")?,
            ScalarType::UnsignedInt => write!(f, "u")?,
            ScalarType::Double => write!(f, "d")?,
            ScalarType::Bool => write!(f, "b")?,
            _ => {}
        }
        write!(f, "texture")?;
        match self.0.dim {
            ImageDimension::Dim1D => write!(f, "1D")?,
            ImageDimension::Dim2D => write!(f, "2D")?,
            ImageDimension::Dim3D => write!(f, "3D")?,
            ImageDimension::DimCube => write!(f, "Cube")?,
            ImageDimension::Dim1DArray => write!(f, "1DArray")?,
            ImageDimension::Dim2DArray => write!(f, "2DArray")?,
        }
        if self.0.ms {
            write!(f, "MS")?
        }
        Ok(())
    }
}

impl Display for DisplayImageType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.element_ty.0 {
            ScalarType::Int => write!(f, "i")?,
            ScalarType::UnsignedInt => write!(f, "u")?,
            ScalarType::Double => write!(f, "d")?,
            ScalarType::Bool => write!(f, "b")?,
            _ => {}
        }
        write!(f, "image")?;
        match self.0.dim {
            ImageDimension::Dim1D => write!(f, "1D")?,
            ImageDimension::Dim2D => write!(f, "2D")?,
            ImageDimension::Dim3D => write!(f, "3D")?,
            ImageDimension::DimCube => write!(f, "Cube")?,
            ImageDimension::Dim1DArray => write!(f, "1DArray")?,
            ImageDimension::Dim2DArray => write!(f, "2DArray")?,
        }
        if self.0.ms {
            write!(f, "MS")?
        }
        Ok(())
    }
}

impl ScalarType {
    fn display(&self) -> DisplayScalarType {
        DisplayScalarType(*self)
    }
}

impl VectorType {
    fn display(&self) -> DisplayVectorType {
        DisplayVectorType(*self)
    }
}

impl MatrixType {
    fn display(&self) -> DisplayMatrixType {
        DisplayMatrixType(*self)
    }
}

impl ImageType {
    fn display(&self) -> DisplayImageType {
        DisplayImageType(*self)
    }
}

impl SampledImageType {
    fn display(&self) -> DisplaySampledImageType {
        DisplaySampledImageType(*self)
    }
}

fn print_struct_type(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, ty: &StructType) -> fmt::Result {
    write!(ctxt.w, "struct<")?;
    write_list!(ctxt.w, field in ty.fields.iter() => {
        write!(ctxt.w, "{}:", field.name)?;
        print_type(ctxt, hir, field.ty)?;
    });
    write!(ctxt.w, ">")?;
    Ok(())
}

fn print_function_type(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, ty: &FunctionType) -> fmt::Result {
    write!(ctxt.w, "fn(")?;
    write_list!(ctxt.w, field in ty.fields.iter() => {
        write!(ctxt.w, "{}:", field.name)?;
        print_type(ctxt, hir, field.ty)?;
    });
    write!(ctxt.w, ")")?;
    Ok(())
}

fn print_type_impl(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, ty: &TypeData) -> fmt::Result {
    match ty {
        TypeData::Unit => {
            write!(ctxt.w, "void")?;
        }
        TypeData::Scalar(scalar_type) => {
            write!(ctxt.w, "{}", scalar_type.display())?;
        }
        TypeData::Vector(vector_type) => {
            write!(ctxt.w, "{}", vector_type.display())?;
        }
        TypeData::Matrix(matrix_type) => {
            write!(ctxt.w, "{}", matrix_type.display())?;
        }
        TypeData::Array(array_type) => {
            write!(ctxt.w, "[")?;
            print_type(ctxt, hir, array_type.0)?;
            write!(ctxt.w, "; {}]", array_type.1)?;
        }
        TypeData::RuntimeArray(ty) => {
            write!(ctxt.w, "[")?;
            print_type(ctxt, hir, *ty)?;
            write!(ctxt.w, "]")?;
        }
        TypeData::Struct(struct_type) => {
            print_struct_type(ctxt, hir, struct_type)?;
        }
        TypeData::SampledImage(_) => {}
        TypeData::Image(_) => {}
        TypeData::Pointer(ty) => {
            write!(ctxt.w, "*")?;
            print_type(ctxt, hir, *ty)?;
        }
        TypeData::Sampler => {
            write!(ctxt.w, "sampler")?;
        }
        TypeData::ShadowSampler => {
            write!(ctxt.w, "shadowSampler")?;
        }
        TypeData::String => {
            write!(ctxt.w, "string")?;
        }
        TypeData::Unknown => {
            write!(ctxt.w, "unknown")?;
        }
        TypeData::Function(function_type) => {
            print_function_type(ctxt, hir, function_type)?;
        }
    }
    Ok(())
}

fn print_type(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, ty: Type) -> fmt::Result {
    match &hir.types[ty] {
        ty @ TypeData::Scalar(_)
        | ty @ TypeData::Vector(_)
        | ty @ TypeData::Matrix(_)
        | ty @ TypeData::Array(_)
        | ty @ TypeData::RuntimeArray(_)
        | ty @ TypeData::String
        | ty @ TypeData::Unknown
        | ty @ TypeData::Pointer(_) => {
            // print inline
            print_type_impl(ctxt, hir, ty)
        }
        _ => {
            let i = ty.index();
            write!(ctxt.w, "<a class=\"val\" href=\"#t{i}\">!{i}</a>")
        }
    }
}

fn print_op(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, op: &InstructionData) -> fmt::Result {
    write!(ctxt.w, "<li class=\"op\">")?;

    // results
    write!(ctxt.w, "<span class=\"results\">")?;
    write_list!(ctxt.w, r in op.results => {
        let index = r.index();
        let ty = r.ty(hir);
        write!(ctxt.w, "<span class=\"value\" id=\"v{index}\">v<sub>{index}</sub> : ")?;
        print_type(ctxt, hir, ty)?;
        write!(ctxt.w, "</span>")?;
    });
    write!(ctxt.w, "</span>")?;
    write!(ctxt.w, " = ")?;

    // mnemonic
    write!(ctxt.w, "<span class=\"mnemonic\">{}</span>", op.op.mnmemonic())?;

    // instruction-specific format
    match op.op {
        Op::Select => {
            // condition
            write!(ctxt.w, "({}) {{", op.operands[0].display_html())?;
            print_region(ctxt, hir, &hir.regions[op.subregions[0]])?;
            write!(ctxt.w, "}}")?;
            if op.subregions.len() > 1 {
                write!(ctxt.w, " else {{")?;
                print_region(ctxt, hir, &hir.regions[op.subregions[1]])?;
                write!(ctxt.w, "}}")?;
            }
        }
        _ => {
            todo!()
        }
    }

    // location
    if let Location::Source(src_loc) = op.location {
        let name = ctxt
            .source_files
            .name(src_loc.file)
            .unwrap_or_else(|_| "???".to_string());
        let start_line = ctxt
            .source_files
            .line_index(src_loc.file, src_loc.range.start().into())
            .unwrap_or(1);
        let start_col = ctxt
            .source_files
            .column_number(src_loc.file, start_line, src_loc.range.start().into())
            .unwrap_or(1);
        //let end_line = self.source_files.line_index(src_loc.file, src_loc.range.end().into()).unwrap_or(1);
        //let end_col = self.source_files.line_index(src_loc.file, end_line, src_loc.range.end().into()).unwrap_or(1);
        write!(
            ctxt.w,
            "<a class=\"loc\" href=\"file://{name}\">{name}:{start_line}:{start_col}</a>"
        )?;
    }

    write!(ctxt.w, "</li>")?;
    Ok(())
}

fn print_region(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt, region: &RegionData) -> fmt::Result {
    if !region.arguments.is_empty() {
        write_list!(ctxt.w, arg in region.arguments => {
            let i = arg.index();
            write!(ctxt.w, "<span class=\"value\" id=\"v{i}\">v<sub>{i}</sub></span>")?;
        });
        write!(ctxt.w, " -> ")?;
    }
    write!(ctxt.w, "<ul class=\"oplist\">")?;
    let mut operation = region.ops.first();
    while let Some(op) = operation {
        let op = &hir.ops[op];
        print_op(ctxt, hir, &op.data)?;
        operation = op.next;
    }
    write!(ctxt.w, "</ul>")?;
    Ok(())
}


fn print_type_definitions(ctxt: &mut HirHtmlPrintCtxt, hir: &HirCtxt) -> fmt::Result {
    write!(ctxt.w, "<p class=\"types\">")?;
    for (ty, tyimpl) in hir.types.iter() {
        let index = ty.index();
        write!(ctxt.w, "<span class=\"deftype\">")?;
        write!(ctxt.w, "<span class=\"attr\" id=\"t{index}\">!{index}</span> = ")?;
        print_type_impl(ctxt, hir, tyimpl)?;
        write!(ctxt.w, "</span><br>")?;
    }
    write!(ctxt.w, "</p>")?;
    Ok(())
}

impl<'a, 'hir> HirHtmlPrintCtxt<'a, 'hir> {
    fn new(
        source_files: SourceFileProvider,
        w: &'a mut dyn Write,
    ) -> HirHtmlPrintCtxt<'a, 'hir> {
        HirHtmlPrintCtxt {
            source_files,
            indent: 0,
            w,
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
    let mut html = HirHtmlPrintCtxt::new(source_files, &mut output);
    print_type_definitions(ctxt, hir, )?;
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
