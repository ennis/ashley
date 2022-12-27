use crate::{
    dialect::{base::VectorType, gpu::ImageType},
    hir::{constraint::Region, MatchCtxt, ValueId},
    operation_constraints,
};

///
pub fn verify_fragment_export(cx: &MatchCtxt, image_resource: ValueId, fragment_value: ValueId) -> bool {
    // image_resource must be a writable image
    // fragment_value must be a 4-component vector
    // the class of the vector element type (fp or integer) must match the type of the image
    let Some(ty @ ImageType{element_ty: image_elem_ty, ..}) = image_resource.ty(cx).cast() else { return false; };
    let Some(frag_ty @ VectorType(elem_ty, 4)) = fragment_value.ty(cx).cast() else { return false; };
    image_elem_ty.0 == elem_ty
}

operation_constraints! {
    /// Writes a color or depth value in a fragment domain to an image resource.
    pub FragmentExport {
        image_resource: ValueId,
        fragment_value: ValueId
    } = {
        "gpu.frag.export"(image_resource:!image_ty, fragment_value:!frag_ty) -> ()
    }

    /// Fragment shader X derivative.
    pub DFdx { precision:DerivativePrecision, result: ResultId, value: ValueId } =
    { "gpu.frag.dfdx"<precision:DerivativePrecision>(value:!ty) -> (result:!ty) }

    /// Fragment shader Y derivative.
    pub DFdy { precision:DerivativePrecision, result: ResultId, value: ValueId } =
    { "gpu.frag.dfdy"<precision:DerivativePrecision>(value:!ty) -> (result:!ty) }

    /// Interpolates an input attribute to a fragment shader.
    pub Interp { interpolation_mode: ValueId, attribute: ValueId } = {
        "gpu.frag.interp"(interpolation:InterpolationMode, attribute:!ty) -> (result:!ty)
    }

    /// Performs vertex processing over draw calls.
    pub ProcessVertices {
        draw_items: ValueId,
        vertex_shader: RegionId,
        output_vertex_stream: ValueId,
    } = {
        "gpu.proc.vertex"(draw_items: DrawItemStream) { vertex_shader:Region } -> (output_vertex_stream:VertexStream)
    }

    /// Assembles primitives from a vertex stream.
    pub ProcessPrimitives {
        primitive_type: PrimitiveType,
        output_primitive_stream: ValueId,
    } =
    {
        "gpu.proc.prim"<primitive_type:PrimitiveType> (vertices: VertexStream, indices?) -> (output_primitive_stream:PrimitiveStream)
    }

    /// Rasterizes primitives into a fragment stream.
    ///
    /// Can take multiple primitive streams.
    pub Rasterize {
        rasterization_state: RasterizationState,
        output_fragment_stream: ValueId
    } = {
        "gpu.raster"<rasterization_state: RasterizationState> (..primitives : PrimitiveStream) -> (output_fragment_stream:FragmentStream)
    }


    /// Runs a fragment shader.



}
