// --- Core instructions ---
#[allow(unused_mut)]
impl<'a> FunctionBuilder<'a> {
    pub fn emit_nop(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Nop);
        self.append_inst(inst_builder);
    }
    pub fn emit_undef(&mut self, result_type: Type) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Undef);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_source_continued(&mut self, continued_source: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SourceContinued);
        continued_source.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_source(
        &mut self,
        source_language0: spirv::SourceLanguage,
        version: i32,
        file: Option<IdRef>,
        source: Option<&str>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Source);
        source_language0.write_operand(&mut inst_builder);
        version.write_operand(&mut inst_builder);
        file.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_source_extension(&mut self, extension: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SourceExtension);
        extension.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_name(&mut self, target: IdRef, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Name);
        target.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_name(&mut self, r#type: IdRef, member: i32, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemberName);
        r#type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_line(&mut self, file: IdRef, line: i32, column: i32) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Line);
        file.write_operand(&mut inst_builder);
        line.write_operand(&mut inst_builder);
        column.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_extension(&mut self, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Extension);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ext_inst(
        &mut self,
        result_type: Type,
        set: IdRef,
        instruction: u32,
        operand_1_operand_2: &[IdRef],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ExtInst);
        inst_builder.set_result(result_type);
        set.write_operand(&mut inst_builder);
        instruction.write_operand(&mut inst_builder);
        operand_1_operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_memory_model(&mut self, addressing_model0: spirv::AddressingModel, memory_model1: spirv::MemoryModel) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemoryModel);
        addressing_model0.write_operand(&mut inst_builder);
        memory_model1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_entry_point(
        &mut self,
        execution_model0: spirv::ExecutionModel,
        entry_point: IdRef,
        name: &str,
        interface: &[IdRef],
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EntryPoint);
        execution_model0.write_operand(&mut inst_builder);
        entry_point.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        interface.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode(&mut self, entry_point: IdRef, mode: spirv::ExecutionMode) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ExecutionMode);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capability(&mut self, capability: spirv::Capability) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Capability);
        capability.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image_texel_pointer(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        sample: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageTexelPointer);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_load(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory_access3: Option<spirv::MemoryAccess>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Load);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_store(&mut self, pointer: IdRef, object: IdRef, memory_access2: Option<spirv::MemoryAccess>) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Store);
        pointer.write_operand(&mut inst_builder);
        object.write_operand(&mut inst_builder);
        memory_access2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_copy_memory(
        &mut self,
        target: IdRef,
        source: IdRef,
        memory_access2: Option<spirv::MemoryAccess>,
        memory_access3: Option<spirv::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CopyMemory);
        target.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        memory_access2.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_copy_memory_sized(
        &mut self,
        target: IdRef,
        source: IdRef,
        size: IdRef,
        memory_access3: Option<spirv::MemoryAccess>,
        memory_access4: Option<spirv::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CopyMemorySized);
        target.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        memory_access4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_access_chain(&mut self, result_type: Type, base: IdRef, indexes: &[IdRef]) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_in_bounds_access_chain(&mut self, result_type: Type, base: IdRef, indexes: &[IdRef]) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::InBoundsAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: IdRef,
        element: IdRef,
        indexes: &[IdRef],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::PtrAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_array_length(&mut self, result_type: Type, structure: IdRef, array_member: i32) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ArrayLength);
        inst_builder.set_result(result_type);
        structure.write_operand(&mut inst_builder);
        array_member.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_ptr_mem_semantics(&mut self, result_type: Type, pointer: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GenericPtrMemSemantics);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_in_bounds_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: IdRef,
        element: IdRef,
        indexes: &[IdRef],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::InBoundsPtrAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_decorate(&mut self, target: IdRef, decoration1: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Decorate);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate(&mut self, structure_type: IdRef, member: i32, decoration2: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemberDecorate);
        structure_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_decorate(&mut self, decoration_group: IdRef, targets: &[IdRef]) {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupDecorate);
        decoration_group.write_operand(&mut inst_builder);
        targets.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_vector_extract_dynamic(&mut self, result_type: Type, vector: IdRef, index: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::VectorExtractDynamic);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_insert_dynamic(
        &mut self,
        result_type: Type,
        vector: IdRef,
        component: IdRef,
        index: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::VectorInsertDynamic);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_shuffle(
        &mut self,
        result_type: Type,
        vector_1: IdRef,
        vector_2: IdRef,
        components: &[i32],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::VectorShuffle);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_construct(&mut self, result_type: Type, constituents: &[IdRef]) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CompositeConstruct);
        inst_builder.set_result(result_type);
        constituents.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_extract(&mut self, result_type: Type, composite: IdRef, indexes: &[i32]) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CompositeExtract);
        inst_builder.set_result(result_type);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_insert(
        &mut self,
        result_type: Type,
        object: IdRef,
        composite: IdRef,
        indexes: &[i32],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CompositeInsert);
        inst_builder.set_result(result_type);
        object.write_operand(&mut inst_builder);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_copy_object(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CopyObject);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_transpose(&mut self, result_type: Type, matrix: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Transpose);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sampled_image(&mut self, result_type: Type, image: IdRef, sampler: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SampledImage);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        sampler.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleDrefImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleDrefExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleProjImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleProjExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleProjDrefImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleProjDrefExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_fetch(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageFetch);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_gather(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        component: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageGather);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_dref_gather(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageDrefGather);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_read(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageRead);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_write(
        &mut self,
        image: IdRef,
        coordinate: IdRef,
        texel: IdRef,
        image_operands3: Option<ImageOperands>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageWrite);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        texel.write_operand(&mut inst_builder);
        image_operands3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image(&mut self, result_type: Type, sampled_image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Image);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_format(&mut self, result_type: Type, image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQueryFormat);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_order(&mut self, result_type: Type, image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQueryOrder);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_size_lod(&mut self, result_type: Type, image: IdRef, level_of_detail: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQuerySizeLod);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        level_of_detail.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_size(&mut self, result_type: Type, image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQuerySize);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_lod(&mut self, result_type: Type, sampled_image: IdRef, coordinate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQueryLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_levels(&mut self, result_type: Type, image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQueryLevels);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_samples(&mut self, result_type: Type, image: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageQuerySamples);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_f_to_u(&mut self, result_type: Type, float_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertFToU);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_f_to_s(&mut self, result_type: Type, float_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertFToS);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_s_to_f(&mut self, result_type: Type, signed_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertSToF);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_f(&mut self, result_type: Type, unsigned_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertUToF);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_convert(&mut self, result_type: Type, unsigned_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UConvert);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_convert(&mut self, result_type: Type, signed_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SConvert);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_convert(&mut self, result_type: Type, float_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FConvert);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_quantize_to_f16(&mut self, result_type: Type, value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::QuantizeToF16);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_ptr_to_u(&mut self, result_type: Type, pointer: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertPtrToU);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sat_convert_s_to_u(&mut self, result_type: Type, signed_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SatConvertSToU);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sat_convert_u_to_s(&mut self, result_type: Type, unsigned_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SatConvertUToS);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_ptr(&mut self, result_type: Type, integer_value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertUToPtr);
        inst_builder.set_result(result_type);
        integer_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_cast_to_generic(&mut self, result_type: Type, pointer: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::PtrCastToGeneric);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_cast_to_ptr(&mut self, result_type: Type, pointer: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GenericCastToPtr);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_cast_to_ptr_explicit(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        storage: spirv::StorageClass,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GenericCastToPtrExplicit);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitcast(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Bitcast);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_negate(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SNegate);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_negate(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FNegate);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IAdd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_add(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FAdd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ISub);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_sub(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FSub);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_mul(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IMul);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_mul(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FMul);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_div(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_div(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_div(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mod(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_rem(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SRem);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_mod(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_rem(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FRem);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_mod(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_times_scalar(&mut self, result_type: Type, vector: IdRef, scalar: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::VectorTimesScalar);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_scalar(&mut self, result_type: Type, matrix: IdRef, scalar: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::MatrixTimesScalar);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_times_matrix(&mut self, result_type: Type, vector: IdRef, matrix: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::VectorTimesMatrix);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_vector(&mut self, result_type: Type, matrix: IdRef, vector: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::MatrixTimesVector);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_matrix(&mut self, result_type: Type, left_matrix: IdRef, right_matrix: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::MatrixTimesMatrix);
        inst_builder.set_result(result_type);
        left_matrix.write_operand(&mut inst_builder);
        right_matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_outer_product(&mut self, result_type: Type, vector_1: IdRef, vector_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::OuterProduct);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dot(&mut self, result_type: Type, vector_1: IdRef, vector_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Dot);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add_carry(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IAddCarry);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub_borrow(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ISubBorrow);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mul_extended(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UMulExtended);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_mul_extended(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SMulExtended);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_any(&mut self, result_type: Type, vector: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Any);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_all(&mut self, result_type: Type, vector: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::All);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_nan(&mut self, result_type: Type, x: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsNan);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_inf(&mut self, result_type: Type, x: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsInf);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_finite(&mut self, result_type: Type, x: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsFinite);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_normal(&mut self, result_type: Type, x: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsNormal);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sign_bit_set(&mut self, result_type: Type, x: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SignBitSet);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_less_or_greater(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LessOrGreater);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ordered(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Ordered);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_unordered(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Unordered);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LogicalEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_not_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LogicalNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_or(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LogicalOr);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_and(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LogicalAnd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_not(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::LogicalNot);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_select(&mut self, result_type: Type, condition: IdRef, object_1: IdRef, object_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Select);
        inst_builder.set_result(result_type);
        condition.write_operand(&mut inst_builder);
        object_1.write_operand(&mut inst_builder);
        object_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_not_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::INotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_greater_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_greater_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_greater_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_greater_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_less_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ULessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_less_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_less_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ULessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_less_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_not_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_not_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_less_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_less_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_greater_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_greater_than(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_less_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_less_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_greater_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FOrdGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_greater_than_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FUnordGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_right_logical(&mut self, result_type: Type, base: IdRef, shift: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ShiftRightLogical);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_right_arithmetic(&mut self, result_type: Type, base: IdRef, shift: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ShiftRightArithmetic);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_left_logical(&mut self, result_type: Type, base: IdRef, shift: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ShiftLeftLogical);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_or(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitwiseOr);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_xor(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitwiseXor);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_and(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitwiseAnd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_not(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Not);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_insert(
        &mut self,
        result_type: Type,
        base: IdRef,
        insert: IdRef,
        offset: IdRef,
        count: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitFieldInsert);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        insert.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_s_extract(&mut self, result_type: Type, base: IdRef, offset: IdRef, count: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitFieldSExtract);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_u_extract(&mut self, result_type: Type, base: IdRef, offset: IdRef, count: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitFieldUExtract);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_reverse(&mut self, result_type: Type, base: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitReverse);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_count(&mut self, result_type: Type, base: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BitCount);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdx);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdy);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::Fwidth);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx_fine(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdxFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy_fine(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdyFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth_fine(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FwidthFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx_coarse(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdxCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy_coarse(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::DPdyCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth_coarse(&mut self, result_type: Type, p: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FwidthCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_emit_vertex(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EmitVertex);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_primitive(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EndPrimitive);
        self.append_inst(inst_builder);
    }
    pub fn emit_emit_stream_vertex(&mut self, stream: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EmitStreamVertex);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_stream_primitive(&mut self, stream: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EndStreamPrimitive);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_control_barrier(
        &mut self,
        execution: ValueOrConstant,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ControlBarrier);
        execution.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_memory_barrier(&mut self, memory: ValueOrConstant, semantics: ValueOrConstant) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemoryBarrier);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_load(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicLoad);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_store(
        &mut self,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicStore);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_exchange(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicExchange);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_compare_exchange(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        equal: ValueOrConstant,
        unequal: ValueOrConstant,
        value: IdRef,
        comparator: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicCompareExchange);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        equal.write_operand(&mut inst_builder);
        unequal.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        comparator.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_compare_exchange_weak(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        equal: ValueOrConstant,
        unequal: ValueOrConstant,
        value: IdRef,
        comparator: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicCompareExchangeWeak);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        equal.write_operand(&mut inst_builder);
        unequal.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        comparator.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_increment(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicIIncrement);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_decrement(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicIDecrement);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_add(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicIAdd);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_sub(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicISub);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_s_min(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicSMin);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_u_min(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicUMin);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_s_max(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicSMax);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_u_max(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicUMax);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_and(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicAnd);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_or(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicOr);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_xor(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicXor);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_kill(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Kill);
        self.append_inst(inst_builder);
    }
    pub fn emit_return(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Return);
        self.append_inst(inst_builder);
    }
    pub fn emit_return_value(&mut self, value: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReturnValue);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_unreachable(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::Unreachable);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_start(&mut self, pointer: IdRef, size: i32) {
        let mut inst_builder = InstBuilder::new(spirv::Op::LifetimeStart);
        pointer.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_stop(&mut self, pointer: IdRef, size: i32) {
        let mut inst_builder = InstBuilder::new(spirv::Op::LifetimeStop);
        pointer.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_async_copy(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        destination: IdRef,
        source: IdRef,
        num_elements: IdRef,
        stride: IdRef,
        event: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupAsyncCopy);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        destination.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        num_elements.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_wait_events(&mut self, execution: ValueOrConstant, num_events: IdRef, events_list: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupWaitEvents);
        execution.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        events_list.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_all(&mut self, result_type: Type, execution: ValueOrConstant, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupAll);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_any(&mut self, result_type: Type, execution: ValueOrConstant, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupAny);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_broadcast(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        local_id: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        local_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_i_add(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupIAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_add(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupUMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupSMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupUMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupSMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_read_pipe(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        pointer: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReadPipe);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_write_pipe(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        pointer: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::WritePipe);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_reserved_read_pipe(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        reserve_id: IdRef,
        index: IdRef,
        pointer: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReservedReadPipe);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_reserved_write_pipe(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        reserve_id: IdRef,
        index: IdRef,
        pointer: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReservedWritePipe);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_reserve_read_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        num_packets: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReserveReadPipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_reserve_write_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        num_packets: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReserveWritePipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_commit_read_pipe(
        &mut self,
        pipe: IdRef,
        reserve_id: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CommitReadPipe);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_commit_write_pipe(
        &mut self,
        pipe: IdRef,
        reserve_id: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CommitWritePipe);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_valid_reserve_id(&mut self, result_type: Type, reserve_id: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsValidReserveId);
        inst_builder.set_result(result_type);
        reserve_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_num_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetNumPipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_max_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetMaxPipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_reserve_read_pipe_packets(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        pipe: IdRef,
        num_packets: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupReserveReadPipePackets);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_reserve_write_pipe_packets(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        pipe: IdRef,
        num_packets: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupReserveWritePipePackets);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_commit_read_pipe(
        &mut self,
        execution: ValueOrConstant,
        pipe: IdRef,
        reserve_id: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupCommitReadPipe);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_commit_write_pipe(
        &mut self,
        execution: ValueOrConstant,
        pipe: IdRef,
        reserve_id: IdRef,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupCommitWritePipe);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_enqueue_marker(
        &mut self,
        result_type: Type,
        queue: IdRef,
        num_events: IdRef,
        wait_events: IdRef,
        ret_event: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::EnqueueMarker);
        inst_builder.set_result(result_type);
        queue.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        wait_events.write_operand(&mut inst_builder);
        ret_event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_enqueue_kernel(
        &mut self,
        result_type: Type,
        queue: IdRef,
        flags: IdRef,
        nd_range: IdRef,
        num_events: IdRef,
        wait_events: IdRef,
        ret_event: IdRef,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
        local_size: &[IdRef],
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::EnqueueKernel);
        inst_builder.set_result(result_type);
        queue.write_operand(&mut inst_builder);
        flags.write_operand(&mut inst_builder);
        nd_range.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        wait_events.write_operand(&mut inst_builder);
        ret_event.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        local_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_n_drange_sub_group_count(
        &mut self,
        result_type: Type,
        nd_range: IdRef,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelNDrangeSubGroupCount);
        inst_builder.set_result(result_type);
        nd_range.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_n_drange_max_sub_group_size(
        &mut self,
        result_type: Type,
        nd_range: IdRef,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelNDrangeMaxSubGroupSize);
        inst_builder.set_result(result_type);
        nd_range.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_work_group_size(
        &mut self,
        result_type: Type,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelWorkGroupSize);
        inst_builder.set_result(result_type);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_preferred_work_group_size_multiple(
        &mut self,
        result_type: Type,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelPreferredWorkGroupSizeMultiple);
        inst_builder.set_result(result_type);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_retain_event(&mut self, event: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::RetainEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_release_event(&mut self, event: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReleaseEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_create_user_event(&mut self, result_type: Type) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CreateUserEvent);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_valid_event(&mut self, result_type: Type, event: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsValidEvent);
        inst_builder.set_result(result_type);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_set_user_event_status(&mut self, event: IdRef, status: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SetUserEventStatus);
        event.write_operand(&mut inst_builder);
        status.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capture_event_profiling_info(&mut self, event: IdRef, profiling_info: IdRef, value: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CaptureEventProfilingInfo);
        event.write_operand(&mut inst_builder);
        profiling_info.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_get_default_queue(&mut self, result_type: Type) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetDefaultQueue);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_build_nd_range(
        &mut self,
        result_type: Type,
        global_work_size: IdRef,
        local_work_size: IdRef,
        global_work_offset: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::BuildNDRange);
        inst_builder.set_result(result_type);
        global_work_size.write_operand(&mut inst_builder);
        local_work_size.write_operand(&mut inst_builder);
        global_work_offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleDrefImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleDrefExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleProjImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        image_operands4: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleProjExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleProjDrefImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: ImageOperands,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseSampleProjDrefExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_fetch(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseFetch);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_gather(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        component: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseGather);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_dref_gather(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        d_ref: IdRef,
        image_operands5: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseDrefGather);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_texels_resident(&mut self, result_type: Type, resident_code: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseTexelsResident);
        inst_builder.set_result(result_type);
        resident_code.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_no_line(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::NoLine);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_flag_test_and_set(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicFlagTestAndSet);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_flag_clear(&mut self, pointer: IdRef, memory: ValueOrConstant, semantics: ValueOrConstant) {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicFlagClear);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image_sparse_read(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        image_operands4: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSparseRead);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_size_of(&mut self, result_type: Type, pointer: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SizeOf);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_constant_pipe_storage(
        &mut self,
        result_type: Type,
        packet_size: i32,
        packet_alignment: i32,
        capacity: i32,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConstantPipeStorage);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        capacity.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_create_pipe_from_pipe_storage(&mut self, result_type: Type, pipe_storage: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CreatePipeFromPipeStorage);
        inst_builder.set_result(result_type);
        pipe_storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_local_size_for_subgroup_count(
        &mut self,
        result_type: Type,
        subgroup_count: IdRef,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelLocalSizeForSubgroupCount);
        inst_builder.set_result(result_type);
        subgroup_count.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_max_num_subgroups(
        &mut self,
        result_type: Type,
        invoke: IdRef,
        param: IdRef,
        param_size: IdRef,
        param_align: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GetKernelMaxNumSubgroups);
        inst_builder.set_result(result_type);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_named_barrier_initialize(&mut self, result_type: Type, subgroup_count: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::NamedBarrierInitialize);
        inst_builder.set_result(result_type);
        subgroup_count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_memory_named_barrier(
        &mut self,
        named_barrier: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemoryNamedBarrier);
        named_barrier.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_module_processed(&mut self, process: &str) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ModuleProcessed);
        process.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode_id(&mut self, entry_point: IdRef, mode: spirv::ExecutionMode) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ExecutionModeId);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_id(&mut self, target: IdRef, decoration1: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::DecorateId);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_non_uniform_elect(&mut self, result_type: Type, execution: ValueOrConstant) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformElect);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_all(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        predicate: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformAll);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_any(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        predicate: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformAny);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_all_equal(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformAllEqual);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_broadcast(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        id: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_broadcast_first(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBroadcastFirst);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        predicate: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBallot);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_inverse_ballot(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformInverseBallot);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_bit_extract(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        index: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBallotBitExtract);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_bit_count(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBallotBitCount);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_find_lsb(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBallotFindLSB);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_find_msb(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBallotFindMSB);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        id: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformShuffle);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_xor(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        mask: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformShuffleXor);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        mask.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_up(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        delta: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformShuffleUp);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_down(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        delta: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformShuffleDown);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_i_add(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformIAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_f_add(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformFAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_i_mul(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformIMul);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_f_mul(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformFMul);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_s_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformSMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_u_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformUMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_f_min(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformFMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_s_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformSMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_u_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformUMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_f_max(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformFMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_bitwise_and(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBitwiseAnd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_bitwise_or(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBitwiseOr);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_bitwise_xor(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformBitwiseXor);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_logical_and(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformLogicalAnd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_logical_or(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformLogicalOr);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_logical_xor(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        value: IdRef,
        cluster_size: Option<IdRef>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformLogicalXor);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_quad_broadcast(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        index: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformQuadBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_quad_swap(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        value: IdRef,
        direction: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformQuadSwap);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_copy_logical(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CopyLogical);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::PtrEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_not_equal(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::PtrNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_diff(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::PtrDiff);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_terminate_invocation(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::TerminateInvocation);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_ballot_khr(&mut self, result_type: Type, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupBallotKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_first_invocation_khr(&mut self, result_type: Type, value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupFirstInvocationKHR);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_all_khr(&mut self, result_type: Type, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupAllKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_any_khr(&mut self, result_type: Type, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupAnyKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_all_equal_khr(&mut self, result_type: Type, predicate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupAllEqualKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_read_invocation_khr(&mut self, result_type: Type, value: IdRef, index: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupReadInvocationKHR);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_trace_ray_khr(
        &mut self,
        accel: IdRef,
        ray_flags: IdRef,
        cull_mask: IdRef,
        sbt_offset: IdRef,
        sbt_stride: IdRef,
        miss_index: IdRef,
        ray_origin: IdRef,
        ray_tmin: IdRef,
        ray_direction: IdRef,
        ray_tmax: IdRef,
        payload: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::TraceRayKHR);
        accel.write_operand(&mut inst_builder);
        ray_flags.write_operand(&mut inst_builder);
        cull_mask.write_operand(&mut inst_builder);
        sbt_offset.write_operand(&mut inst_builder);
        sbt_stride.write_operand(&mut inst_builder);
        miss_index.write_operand(&mut inst_builder);
        ray_origin.write_operand(&mut inst_builder);
        ray_tmin.write_operand(&mut inst_builder);
        ray_direction.write_operand(&mut inst_builder);
        ray_tmax.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execute_callable_khr(&mut self, sbt_index: IdRef, callable_data: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ExecuteCallableKHR);
        sbt_index.write_operand(&mut inst_builder);
        callable_data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_convert_u_to_acceleration_structure_khr(&mut self, result_type: Type, accel: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ConvertUToAccelerationStructureKHR);
        inst_builder.set_result(result_type);
        accel.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ignore_intersection_khr(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::IgnoreIntersectionKHR);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_khr(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::TerminateRayKHR);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_initialize_khr(
        &mut self,
        ray_query: IdRef,
        accel: IdRef,
        ray_flags: IdRef,
        cull_mask: IdRef,
        ray_origin: IdRef,
        ray_t_min: IdRef,
        ray_direction: IdRef,
        ray_t_max: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryInitializeKHR);
        ray_query.write_operand(&mut inst_builder);
        accel.write_operand(&mut inst_builder);
        ray_flags.write_operand(&mut inst_builder);
        cull_mask.write_operand(&mut inst_builder);
        ray_origin.write_operand(&mut inst_builder);
        ray_t_min.write_operand(&mut inst_builder);
        ray_direction.write_operand(&mut inst_builder);
        ray_t_max.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_terminate_khr(&mut self, ray_query: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryTerminateKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_generate_intersection_khr(&mut self, ray_query: IdRef, hit_t: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGenerateIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        hit_t.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_confirm_intersection_khr(&mut self, ray_query: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryConfirmIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_proceed_khr(&mut self, result_type: Type, ray_query: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryProceedKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_type_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionTypeKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_i_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupIAddNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFAddNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupUMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupSMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupFMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupUMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ValueOrConstant,
        operation: spirv::GroupOperation,
        x: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupSMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fragment_mask_fetch_amd(&mut self, result_type: Type, image: IdRef, coordinate: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FragmentMaskFetchAMD);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fragment_fetch_amd(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        fragment_index: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FragmentFetchAMD);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        fragment_index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_read_clock_khr(&mut self, result_type: Type, scope: ValueOrConstant) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReadClockKHR);
        inst_builder.set_result(result_type);
        scope.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_footprint_nv(
        &mut self,
        result_type: Type,
        sampled_image: IdRef,
        coordinate: IdRef,
        granularity: IdRef,
        coarse: IdRef,
        image_operands6: Option<ImageOperands>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ImageSampleFootprintNV);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        granularity.write_operand(&mut inst_builder);
        coarse.write_operand(&mut inst_builder);
        image_operands6.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_partition_nv(&mut self, result_type: Type, value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::GroupNonUniformPartitionNV);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_write_packed_primitive_indices4x8_nv(&mut self, index_offset: IdRef, packed_indices: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::WritePackedPrimitiveIndices4x8NV);
        index_offset.write_operand(&mut inst_builder);
        packed_indices.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_report_intersection_nv(&mut self, result_type: Type, hit: IdRef, hit_kind: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReportIntersectionNV);
        inst_builder.set_result(result_type);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_report_intersection_khr(&mut self, result_type: Type, hit: IdRef, hit_kind: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReportIntersectionKHR);
        inst_builder.set_result(result_type);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ignore_intersection_nv(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::IgnoreIntersectionNV);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_nv(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::TerminateRayNV);
        self.append_inst(inst_builder);
    }
    pub fn emit_trace_nv(
        &mut self,
        accel: IdRef,
        ray_flags: IdRef,
        cull_mask: IdRef,
        sbt_offset: IdRef,
        sbt_stride: IdRef,
        miss_index: IdRef,
        ray_origin: IdRef,
        ray_tmin: IdRef,
        ray_direction: IdRef,
        ray_tmax: IdRef,
        payload_id: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::TraceNV);
        accel.write_operand(&mut inst_builder);
        ray_flags.write_operand(&mut inst_builder);
        cull_mask.write_operand(&mut inst_builder);
        sbt_offset.write_operand(&mut inst_builder);
        sbt_stride.write_operand(&mut inst_builder);
        miss_index.write_operand(&mut inst_builder);
        ray_origin.write_operand(&mut inst_builder);
        ray_tmin.write_operand(&mut inst_builder);
        ray_direction.write_operand(&mut inst_builder);
        ray_tmax.write_operand(&mut inst_builder);
        payload_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execute_callable_nv(&mut self, sbt_index: IdRef, callable_data_id: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::ExecuteCallableNV);
        sbt_index.write_operand(&mut inst_builder);
        callable_data_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_cooperative_matrix_load_nv(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        stride: IdRef,
        column_major: IdRef,
        memory_access5: Option<spirv::MemoryAccess>,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CooperativeMatrixLoadNV);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        column_major.write_operand(&mut inst_builder);
        memory_access5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cooperative_matrix_store_nv(
        &mut self,
        pointer: IdRef,
        object: IdRef,
        stride: IdRef,
        column_major: IdRef,
        memory_access4: Option<spirv::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::CooperativeMatrixStoreNV);
        pointer.write_operand(&mut inst_builder);
        object.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        column_major.write_operand(&mut inst_builder);
        memory_access4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_cooperative_matrix_mul_add_nv(&mut self, result_type: Type, a: IdRef, b: IdRef, c: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CooperativeMatrixMulAddNV);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cooperative_matrix_length_nv(&mut self, result_type: Type, r#type: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::CooperativeMatrixLengthNV);
        inst_builder.set_result(result_type);
        r#type.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_begin_invocation_interlock_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::BeginInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_invocation_interlock_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::EndInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_demote_to_helper_invocation_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv::Op::DemoteToHelperInvocationEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_helper_invocation_ext(&mut self, result_type: Type) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IsHelperInvocationEXT);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_intel(&mut self, result_type: Type, data: IdRef, invocation_id: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupShuffleINTEL);
        inst_builder.set_result(result_type);
        data.write_operand(&mut inst_builder);
        invocation_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_down_intel(
        &mut self,
        result_type: Type,
        current: IdRef,
        next: IdRef,
        delta: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupShuffleDownINTEL);
        inst_builder.set_result(result_type);
        current.write_operand(&mut inst_builder);
        next.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_up_intel(
        &mut self,
        result_type: Type,
        previous: IdRef,
        current: IdRef,
        delta: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupShuffleUpINTEL);
        inst_builder.set_result(result_type);
        previous.write_operand(&mut inst_builder);
        current.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_xor_intel(&mut self, result_type: Type, data: IdRef, value: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupShuffleXorINTEL);
        inst_builder.set_result(result_type);
        data.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_block_read_intel(&mut self, result_type: Type, ptr: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupBlockReadINTEL);
        inst_builder.set_result(result_type);
        ptr.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_block_write_intel(&mut self, ptr: IdRef, data: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupBlockWriteINTEL);
        ptr.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_image_block_read_intel(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupImageBlockReadINTEL);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_image_block_write_intel(&mut self, image: IdRef, coordinate: IdRef, data: IdRef) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupImageBlockWriteINTEL);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_image_media_block_read_intel(
        &mut self,
        result_type: Type,
        image: IdRef,
        coordinate: IdRef,
        width: IdRef,
        height: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupImageMediaBlockReadINTEL);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_image_media_block_write_intel(
        &mut self,
        image: IdRef,
        coordinate: IdRef,
        width: IdRef,
        height: IdRef,
        data: IdRef,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::SubgroupImageMediaBlockWriteINTEL);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_u_count_leading_zeros_intel(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UCountLeadingZerosINTEL);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_count_trailing_zeros_intel(&mut self, result_type: Type, operand: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UCountTrailingZerosINTEL);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_abs_i_sub_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AbsISubINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_abs_u_sub_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AbsUSubINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add_sat_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IAddSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_add_sat_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UAddSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_average_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IAverageINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_average_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UAverageINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_average_rounded_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IAverageRoundedINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_average_rounded_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UAverageRoundedINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub_sat_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ISubSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_sub_sat_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::USubSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_mul32x16_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::IMul32x16INTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mul32x16_intel(&mut self, result_type: Type, operand_1: IdRef, operand_2: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::UMul32x16INTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_decorate_string(&mut self, target: IdRef, decoration1: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::DecorateString);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_string_google(&mut self, target: IdRef, decoration1: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::DecorateStringGOOGLE);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate_string(&mut self, struct_type: IdRef, member: i32, decoration2: spirv::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemberDecorateString);
        struct_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate_string_google(
        &mut self,
        struct_type: IdRef,
        member: i32,
        decoration2: spirv::Decoration,
    ) {
        let mut inst_builder = InstBuilder::new(spirv::Op::MemberDecorateStringGOOGLE);
        struct_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_loop_control_intel(&mut self, loop_control_parameters: &[i32]) {
        let mut inst_builder = InstBuilder::new(spirv::Op::LoopControlINTEL);
        loop_control_parameters.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_read_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::ReadPipeBlockingINTEL);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_write_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: IdRef,
        packet_alignment: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::WritePipeBlockingINTEL);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fpga_reg_intel(&mut self, result_type: Type, result: IdRef, input: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::FPGARegINTEL);
        inst_builder.set_result(result_type);
        result.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_ray_t_min_khr(&mut self, result_type: Type, ray_query: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetRayTMinKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_ray_flags_khr(&mut self, result_type: Type, ray_query: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetRayFlagsKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_tkhr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionTKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_custom_index_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionInstanceCustomIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_id_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionInstanceIdKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_shader_binding_table_record_offset_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder =
            InstBuilder::new(spirv::Op::RayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_geometry_index_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionGeometryIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_primitive_index_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionPrimitiveIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_barycentrics_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionBarycentricsKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_front_face_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionFrontFaceKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_candidate_aabb_opaque_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionCandidateAABBOpaqueKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_ray_direction_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionObjectRayDirectionKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_ray_origin_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionObjectRayOriginKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_world_ray_direction_khr(&mut self, result_type: Type, ray_query: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetWorldRayDirectionKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_world_ray_origin_khr(&mut self, result_type: Type, ray_query: IdRef) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetWorldRayOriginKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_to_world_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionObjectToWorldKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_world_to_object_khr(
        &mut self,
        result_type: Type,
        ray_query: IdRef,
        intersection: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::RayQueryGetIntersectionWorldToObjectKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_f_add_ext(
        &mut self,
        result_type: Type,
        pointer: IdRef,
        memory: ValueOrConstant,
        semantics: ValueOrConstant,
        value: IdRef,
    ) -> Value {
        let mut inst_builder = InstBuilder::new(spirv::Op::AtomicFAddEXT);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
}

// --- GLSL.std.450 ---
#[allow(unused_mut)]
impl<'a> FunctionBuilder<'a> {
    pub fn emit_glsl_round(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 1u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_round_even(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 2u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_trunc(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 3u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_abs(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 4u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_abs(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 5u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_sign(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 6u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_sign(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 7u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_floor(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 8u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_ceil(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 9u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_fract(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 10u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_radians(&mut self, result_type: Type, degrees: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 11u32);
        inst_builder.set_result(result_type);
        degrees.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_degrees(&mut self, result_type: Type, radians: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 12u32);
        inst_builder.set_result(result_type);
        radians.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sin(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 13u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cos(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 14u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_tan(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 15u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_asin(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 16u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_acos(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 17u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atan(&mut self, result_type: Type, y_over_x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 18u32);
        inst_builder.set_result(result_type);
        y_over_x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sinh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 19u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cosh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 20u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_tanh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 21u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_asinh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 22u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_acosh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 23u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atanh(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 24u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atan2(&mut self, result_type: Type, y: IdRef, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 25u32);
        inst_builder.set_result(result_type);
        y.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pow(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 26u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_exp(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 27u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_log(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 28u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_exp2(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 29u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_log2(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 30u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sqrt(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 31u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_inverse_sqrt(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 32u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_determinant(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 33u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_matrix_inverse(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 34u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_modf(&mut self, result_type: Type, x: IdRef, i: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 35u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_modf_struct(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 36u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_min(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 37u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_min(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 38u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_min(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 39u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_max(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 40u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_max(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 41u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_max(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 42u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_clamp(&mut self, result_type: Type, x: IdRef, min_val: IdRef, max_val: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 43u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_clamp(&mut self, result_type: Type, x: IdRef, min_val: IdRef, max_val: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 44u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_clamp(&mut self, result_type: Type, x: IdRef, min_val: IdRef, max_val: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 45u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_mix(&mut self, result_type: Type, x: IdRef, y: IdRef, a: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 46u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_i_mix(&mut self, result_type: Type, x: IdRef, y: IdRef, a: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 47u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_step(&mut self, result_type: Type, edge: IdRef, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 48u32);
        inst_builder.set_result(result_type);
        edge.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_smooth_step(&mut self, result_type: Type, edge0: IdRef, edge1: IdRef, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 49u32);
        inst_builder.set_result(result_type);
        edge0.write_operand(&mut inst_builder);
        edge1.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_fma(&mut self, result_type: Type, a: IdRef, b: IdRef, c: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 50u32);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_frexp(&mut self, result_type: Type, x: IdRef, exp: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 51u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_frexp_struct(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 52u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_ldexp(&mut self, result_type: Type, x: IdRef, exp: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 53u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_snorm4x8(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 54u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_unorm4x8(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 55u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_snorm2x16(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 56u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_unorm2x16(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 57u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_half2x16(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 58u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_double2x32(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 59u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_snorm2x16(&mut self, result_type: Type, p: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 60u32);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_unorm2x16(&mut self, result_type: Type, p: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 61u32);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_half2x16(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 62u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_snorm4x8(&mut self, result_type: Type, p: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 63u32);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_unorm4x8(&mut self, result_type: Type, p: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 64u32);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_double2x32(&mut self, result_type: Type, v: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 65u32);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_length(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 66u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_distance(&mut self, result_type: Type, p0: IdRef, p1: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 67u32);
        inst_builder.set_result(result_type);
        p0.write_operand(&mut inst_builder);
        p1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cross(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 68u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_normalize(&mut self, result_type: Type, x: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 69u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_face_forward(&mut self, result_type: Type, n: IdRef, i: IdRef, nref: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 70u32);
        inst_builder.set_result(result_type);
        n.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        nref.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_reflect(&mut self, result_type: Type, i: IdRef, n: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 71u32);
        inst_builder.set_result(result_type);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_refract(&mut self, result_type: Type, i: IdRef, n: IdRef, eta: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 72u32);
        inst_builder.set_result(result_type);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        eta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_i_lsb(&mut self, result_type: Type, value: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 73u32);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_s_msb(&mut self, result_type: Type, value: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 74u32);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_u_msb(&mut self, result_type: Type, value: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 75u32);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_centroid(&mut self, result_type: Type, interpolant: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 76u32);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_sample(&mut self, result_type: Type, interpolant: IdRef, sample: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 77u32);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_offset(&mut self, result_type: Type, interpolant: IdRef, offset: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 78u32);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_min(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 79u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_max(&mut self, result_type: Type, x: IdRef, y: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 80u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_clamp(&mut self, result_type: Type, x: IdRef, min_val: IdRef, max_val: IdRef) -> Value {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 81u32);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
}
