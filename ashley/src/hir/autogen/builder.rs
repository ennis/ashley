// --- Core instructions ---
impl<'a, 'hir> FunctionBuilder<'a, 'hir> {
    pub fn emit_nop(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Nop);
        self.append_inst(inst_builder);
    }
    pub fn emit_undef(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Undef);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_source_continued(&mut self, continued_source: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SourceContinued);
        continued_source.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_source(
        &mut self,
        source_language0: spirv_headers::SourceLanguage,
        version: i32,
        file: Option<ValueId>,
        source: Option<&str>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Source);
        source_language0.write_operand(&mut inst_builder);
        version.write_operand(&mut inst_builder);
        file.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_source_extension(&mut self, extension: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SourceExtension);
        extension.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_name(&mut self, target: ValueId, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Name);
        target.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_name(&mut self, r#type: ValueId, member: i32, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberName);
        r#type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_string(&mut self, string: &str) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::String);
        inst_builder.set_result(result_type);
        string.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_line(&mut self, file: ValueId, line: i32, column: i32) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Line);
        file.write_operand(&mut inst_builder);
        line.write_operand(&mut inst_builder);
        column.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_extension(&mut self, name: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Extension);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ext_inst_import(&mut self, name: &str) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExtInstImport);
        inst_builder.set_result(result_type);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ext_inst(
        &mut self,
        result_type: Type,
        set: ValueId,
        instruction: u32,
        operand_1_operand_2: &[ValueId],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExtInst);
        inst_builder.set_result(result_type);
        set.write_operand(&mut inst_builder);
        instruction.write_operand(&mut inst_builder);
        operand_1_operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_memory_model(
        &mut self,
        addressing_model0: spirv_headers::AddressingModel,
        memory_model1: spirv_headers::MemoryModel,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemoryModel);
        addressing_model0.write_operand(&mut inst_builder);
        memory_model1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_entry_point(
        &mut self,
        execution_model0: spirv_headers::ExecutionModel,
        entry_point: ValueId,
        name: &str,
        interface: &[ValueId],
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EntryPoint);
        execution_model0.write_operand(&mut inst_builder);
        entry_point.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        interface.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode(&mut self, entry_point: ValueId, mode: spirv_headers::ExecutionMode) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecutionMode);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capability(&mut self, capability: spirv_headers::Capability) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Capability);
        capability.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_variable(
        &mut self,
        result_type: Type,
        storage_class2: spirv_headers::StorageClass,
        initializer: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Variable);
        inst_builder.set_result(result_type);
        storage_class2.write_operand(&mut inst_builder);
        initializer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_texel_pointer(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        sample: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageTexelPointer);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_load(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory_access3: Option<spirv_headers::MemoryAccess>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Load);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_store(
        &mut self,
        pointer: ValueId,
        object: ValueId,
        memory_access2: Option<spirv_headers::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Store);
        pointer.write_operand(&mut inst_builder);
        object.write_operand(&mut inst_builder);
        memory_access2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_copy_memory(
        &mut self,
        target: ValueId,
        source: ValueId,
        memory_access2: Option<spirv_headers::MemoryAccess>,
        memory_access3: Option<spirv_headers::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyMemory);
        target.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        memory_access2.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_copy_memory_sized(
        &mut self,
        target: ValueId,
        source: ValueId,
        size: ValueId,
        memory_access3: Option<spirv_headers::MemoryAccess>,
        memory_access4: Option<spirv_headers::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyMemorySized);
        target.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        memory_access4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_access_chain(&mut self, result_type: Type, base: ValueId, indexes: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_in_bounds_access_chain(&mut self, result_type: Type, base: ValueId, indexes: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::InBoundsAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: ValueId,
        element: ValueId,
        indexes: &[ValueId],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_array_length(&mut self, result_type: Type, structure: ValueId, array_member: i32) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArrayLength);
        inst_builder.set_result(result_type);
        structure.write_operand(&mut inst_builder);
        array_member.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_ptr_mem_semantics(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericPtrMemSemantics);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_in_bounds_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: ValueId,
        element: ValueId,
        indexes: &[ValueId],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::InBoundsPtrAccessChain);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_decorate(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Decorate);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate(
        &mut self,
        structure_type: ValueId,
        member: i32,
        decoration2: spirv_headers::Decoration,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberDecorate);
        structure_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decoration_group(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorationGroup);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_decorate(&mut self, decoration_group: ValueId, targets: &[ValueId]) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupDecorate);
        decoration_group.write_operand(&mut inst_builder);
        targets.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_member_decorate(&mut self, decoration_group: ValueId, targets: &[(ValueId, i32)]) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupMemberDecorate);
        decoration_group.write_operand(&mut inst_builder);
        targets.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_vector_extract_dynamic(&mut self, result_type: Type, vector: ValueId, index: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorExtractDynamic);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_insert_dynamic(
        &mut self,
        result_type: Type,
        vector: ValueId,
        component: ValueId,
        index: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorInsertDynamic);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_shuffle(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        components: &[i32],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorShuffle);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_construct(&mut self, result_type: Type, constituents: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeConstruct);
        inst_builder.set_result(result_type);
        constituents.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_extract(&mut self, result_type: Type, composite: ValueId, indexes: &[i32]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeExtract);
        inst_builder.set_result(result_type);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_composite_insert(
        &mut self,
        result_type: Type,
        object: ValueId,
        composite: ValueId,
        indexes: &[i32],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeInsert);
        inst_builder.set_result(result_type);
        object.write_operand(&mut inst_builder);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_copy_object(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyObject);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_transpose(&mut self, result_type: Type, matrix: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Transpose);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sampled_image(&mut self, result_type: Type, image: ValueId, sampler: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SampledImage);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        sampler.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleDrefImplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleDrefExplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjDrefImplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjDrefExplicitLod);
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
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageFetch);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        component: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageGather);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageDrefGather);
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
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageRead);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_write(
        &mut self,
        image: ValueId,
        coordinate: ValueId,
        texel: ValueId,
        image_operands3: Option<ImageOperands>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageWrite);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        texel.write_operand(&mut inst_builder);
        image_operands3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image(&mut self, result_type: Type, sampled_image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Image);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_format(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryFormat);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_order(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryOrder);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_size_lod(
        &mut self,
        result_type: Type,
        image: ValueId,
        level_of_detail: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySizeLod);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        level_of_detail.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_size(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySize);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_lod(&mut self, result_type: Type, sampled_image: ValueId, coordinate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_levels(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryLevels);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_query_samples(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySamples);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_f_to_u(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertFToU);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_f_to_s(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertFToS);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_s_to_f(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSToF);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_f(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToF);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_convert(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UConvert);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_convert(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SConvert);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_convert(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FConvert);
        inst_builder.set_result(result_type);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_quantize_to_f16(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::QuantizeToF16);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_ptr_to_u(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertPtrToU);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sat_convert_s_to_u(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SatConvertSToU);
        inst_builder.set_result(result_type);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sat_convert_u_to_s(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SatConvertUToS);
        inst_builder.set_result(result_type);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_ptr(&mut self, result_type: Type, integer_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToPtr);
        inst_builder.set_result(result_type);
        integer_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_cast_to_generic(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrCastToGeneric);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_cast_to_ptr(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericCastToPtr);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_generic_cast_to_ptr_explicit(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        storage: spirv_headers::StorageClass,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericCastToPtrExplicit);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitcast(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Bitcast);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_negate(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SNegate);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_negate(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FNegate);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAdd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_add(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FAdd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISub);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_sub(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FSub);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_mul(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IMul);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_mul(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FMul);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FDiv);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_rem(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SRem);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_rem(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FRem);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FMod);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_times_scalar(&mut self, result_type: Type, vector: ValueId, scalar: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorTimesScalar);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_scalar(&mut self, result_type: Type, matrix: ValueId, scalar: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesScalar);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_vector_times_matrix(&mut self, result_type: Type, vector: ValueId, matrix: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorTimesMatrix);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_vector(&mut self, result_type: Type, matrix: ValueId, vector: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesVector);
        inst_builder.set_result(result_type);
        matrix.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_matrix_times_matrix(
        &mut self,
        result_type: Type,
        left_matrix: ValueId,
        right_matrix: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesMatrix);
        inst_builder.set_result(result_type);
        left_matrix.write_operand(&mut inst_builder);
        right_matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_outer_product(&mut self, result_type: Type, vector_1: ValueId, vector_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::OuterProduct);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dot(&mut self, result_type: Type, vector_1: ValueId, vector_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Dot);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add_carry(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAddCarry);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub_borrow(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISubBorrow);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mul_extended(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMulExtended);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_mul_extended(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SMulExtended);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_any(&mut self, result_type: Type, vector: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Any);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_all(&mut self, result_type: Type, vector: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::All);
        inst_builder.set_result(result_type);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_nan(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsNan);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_inf(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsInf);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_finite(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsFinite);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_normal(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsNormal);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sign_bit_set(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SignBitSet);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_less_or_greater(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LessOrGreater);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ordered(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Ordered);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_unordered(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Unordered);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_or(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalOr);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_and(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalAnd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_logical_not(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalNot);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_select(
        &mut self,
        result_type: Type,
        condition: ValueId,
        object_1: ValueId,
        object_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Select);
        inst_builder.set_result(result_type);
        condition.write_operand(&mut inst_builder);
        object_1.write_operand(&mut inst_builder);
        object_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::INotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_greater_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_greater_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ULessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ULessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordLessThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordGreaterThan);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_less_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordLessThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_ord_greater_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_f_unord_greater_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordGreaterThanEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_right_logical(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftRightLogical);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_right_arithmetic(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftRightArithmetic);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_shift_left_logical(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftLeftLogical);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_or(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseOr);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_xor(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseXor);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bitwise_and(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseAnd);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_not(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Not);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_insert(
        &mut self,
        result_type: Type,
        base: ValueId,
        insert: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldInsert);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        insert.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_s_extract(
        &mut self,
        result_type: Type,
        base: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldSExtract);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_field_u_extract(
        &mut self,
        result_type: Type,
        base: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldUExtract);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_reverse(&mut self, result_type: Type, base: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitReverse);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_bit_count(&mut self, result_type: Type, base: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitCount);
        inst_builder.set_result(result_type);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdx);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdy);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Fwidth);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdxFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdyFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FwidthFine);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdx_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdxCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_dpdy_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdyCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fwidth_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FwidthCoarse);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_emit_vertex(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitVertex);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_primitive(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndPrimitive);
        self.append_inst(inst_builder);
    }
    pub fn emit_emit_stream_vertex(&mut self, stream: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitStreamVertex);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_stream_primitive(&mut self, stream: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndStreamPrimitive);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_control_barrier(&mut self, execution: ScopeId, memory: ScopeId, semantics: MemorySemanticsId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ControlBarrier);
        execution.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_memory_barrier(&mut self, memory: ScopeId, semantics: MemorySemanticsId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemoryBarrier);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_load(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicLoad);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_store(
        &mut self,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicStore);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_exchange(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicExchange);
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
        pointer: ValueId,
        memory: ScopeId,
        equal: MemorySemanticsId,
        unequal: MemorySemanticsId,
        value: ValueId,
        comparator: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicCompareExchange);
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
        pointer: ValueId,
        memory: ScopeId,
        equal: MemorySemanticsId,
        unequal: MemorySemanticsId,
        value: ValueId,
        comparator: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicCompareExchangeWeak);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIIncrement);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_decrement(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIDecrement);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_i_add(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIAdd);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicISub);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicSMin);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicUMin);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicSMax);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicUMax);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicAnd);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicOr);
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
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicXor);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_phi(&mut self, result_type: Type, variable_parent: &[(ValueId, ValueId)]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Phi);
        inst_builder.set_result(result_type);
        variable_parent.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_loop_merge(
        &mut self,
        merge_block: ValueId,
        continue_target: ValueId,
        loop_control2: spirv_headers::LoopControl,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LoopMerge);
        merge_block.write_operand(&mut inst_builder);
        continue_target.write_operand(&mut inst_builder);
        loop_control2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_selection_merge(&mut self, merge_block: ValueId, selection_control1: spirv_headers::SelectionControl) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SelectionMerge);
        merge_block.write_operand(&mut inst_builder);
        selection_control1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_label(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Label);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_branch(&mut self, target_label: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Branch);
        target_label.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_branch_conditional(
        &mut self,
        condition: ValueId,
        true_label: ValueId,
        false_label: ValueId,
        branch_weights: &[i32],
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BranchConditional);
        condition.write_operand(&mut inst_builder);
        true_label.write_operand(&mut inst_builder);
        false_label.write_operand(&mut inst_builder);
        branch_weights.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_switch(&mut self, selector: ValueId, default: ValueId, target: &[(i32, ValueId)]) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Switch);
        selector.write_operand(&mut inst_builder);
        default.write_operand(&mut inst_builder);
        target.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_kill(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Kill);
        self.append_inst(inst_builder);
    }
    pub fn emit_return(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Return);
        self.append_inst(inst_builder);
    }
    pub fn emit_return_value(&mut self, value: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReturnValue);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_unreachable(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Unreachable);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_start(&mut self, pointer: ValueId, size: i32) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LifetimeStart);
        pointer.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_stop(&mut self, pointer: ValueId, size: i32) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LifetimeStop);
        pointer.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_async_copy(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        destination: ValueId,
        source: ValueId,
        num_elements: ValueId,
        stride: ValueId,
        event: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAsyncCopy);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        destination.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        num_elements.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_wait_events(&mut self, execution: ScopeId, num_events: ValueId, events_list: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupWaitEvents);
        execution.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        events_list.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_all(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAll);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_any(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAny);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_broadcast(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        local_id: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        local_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_i_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFAdd);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMin);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMax);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_read_pipe(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadPipe);
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
        pipe: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePipe);
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
        pipe: ValueId,
        reserve_id: ValueId,
        index: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReservedReadPipe);
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
        pipe: ValueId,
        reserve_id: ValueId,
        index: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReservedWritePipe);
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
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReserveReadPipePackets);
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
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReserveWritePipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_commit_read_pipe(
        &mut self,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CommitReadPipe);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_commit_write_pipe(
        &mut self,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CommitWritePipe);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_valid_reserve_id(&mut self, result_type: Type, reserve_id: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsValidReserveId);
        inst_builder.set_result(result_type);
        reserve_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_num_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetNumPipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_max_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetMaxPipePackets);
        inst_builder.set_result(result_type);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_reserve_read_pipe_packets(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupReserveReadPipePackets);
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
        execution: ScopeId,
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupReserveWritePipePackets);
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
        execution: ScopeId,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupCommitReadPipe);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_commit_write_pipe(
        &mut self,
        execution: ScopeId,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupCommitWritePipe);
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
        queue: ValueId,
        num_events: ValueId,
        wait_events: ValueId,
        ret_event: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EnqueueMarker);
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
        queue: ValueId,
        flags: ValueId,
        nd_range: ValueId,
        num_events: ValueId,
        wait_events: ValueId,
        ret_event: ValueId,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
        local_size: &[ValueId],
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EnqueueKernel);
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
        nd_range: ValueId,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelNDrangeSubGroupCount);
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
        nd_range: ValueId,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelNDrangeMaxSubGroupSize);
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
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelWorkGroupSize);
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
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelPreferredWorkGroupSizeMultiple);
        inst_builder.set_result(result_type);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_retain_event(&mut self, event: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RetainEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_release_event(&mut self, event: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReleaseEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_create_user_event(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CreateUserEvent);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_is_valid_event(&mut self, result_type: Type, event: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsValidEvent);
        inst_builder.set_result(result_type);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_set_user_event_status(&mut self, event: ValueId, status: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SetUserEventStatus);
        event.write_operand(&mut inst_builder);
        status.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capture_event_profiling_info(&mut self, event: ValueId, profiling_info: ValueId, value: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CaptureEventProfilingInfo);
        event.write_operand(&mut inst_builder);
        profiling_info.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_get_default_queue(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetDefaultQueue);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_build_nd_range(
        &mut self,
        result_type: Type,
        global_work_size: ValueId,
        local_work_size: ValueId,
        global_work_offset: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BuildNDRange);
        inst_builder.set_result(result_type);
        global_work_size.write_operand(&mut inst_builder);
        local_work_size.write_operand(&mut inst_builder);
        global_work_offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleDrefImplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleDrefExplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjImplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjExplicitLod);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjDrefImplicitLod);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjDrefExplicitLod);
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
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseFetch);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        component: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseGather);
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
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseDrefGather);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sparse_texels_resident(&mut self, result_type: Type, resident_code: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseTexelsResident);
        inst_builder.set_result(result_type);
        resident_code.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_no_line(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::NoLine);
        self.append_inst(inst_builder);
    }
    pub fn emit_atomic_flag_test_and_set(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFlagTestAndSet);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_flag_clear(&mut self, pointer: ValueId, memory: ScopeId, semantics: MemorySemanticsId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFlagClear);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image_sparse_read(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseRead);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_size_of(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SizeOf);
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
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConstantPipeStorage);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        capacity.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_create_pipe_from_pipe_storage(&mut self, result_type: Type, pipe_storage: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CreatePipeFromPipeStorage);
        inst_builder.set_result(result_type);
        pipe_storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_get_kernel_local_size_for_subgroup_count(
        &mut self,
        result_type: Type,
        subgroup_count: ValueId,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelLocalSizeForSubgroupCount);
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
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelMaxNumSubgroups);
        inst_builder.set_result(result_type);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_named_barrier_initialize(&mut self, result_type: Type, subgroup_count: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::NamedBarrierInitialize);
        inst_builder.set_result(result_type);
        subgroup_count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_memory_named_barrier(&mut self, named_barrier: ValueId, memory: ScopeId, semantics: MemorySemanticsId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemoryNamedBarrier);
        named_barrier.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_module_processed(&mut self, process: &str) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ModuleProcessed);
        process.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode_id(&mut self, entry_point: ValueId, mode: spirv_headers::ExecutionMode) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecutionModeId);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_id(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorateId);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_non_uniform_elect(&mut self, result_type: Type, execution: ScopeId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformElect);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_all(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAll);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_any(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAny);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_all_equal(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAllEqual);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_broadcast(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        id: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_broadcast_first(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBroadcastFirst);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        predicate: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallot);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_inverse_ballot(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformInverseBallot);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_bit_extract(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        index: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotBitExtract);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_bit_count(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotBitCount);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_find_lsb(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotFindLSB);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_ballot_find_msb(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotFindMSB);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        id: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffle);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_xor(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        mask: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleXor);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        mask.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_up(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleUp);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_shuffle_down(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleDown);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_i_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformIAdd);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFAdd);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformIMul);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMul);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformSMin);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformUMin);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMin);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformSMax);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformUMax);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMax);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseAnd);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseOr);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseXor);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalAnd);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalOr);
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
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalXor);
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
        execution: ScopeId,
        value: ValueId,
        index: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformQuadBroadcast);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_quad_swap(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformQuadSwap);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_copy_logical(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyLogical);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrNotEqual);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_diff(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrDiff);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_terminate_invocation(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TerminateInvocation);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_ballot_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupBallotKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_first_invocation_khr(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupFirstInvocationKHR);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_all_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAllKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_any_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAnyKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_all_equal_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAllEqualKHR);
        inst_builder.set_result(result_type);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_non_uniform_rotate_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformRotateKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_read_invocation_khr(&mut self, result_type: Type, value: ValueId, index: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupReadInvocationKHR);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_trace_ray_khr(
        &mut self,
        accel: ValueId,
        ray_flags: ValueId,
        cull_mask: ValueId,
        sbt_offset: ValueId,
        sbt_stride: ValueId,
        miss_index: ValueId,
        ray_origin: ValueId,
        ray_tmin: ValueId,
        ray_direction: ValueId,
        ray_tmax: ValueId,
        payload: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TraceRayKHR);
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
    pub fn emit_execute_callable_khr(&mut self, sbt_index: ValueId, callable_data: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecuteCallableKHR);
        sbt_index.write_operand(&mut inst_builder);
        callable_data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_convert_u_to_acceleration_structure_khr(&mut self, result_type: Type, accel: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToAccelerationStructureKHR);
        inst_builder.set_result(result_type);
        accel.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ignore_intersection_khr(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IgnoreIntersectionKHR);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_khr(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TerminateRayKHR);
        self.append_inst(inst_builder);
    }
    pub fn emit_s_dot(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDot);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_dot(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDot);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_su_dot(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDot);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_su_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotAccSat);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_s_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotAccSatKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotAccSat);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotAccSatKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_su_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotAccSat);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_su_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotAccSatKHR);
        inst_builder.set_result(result_type);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_ray_query_khr(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeRayQueryKHR);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_initialize_khr(
        &mut self,
        ray_query: ValueId,
        accel: ValueId,
        ray_flags: ValueId,
        cull_mask: ValueId,
        ray_origin: ValueId,
        ray_t_min: ValueId,
        ray_direction: ValueId,
        ray_t_max: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryInitializeKHR);
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
    pub fn emit_ray_query_terminate_khr(&mut self, ray_query: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryTerminateKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_generate_intersection_khr(&mut self, ray_query: ValueId, hit_t: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGenerateIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        hit_t.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_confirm_intersection_khr(&mut self, ray_query: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryConfirmIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_proceed_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryProceedKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_type_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionTypeKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_i_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIAddNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFAddNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMinNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_u_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_s_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMaxNonUniformAMD);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fragment_mask_fetch_amd(&mut self, result_type: Type, image: ValueId, coordinate: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FragmentMaskFetchAMD);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fragment_fetch_amd(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        fragment_index: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FragmentFetchAMD);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        fragment_index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_read_clock_khr(&mut self, result_type: Type, scope: ScopeId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadClockKHR);
        inst_builder.set_result(result_type);
        scope.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_record_hit_motion_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        instance_id: ValueId,
        primitive_id: ValueId,
        geometry_index: ValueId,
        hit_kind: ValueId,
        sbt_record_offset: ValueId,
        sbt_record_stride: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        current_time: ValueId,
        hit_object_attributes: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordHitMotionNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        instance_id.write_operand(&mut inst_builder);
        primitive_id.write_operand(&mut inst_builder);
        geometry_index.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        sbt_record_offset.write_operand(&mut inst_builder);
        sbt_record_stride.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        current_time.write_operand(&mut inst_builder);
        hit_object_attributes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_record_hit_with_index_motion_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        instance_id: ValueId,
        primitive_id: ValueId,
        geometry_index: ValueId,
        hit_kind: ValueId,
        sbt_record_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        current_time: ValueId,
        hit_object_attributes: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordHitWithIndexMotionNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        instance_id.write_operand(&mut inst_builder);
        primitive_id.write_operand(&mut inst_builder);
        geometry_index.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        sbt_record_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        current_time.write_operand(&mut inst_builder);
        hit_object_attributes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_record_miss_motion_nv(
        &mut self,
        hit_object: ValueId,
        sbt_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        current_time: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordMissMotionNV);
        hit_object.write_operand(&mut inst_builder);
        sbt_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        current_time.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_world_to_object_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldToObjectNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_object_to_world_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectToWorldNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_object_ray_direction_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectRayDirectionNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_object_ray_origin_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectRayOriginNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_trace_ray_motion_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        ray_flags: ValueId,
        cullmask: ValueId,
        sbt_record_offset: ValueId,
        sbt_record_stride: ValueId,
        miss_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        time: ValueId,
        payload: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectTraceRayMotionNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        ray_flags.write_operand(&mut inst_builder);
        cullmask.write_operand(&mut inst_builder);
        sbt_record_offset.write_operand(&mut inst_builder);
        sbt_record_stride.write_operand(&mut inst_builder);
        miss_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        time.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_shader_record_buffer_handle_nv(
        &mut self,
        result_type: Type,
        hit_object: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetShaderRecordBufferHandleNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_shader_binding_table_record_index_nv(
        &mut self,
        result_type: Type,
        hit_object: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetShaderBindingTableRecordIndexNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_record_empty_nv(&mut self, hit_object: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordEmptyNV);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_trace_ray_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        ray_flags: ValueId,
        cullmask: ValueId,
        sbt_record_offset: ValueId,
        sbt_record_stride: ValueId,
        miss_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        payload: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectTraceRayNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        ray_flags.write_operand(&mut inst_builder);
        cullmask.write_operand(&mut inst_builder);
        sbt_record_offset.write_operand(&mut inst_builder);
        sbt_record_stride.write_operand(&mut inst_builder);
        miss_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_record_hit_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        instance_id: ValueId,
        primitive_id: ValueId,
        geometry_index: ValueId,
        hit_kind: ValueId,
        sbt_record_offset: ValueId,
        sbt_record_stride: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        hit_object_attributes: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordHitNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        instance_id.write_operand(&mut inst_builder);
        primitive_id.write_operand(&mut inst_builder);
        geometry_index.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        sbt_record_offset.write_operand(&mut inst_builder);
        sbt_record_stride.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        hit_object_attributes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_record_hit_with_index_nv(
        &mut self,
        hit_object: ValueId,
        acceleration_structure: ValueId,
        instance_id: ValueId,
        primitive_id: ValueId,
        geometry_index: ValueId,
        hit_kind: ValueId,
        sbt_record_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
        hit_object_attributes: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordHitWithIndexNV);
        hit_object.write_operand(&mut inst_builder);
        acceleration_structure.write_operand(&mut inst_builder);
        instance_id.write_operand(&mut inst_builder);
        primitive_id.write_operand(&mut inst_builder);
        geometry_index.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        sbt_record_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        hit_object_attributes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_record_miss_nv(
        &mut self,
        hit_object: ValueId,
        sbt_index: ValueId,
        origin: ValueId,
        t_min: ValueId,
        direction: ValueId,
        t_max: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectRecordMissNV);
        hit_object.write_operand(&mut inst_builder);
        sbt_index.write_operand(&mut inst_builder);
        origin.write_operand(&mut inst_builder);
        t_min.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        t_max.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_execute_shader_nv(&mut self, hit_object: ValueId, payload: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectExecuteShaderNV);
        hit_object.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_current_time_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetCurrentTimeNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_attributes_nv(&mut self, hit_object: ValueId, hit_object_attribute: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetAttributesNV);
        hit_object.write_operand(&mut inst_builder);
        hit_object_attribute.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_hit_kind_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetHitKindNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_primitive_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetPrimitiveIndexNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_geometry_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetGeometryIndexNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_instance_id_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetInstanceIdNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_instance_custom_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetInstanceCustomIndexNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_world_ray_direction_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldRayDirectionNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_world_ray_origin_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldRayOriginNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_ray_t_max_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetRayTMaxNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_get_ray_t_min_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetRayTMinNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_is_empty_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsEmptyNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_is_hit_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsHitNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_hit_object_is_miss_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsMissNV);
        inst_builder.set_result(result_type);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_reorder_thread_with_hit_object_nv(
        &mut self,
        hit_object: ValueId,
        hint: Option<ValueId>,
        bits: Option<ValueId>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReorderThreadWithHitObjectNV);
        hit_object.write_operand(&mut inst_builder);
        hint.write_operand(&mut inst_builder);
        bits.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_reorder_thread_with_hint_nv(&mut self, hint: ValueId, bits: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReorderThreadWithHintNV);
        hint.write_operand(&mut inst_builder);
        bits.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_type_hit_object_nv(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeHitObjectNV);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_image_sample_footprint_nv(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        granularity: ValueId,
        coarse: ValueId,
        image_operands6: Option<ImageOperands>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleFootprintNV);
        inst_builder.set_result(result_type);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        granularity.write_operand(&mut inst_builder);
        coarse.write_operand(&mut inst_builder);
        image_operands6.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_emit_mesh_tasks_ext(
        &mut self,
        group_count_x: ValueId,
        group_count_y: ValueId,
        group_count_z: ValueId,
        payload: Option<ValueId>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitMeshTasksEXT);
        group_count_x.write_operand(&mut inst_builder);
        group_count_y.write_operand(&mut inst_builder);
        group_count_z.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_set_mesh_outputs_ext(&mut self, vertex_count: ValueId, primitive_count: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SetMeshOutputsEXT);
        vertex_count.write_operand(&mut inst_builder);
        primitive_count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_non_uniform_partition_nv(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformPartitionNV);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_write_packed_primitive_indices4x8_nv(&mut self, index_offset: ValueId, packed_indices: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePackedPrimitiveIndices4x8NV);
        index_offset.write_operand(&mut inst_builder);
        packed_indices.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_report_intersection_nv(&mut self, result_type: Type, hit: ValueId, hit_kind: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReportIntersectionNV);
        inst_builder.set_result(result_type);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_report_intersection_khr(&mut self, result_type: Type, hit: ValueId, hit_kind: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReportIntersectionKHR);
        inst_builder.set_result(result_type);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ignore_intersection_nv(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IgnoreIntersectionNV);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_nv(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TerminateRayNV);
        self.append_inst(inst_builder);
    }
    pub fn emit_trace_nv(
        &mut self,
        accel: ValueId,
        ray_flags: ValueId,
        cull_mask: ValueId,
        sbt_offset: ValueId,
        sbt_stride: ValueId,
        miss_index: ValueId,
        ray_origin: ValueId,
        ray_tmin: ValueId,
        ray_direction: ValueId,
        ray_tmax: ValueId,
        payload_id: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TraceNV);
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
    pub fn emit_trace_motion_nv(
        &mut self,
        accel: ValueId,
        ray_flags: ValueId,
        cull_mask: ValueId,
        sbt_offset: ValueId,
        sbt_stride: ValueId,
        miss_index: ValueId,
        ray_origin: ValueId,
        ray_tmin: ValueId,
        ray_direction: ValueId,
        ray_tmax: ValueId,
        time: ValueId,
        payload_id: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TraceMotionNV);
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
        time.write_operand(&mut inst_builder);
        payload_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_trace_ray_motion_nv(
        &mut self,
        accel: ValueId,
        ray_flags: ValueId,
        cull_mask: ValueId,
        sbt_offset: ValueId,
        sbt_stride: ValueId,
        miss_index: ValueId,
        ray_origin: ValueId,
        ray_tmin: ValueId,
        ray_direction: ValueId,
        ray_tmax: ValueId,
        time: ValueId,
        payload: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TraceRayMotionNV);
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
        time.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_type_acceleration_structure_nv(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAccelerationStructureNV);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_acceleration_structure_khr(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAccelerationStructureKHR);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_execute_callable_nv(&mut self, sbt_index: ValueId, callable_data_id: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecuteCallableNV);
        sbt_index.write_operand(&mut inst_builder);
        callable_data_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_type_cooperative_matrix_nv(
        &mut self,
        component_type: ValueId,
        execution: ScopeId,
        rows: ValueId,
        columns: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeCooperativeMatrixNV);
        inst_builder.set_result(result_type);
        component_type.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        rows.write_operand(&mut inst_builder);
        columns.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cooperative_matrix_load_nv(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        stride: ValueId,
        column_major: ValueId,
        memory_access5: Option<spirv_headers::MemoryAccess>,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixLoadNV);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        column_major.write_operand(&mut inst_builder);
        memory_access5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cooperative_matrix_store_nv(
        &mut self,
        pointer: ValueId,
        object: ValueId,
        stride: ValueId,
        column_major: ValueId,
        memory_access4: Option<spirv_headers::MemoryAccess>,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixStoreNV);
        pointer.write_operand(&mut inst_builder);
        object.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        column_major.write_operand(&mut inst_builder);
        memory_access4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_cooperative_matrix_mul_add_nv(
        &mut self,
        result_type: Type,
        a: ValueId,
        b: ValueId,
        c: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixMulAddNV);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cooperative_matrix_length_nv(&mut self, result_type: Type, r#type: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixLengthNV);
        inst_builder.set_result(result_type);
        r#type.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_begin_invocation_interlock_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BeginInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_invocation_interlock_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_demote_to_helper_invocation(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DemoteToHelperInvocation);
        self.append_inst(inst_builder);
    }
    pub fn emit_demote_to_helper_invocation_ext(&mut self) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DemoteToHelperInvocationEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_helper_invocation_ext(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsHelperInvocationEXT);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_image_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToImageNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_sampler_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToSamplerNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_image_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertImageToUNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_sampler_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSamplerToUNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_u_to_sampled_image_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToSampledImageNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_convert_sampled_image_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSampledImageToUNV);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_sampler_image_addressing_mode_nv(&mut self, bit_width: i32) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SamplerImageAddressingModeNV);
        bit_width.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_shuffle_intel(&mut self, result_type: Type, data: ValueId, invocation_id: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleINTEL);
        inst_builder.set_result(result_type);
        data.write_operand(&mut inst_builder);
        invocation_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_down_intel(
        &mut self,
        result_type: Type,
        current: ValueId,
        next: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleDownINTEL);
        inst_builder.set_result(result_type);
        current.write_operand(&mut inst_builder);
        next.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_up_intel(
        &mut self,
        result_type: Type,
        previous: ValueId,
        current: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleUpINTEL);
        inst_builder.set_result(result_type);
        previous.write_operand(&mut inst_builder);
        current.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_shuffle_xor_intel(&mut self, result_type: Type, data: ValueId, value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleXorINTEL);
        inst_builder.set_result(result_type);
        data.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_block_read_intel(&mut self, result_type: Type, ptr: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupBlockReadINTEL);
        inst_builder.set_result(result_type);
        ptr.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_block_write_intel(&mut self, ptr: ValueId, data: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupBlockWriteINTEL);
        ptr.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_image_block_read_intel(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageBlockReadINTEL);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_image_block_write_intel(&mut self, image: ValueId, coordinate: ValueId, data: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageBlockWriteINTEL);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_image_media_block_read_intel(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        width: ValueId,
        height: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageMediaBlockReadINTEL);
        inst_builder.set_result(result_type);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_image_media_block_write_intel(
        &mut self,
        image: ValueId,
        coordinate: ValueId,
        width: ValueId,
        height: ValueId,
        data: ValueId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageMediaBlockWriteINTEL);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_u_count_leading_zeros_intel(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UCountLeadingZerosINTEL);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_count_trailing_zeros_intel(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UCountTrailingZerosINTEL);
        inst_builder.set_result(result_type);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_abs_i_sub_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AbsISubINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_abs_u_sub_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AbsUSubINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_add_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAddSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_add_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAddSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_average_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAverageINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_average_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAverageINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_average_rounded_intel(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAverageRoundedINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_average_rounded_intel(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAverageRoundedINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_sub_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISubSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_sub_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::USubSatINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_i_mul32x16_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IMul32x16INTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_u_mul32x16_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMul32x16INTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_constant_function_pointer_intel(&mut self, result_type: Type, function: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConstantFunctionPointerINTEL);
        inst_builder.set_result(result_type);
        function.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_function_pointer_call_intel(&mut self, result_type: Type, operand_1: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FunctionPointerCallINTEL);
        inst_builder.set_result(result_type);
        operand_1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_asm_target_intel(&mut self, result_type: Type, asm_target: &str) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmTargetINTEL);
        inst_builder.set_result(result_type);
        asm_target.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_asm_intel(
        &mut self,
        result_type: Type,
        asm_type: ValueId,
        target: ValueId,
        asm_instructions: &str,
        constraints: &str,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmINTEL);
        inst_builder.set_result(result_type);
        asm_type.write_operand(&mut inst_builder);
        target.write_operand(&mut inst_builder);
        asm_instructions.write_operand(&mut inst_builder);
        constraints.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_asm_call_intel(&mut self, result_type: Type, asm: ValueId, argument_0: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmCallINTEL);
        inst_builder.set_result(result_type);
        asm.write_operand(&mut inst_builder);
        argument_0.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_f_min_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFMinEXT);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_f_max_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFMaxEXT);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_assume_true_khr(&mut self, condition: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AssumeTrueKHR);
        condition.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_expect_khr(&mut self, result_type: Type, value: ValueId, expected_value: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExpectKHR);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        expected_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_decorate_string(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorateString);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_string_google(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorateStringGOOGLE);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate_string(
        &mut self,
        struct_type: ValueId,
        member: i32,
        decoration2: spirv_headers::Decoration,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberDecorateString);
        struct_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_decorate_string_google(
        &mut self,
        struct_type: ValueId,
        member: i32,
        decoration2: spirv_headers::Decoration,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberDecorateStringGOOGLE);
        struct_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_vme_image_intel(&mut self, result_type: Type, image_type: ValueId, sampler: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VmeImageINTEL);
        inst_builder.set_result(result_type);
        image_type.write_operand(&mut inst_builder);
        sampler.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_vme_image_intel(&mut self, image_type: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeVmeImageINTEL);
        inst_builder.set_result(result_type);
        image_type.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_payload_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImePayloadINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ref_payload_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcRefPayloadINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_sic_payload_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcSicPayloadINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_mce_payload_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcMcePayloadINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_mce_result_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcMceResultINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_result_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_result_single_reference_streamout_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultSingleReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_result_dual_reference_streamout_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultDualReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_single_reference_streamin_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeSingleReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ime_dual_reference_streamin_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeDualReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_ref_result_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcRefResultINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_type_avc_sic_result_intel(&mut self) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcSicResultINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_base_multi_reference_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_inter_base_multi_reference_penalty_intel(
        &mut self,
        result_type: Type,
        reference_base_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL);
        inst_builder.set_result(result_type);
        reference_base_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_shape_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterShapePenaltyINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_inter_shape_penalty_intel(
        &mut self,
        result_type: Type,
        packed_shape_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterShapePenaltyINTEL);
        inst_builder.set_result(result_type);
        packed_shape_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_direction_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_inter_direction_penalty_intel(
        &mut self,
        result_type: Type,
        direction_cost: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterDirectionPenaltyINTEL);
        inst_builder.set_result(result_type);
        direction_cost.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_luma_shape_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_motion_vector_cost_table_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_high_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_medium_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_low_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_motion_vector_cost_function_intel(
        &mut self,
        result_type: Type,
        packed_cost_center_delta: ValueId,
        packed_cost_table: ValueId,
        cost_precision: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetMotionVectorCostFunctionINTEL);
        inst_builder.set_result(result_type);
        packed_cost_center_delta.write_operand(&mut inst_builder);
        packed_cost_table.write_operand(&mut inst_builder);
        cost_precision.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_luma_mode_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL);
        inst_builder.set_result(result_type);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_non_dc_luma_intra_penalty_intel(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_chroma_mode_base_penalty_intel(
        &mut self,
        result_type: Type,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_ac_only_haar_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetAcOnlyHaarINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_source_interlaced_field_polarity_intel(
        &mut self,
        result_type: Type,
        source_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL);
        inst_builder.set_result(result_type);
        source_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_single_reference_interlaced_field_polarity_intel(
        &mut self,
        result_type: Type,
        reference_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL);
        inst_builder.set_result(result_type);
        reference_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_set_dual_reference_interlaced_field_polarities_intel(
        &mut self,
        result_type: Type,
        forward_reference_field_polarity: ValueId,
        backward_reference_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL);
        inst_builder.set_result(result_type);
        forward_reference_field_polarity.write_operand(&mut inst_builder);
        backward_reference_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_ime_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToImePayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_ime_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToImeResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_ref_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToRefPayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_ref_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToRefResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_sic_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToSicPayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_convert_to_sic_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToSicResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_motion_vectors_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetMotionVectorsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterDistortionsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_best_inter_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetBestInterDistortionsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_major_shape_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMajorShapeINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_minor_shape_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMinorShapeINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_directions_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterDirectionsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_motion_vector_count_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMotionVectorCountINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterReferenceIdsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_mce_get_inter_reference_interlaced_field_polarities_intel(
        &mut self,
        result_type: Type,
        packed_reference_ids: ValueId,
        packed_reference_parameter_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL);
        inst_builder.set_result(result_type);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_parameter_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_initialize_intel(
        &mut self,
        result_type: Type,
        src_coord: ValueId,
        partition_mask: ValueId,
        sad_adjustment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeInitializeINTEL);
        inst_builder.set_result(result_type);
        src_coord.write_operand(&mut inst_builder);
        partition_mask.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_single_reference_intel(
        &mut self,
        result_type: Type,
        ref_offset: ValueId,
        search_window_config: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetSingleReferenceINTEL);
        inst_builder.set_result(result_type);
        ref_offset.write_operand(&mut inst_builder);
        search_window_config.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_dual_reference_intel(
        &mut self,
        result_type: Type,
        fwd_ref_offset: ValueId,
        bwd_ref_offset: ValueId,
        id_search_window_config: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetDualReferenceINTEL);
        inst_builder.set_result(result_type);
        fwd_ref_offset.write_operand(&mut inst_builder);
        bwd_ref_offset.write_operand(&mut inst_builder);
        id_search_window_config.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_ref_window_size_intel(
        &mut self,
        result_type: Type,
        search_window_config: ValueId,
        dual_ref: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeRefWindowSizeINTEL);
        inst_builder.set_result(result_type);
        search_window_config.write_operand(&mut inst_builder);
        dual_ref.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_adjust_ref_offset_intel(
        &mut self,
        result_type: Type,
        ref_offset: ValueId,
        src_coord: ValueId,
        ref_window_size: ValueId,
        image_size: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeAdjustRefOffsetINTEL);
        inst_builder.set_result(result_type);
        ref_offset.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        ref_window_size.write_operand(&mut inst_builder);
        image_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeConvertToMcePayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_max_motion_vector_count_intel(
        &mut self,
        result_type: Type,
        max_motion_vector_count: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetMaxMotionVectorCountINTEL);
        inst_builder.set_result(result_type);
        max_motion_vector_count.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_unidirectional_mix_disable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetUnidirectionalMixDisableINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_early_search_termination_threshold_intel(
        &mut self,
        result_type: Type,
        threshold: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetEarlySearchTerminationThresholdINTEL);
        inst_builder.set_result(result_type);
        threshold.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_set_weighted_sad_intel(
        &mut self,
        result_type: Type,
        packed_sad_weights: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetWeightedSadINTEL);
        inst_builder.set_result(result_type);
        packed_sad_weights.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streamin_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_streamin_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streamout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_streamout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streaminout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_streaminout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeConvertToMceResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_single_reference_streamin_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetSingleReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_dual_reference_streamin_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetDualReferenceStreaminINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_strip_single_reference_streamout_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeStripSingleReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_strip_dual_reference_streamout_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeStripDualReferenceStreamoutINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_motion_vectors_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_motion_vectors_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_border_reached_intel(
        &mut self,
        result_type: Type,
        image_select: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetBorderReachedINTEL);
        inst_builder.set_result(result_type);
        image_select.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_truncated_search_indication_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetTruncatedSearchIndicationINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_unidirectional_early_search_termination_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_weighting_pattern_minimum_motion_vector_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ime_get_weighting_pattern_minimum_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_fme_initialize_intel(
        &mut self,
        result_type: Type,
        src_coord: ValueId,
        motion_vectors: ValueId,
        major_shapes: ValueId,
        minor_shapes: ValueId,
        direction: ValueId,
        pixel_resolution: ValueId,
        sad_adjustment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcFmeInitializeINTEL);
        inst_builder.set_result(result_type);
        src_coord.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        major_shapes.write_operand(&mut inst_builder);
        minor_shapes.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        pixel_resolution.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_bme_initialize_intel(
        &mut self,
        result_type: Type,
        src_coord: ValueId,
        motion_vectors: ValueId,
        major_shapes: ValueId,
        minor_shapes: ValueId,
        direction: ValueId,
        pixel_resolution: ValueId,
        bidirectional_weight: ValueId,
        sad_adjustment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcBmeInitializeINTEL);
        inst_builder.set_result(result_type);
        src_coord.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        major_shapes.write_operand(&mut inst_builder);
        minor_shapes.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        pixel_resolution.write_operand(&mut inst_builder);
        bidirectional_weight.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefConvertToMcePayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_set_bidirectional_mix_disable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefSetBidirectionalMixDisableINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_set_bilinear_filter_enable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefSetBilinearFilterEnableINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithSingleReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithDualReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_multi_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithMultiReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_multi_reference_interlaced_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        packed_reference_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_ref_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefConvertToMceResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_initialize_intel(&mut self, result_type: Type, src_coord: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicInitializeINTEL);
        inst_builder.set_result(result_type);
        src_coord.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_configure_skc_intel(
        &mut self,
        result_type: Type,
        skip_block_partition_type: ValueId,
        skip_motion_vector_mask: ValueId,
        motion_vectors: ValueId,
        bidirectional_weight: ValueId,
        sad_adjustment: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureSkcINTEL);
        inst_builder.set_result(result_type);
        skip_block_partition_type.write_operand(&mut inst_builder);
        skip_motion_vector_mask.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        bidirectional_weight.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_configure_ipe_luma_intel(
        &mut self,
        result_type: Type,
        luma_intra_partition_mask: ValueId,
        intra_neighbour_availabilty: ValueId,
        left_edge_luma_pixels: ValueId,
        upper_left_corner_luma_pixel: ValueId,
        upper_edge_luma_pixels: ValueId,
        upper_right_edge_luma_pixels: ValueId,
        sad_adjustment: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureIpeLumaINTEL);
        inst_builder.set_result(result_type);
        luma_intra_partition_mask.write_operand(&mut inst_builder);
        intra_neighbour_availabilty.write_operand(&mut inst_builder);
        left_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_left_corner_luma_pixel.write_operand(&mut inst_builder);
        upper_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_right_edge_luma_pixels.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_configure_ipe_luma_chroma_intel(
        &mut self,
        result_type: Type,
        luma_intra_partition_mask: ValueId,
        intra_neighbour_availabilty: ValueId,
        left_edge_luma_pixels: ValueId,
        upper_left_corner_luma_pixel: ValueId,
        upper_edge_luma_pixels: ValueId,
        upper_right_edge_luma_pixels: ValueId,
        left_edge_chroma_pixels: ValueId,
        upper_left_corner_chroma_pixel: ValueId,
        upper_edge_chroma_pixels: ValueId,
        sad_adjustment: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureIpeLumaChromaINTEL);
        inst_builder.set_result(result_type);
        luma_intra_partition_mask.write_operand(&mut inst_builder);
        intra_neighbour_availabilty.write_operand(&mut inst_builder);
        left_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_left_corner_luma_pixel.write_operand(&mut inst_builder);
        upper_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_right_edge_luma_pixels.write_operand(&mut inst_builder);
        left_edge_chroma_pixels.write_operand(&mut inst_builder);
        upper_left_corner_chroma_pixel.write_operand(&mut inst_builder);
        upper_edge_chroma_pixels.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_motion_vector_mask_intel(
        &mut self,
        result_type: Type,
        skip_block_partition_type: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetMotionVectorMaskINTEL);
        inst_builder.set_result(result_type);
        skip_block_partition_type.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConvertToMcePayloadINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_intra_luma_shape_penalty_intel(
        &mut self,
        result_type: Type,
        packed_shape_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraLumaShapePenaltyINTEL);
        inst_builder.set_result(result_type);
        packed_shape_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_intra_luma_mode_cost_function_intel(
        &mut self,
        result_type: Type,
        luma_mode_penalty: ValueId,
        luma_packed_neighbor_modes: ValueId,
        luma_packed_non_dc_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraLumaModeCostFunctionINTEL);
        inst_builder.set_result(result_type);
        luma_mode_penalty.write_operand(&mut inst_builder);
        luma_packed_neighbor_modes.write_operand(&mut inst_builder);
        luma_packed_non_dc_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_intra_chroma_mode_cost_function_intel(
        &mut self,
        result_type: Type,
        chroma_mode_base_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraChromaModeCostFunctionINTEL);
        inst_builder.set_result(result_type);
        chroma_mode_base_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_bilinear_filter_enable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetBilinearFilterEnableINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_skc_forward_transform_enable_intel(
        &mut self,
        result_type: Type,
        packed_sad_coefficients: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetSkcForwardTransformEnableINTEL);
        inst_builder.set_result(result_type);
        packed_sad_coefficients.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_set_block_based_raw_skip_sad_intel(
        &mut self,
        result_type: Type,
        block_based_skip_type: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetBlockBasedRawSkipSadINTEL);
        inst_builder.set_result(result_type);
        block_based_skip_type.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_evaluate_ipe_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateIpeINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithSingleReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithDualReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_multi_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithMultiReferenceINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_multi_reference_interlaced_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        packed_reference_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL);
        inst_builder.set_result(result_type);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConvertToMceResultINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_ipe_luma_shape_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetIpeLumaShapeINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_best_ipe_luma_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetBestIpeLumaDistortionINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_best_ipe_chroma_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetBestIpeChromaDistortionINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_packed_ipe_luma_modes_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedIpeLumaModesINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_ipe_chroma_mode_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetIpeChromaModeINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_packed_skc_luma_count_threshold_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_packed_skc_luma_sum_threshold_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_subgroup_avc_sic_get_inter_raw_sads_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetInterRawSadsINTEL);
        inst_builder.set_result(result_type);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_variable_length_array_intel(&mut self, result_type: Type, lenght: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VariableLengthArrayINTEL);
        inst_builder.set_result(result_type);
        lenght.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_save_memory_intel(&mut self, result_type: Type) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SaveMemoryINTEL);
        inst_builder.set_result(result_type);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_restore_memory_intel(&mut self, ptr: ValueId) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RestoreMemoryINTEL);
        ptr.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_arbitrary_float_sin_cos_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        from_sign: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinCosPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        from_sign.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cast_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cast_from_int_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        mout: i32,
        from_sign: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastFromIntINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        from_sign.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cast_to_int_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastToIntINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_add_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatAddINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_sub_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSubINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_mul_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatMulINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_div_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatDivINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_gtintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatGTINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_geintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatGEINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_ltintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLTINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_leintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLEINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_eqintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatEQINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_recip_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatRecipINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_r_sqrt_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatRSqrtINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cbrt_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCbrtINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_hypot_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatHypotINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_sqrt_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSqrtINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_log_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLogINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_log2_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog2INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_log10_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog10INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_log1p_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog1pINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_exp_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExpINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_exp2_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExp2INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_exp10_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExp10INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_expm1_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExpm1INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_sin_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cos_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCosINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_sin_cos_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinCosINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_sin_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_cos_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCosPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_sin_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatASinINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_sin_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatASinPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_cos_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatACosINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_cos_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatACosPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_tan_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATanINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_tan_pi_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATanPiINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_a_tan2_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATan2INTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_pow_intel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_pow_rintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowRINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_arbitrary_float_pow_nintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        mout: i32,
        enable_subnormals: i32,
        rounding_mode: i32,
        rounding_accuracy: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowNINTEL);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_loop_control_intel(&mut self, loop_control_parameters: &[i32]) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LoopControlINTEL);
        loop_control_parameters.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_alias_domain_decl_intel(&mut self, name: Option<ValueId>) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasDomainDeclINTEL);
        inst_builder.set_result(result_type);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_alias_scope_decl_intel(&mut self, alias_domain: ValueId, name: Option<ValueId>) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasScopeDeclINTEL);
        inst_builder.set_result(result_type);
        alias_domain.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_alias_scope_list_decl_intel(&mut self, alias_scope1_alias_scope2: &[ValueId]) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasScopeListDeclINTEL);
        inst_builder.set_result(result_type);
        alias_scope1_alias_scope2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_sqrt_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSqrtINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_recip_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedRecipINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_rsqrt_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedRsqrtINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_sin_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_cos_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedCosINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_sin_cos_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinCosINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_sin_pi_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinPiINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_cos_pi_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedCosPiINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_sin_cos_pi_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinCosPiINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_log_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedLogINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fixed_exp_intel(
        &mut self,
        result_type: Type,
        input_type: ValueId,
        input: ValueId,
        s: i32,
        i: i32,
        r_i: i32,
        q: i32,
        o: i32,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedExpINTEL);
        inst_builder.set_result(result_type);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ptr_cast_to_cross_workgroup_intel(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrCastToCrossWorkgroupINTEL);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_cross_workgroup_cast_to_ptr_intel(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CrossWorkgroupCastToPtrINTEL);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_read_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadPipeBlockingINTEL);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_write_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePipeBlockingINTEL);
        inst_builder.set_result(result_type);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_fpga_reg_intel(&mut self, result_type: Type, result: ValueId, input: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FPGARegINTEL);
        inst_builder.set_result(result_type);
        result.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_ray_t_min_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetRayTMinKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_ray_flags_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetRayFlagsKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_tkhr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionTKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_custom_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceCustomIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_id_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceIdKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_instance_shader_binding_table_record_offset_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_geometry_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionGeometryIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_primitive_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionPrimitiveIndexKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_barycentrics_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionBarycentricsKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_front_face_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionFrontFaceKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_candidate_aabb_opaque_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionCandidateAABBOpaqueKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_ray_direction_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectRayDirectionKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_ray_origin_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectRayOriginKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_world_ray_direction_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetWorldRayDirectionKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_world_ray_origin_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetWorldRayOriginKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_object_to_world_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectToWorldKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_ray_query_get_intersection_world_to_object_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionWorldToObjectKHR);
        inst_builder.set_result(result_type);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_atomic_f_add_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFAddEXT);
        inst_builder.set_result(result_type);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_control_barrier_arrive_intel(
        &mut self,
        execution: ScopeId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ControlBarrierArriveINTEL);
        execution.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_control_barrier_wait_intel(
        &mut self,
        execution: ScopeId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ControlBarrierWaitINTEL);
        execution.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_i_mul_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIMulKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_f_mul_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMulKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_bitwise_and_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseAndKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_bitwise_or_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseOrKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_bitwise_xor_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseXorKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_logical_and_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalAndKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_logical_or_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalOrKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_group_logical_xor_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalXorKHR);
        inst_builder.set_result(result_type);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
}

// --- GLSL.std.450 ---
impl<'a, 'hir> FunctionBuilder<'a, 'hir> {
    pub fn emit_glsl_round(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 1i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_round_even(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 2i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_trunc(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 3i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_abs(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 4i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_abs(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 5i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_sign(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 6i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_sign(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 7i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_floor(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 8i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_ceil(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 9i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_fract(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 10i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_radians(&mut self, result_type: Type, degrees: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 11i64);
        inst_builder.set_result(result_type);
        degrees.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_degrees(&mut self, result_type: Type, radians: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 12i64);
        inst_builder.set_result(result_type);
        radians.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sin(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 13i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cos(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 14i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_tan(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 15i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_asin(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 16i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_acos(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 17i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atan(&mut self, result_type: Type, y_over_x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 18i64);
        inst_builder.set_result(result_type);
        y_over_x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sinh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 19i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cosh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 20i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_tanh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 21i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_asinh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 22i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_acosh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 23i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atanh(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 24i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_atan2(&mut self, result_type: Type, y: ValueId, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 25i64);
        inst_builder.set_result(result_type);
        y.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pow(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 26i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_exp(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 27i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_log(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 28i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_exp2(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 29i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_log2(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 30i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_sqrt(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 31i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_inverse_sqrt(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 32i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_determinant(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 33i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_matrix_inverse(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 34i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_modf(&mut self, result_type: Type, x: ValueId, i: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 35i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_modf_struct(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 36i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_min(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 37i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_min(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 38i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_min(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 39i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_max(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 40i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_max(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 41i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_max(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 42i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_clamp(&mut self, result_type: Type, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 43i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_u_clamp(&mut self, result_type: Type, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 44i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_s_clamp(&mut self, result_type: Type, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 45i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_f_mix(&mut self, result_type: Type, x: ValueId, y: ValueId, a: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 46i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_i_mix(&mut self, result_type: Type, x: ValueId, y: ValueId, a: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 47i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_step(&mut self, result_type: Type, edge: ValueId, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 48i64);
        inst_builder.set_result(result_type);
        edge.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_smooth_step(&mut self, result_type: Type, edge0: ValueId, edge1: ValueId, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 49i64);
        inst_builder.set_result(result_type);
        edge0.write_operand(&mut inst_builder);
        edge1.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_fma(&mut self, result_type: Type, a: ValueId, b: ValueId, c: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 50i64);
        inst_builder.set_result(result_type);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_frexp(&mut self, result_type: Type, x: ValueId, exp: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 51i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_frexp_struct(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 52i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_ldexp(&mut self, result_type: Type, x: ValueId, exp: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 53i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_snorm4x8(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 54i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_unorm4x8(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 55i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_snorm2x16(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 56i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_unorm2x16(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 57i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_half2x16(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 58i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_pack_double2x32(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 59i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_snorm2x16(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 60i64);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_unorm2x16(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 61i64);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_half2x16(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 62i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_snorm4x8(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 63i64);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_unorm4x8(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 64i64);
        inst_builder.set_result(result_type);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_unpack_double2x32(&mut self, result_type: Type, v: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 65i64);
        inst_builder.set_result(result_type);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_length(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 66i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_distance(&mut self, result_type: Type, p0: ValueId, p1: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 67i64);
        inst_builder.set_result(result_type);
        p0.write_operand(&mut inst_builder);
        p1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_cross(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 68i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_normalize(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 69i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_face_forward(&mut self, result_type: Type, n: ValueId, i: ValueId, nref: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 70i64);
        inst_builder.set_result(result_type);
        n.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        nref.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_reflect(&mut self, result_type: Type, i: ValueId, n: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 71i64);
        inst_builder.set_result(result_type);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_refract(&mut self, result_type: Type, i: ValueId, n: ValueId, eta: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 72i64);
        inst_builder.set_result(result_type);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        eta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_i_lsb(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 73i64);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_s_msb(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 74i64);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_find_u_msb(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 75i64);
        inst_builder.set_result(result_type);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_centroid(&mut self, result_type: Type, interpolant: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 76i64);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_sample(
        &mut self,
        result_type: Type,
        interpolant: ValueId,
        sample: ValueId,
    ) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 77i64);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_interpolate_at_offset(
        &mut self,
        result_type: Type,
        interpolant: ValueId,
        offset: ValueId,
    ) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 78i64);
        inst_builder.set_result(result_type);
        interpolant.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_min(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 79i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_max(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 80i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
    pub fn emit_glsl_n_clamp(&mut self, result_type: Type, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 81i64);
        inst_builder.set_result(result_type);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder).unwrap()
    }
}
