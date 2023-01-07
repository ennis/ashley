// --- Core instructions ---
impl Builder {
    pub fn emit_nop(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Nop);
        self.append_inst(inst_builder);
    }
    pub fn emit_undef(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Undef);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_source_continued(&mut self, continued_source: &str) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Source);
        source_language0.write_operand(&mut inst_builder);
        version.write_operand(&mut inst_builder);
        file.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_source_extension(&mut self, extension: &str) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SourceExtension);
        extension.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_name(&mut self, target: ValueId, name: &str) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Name);
        target.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_member_name(&mut self, r#type: ValueId, member: i32, name: &str) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberName);
        r#type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_string(&mut self, string: &str) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::String);
        result.write_operand(&mut inst_builder);
        string.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_line(&mut self, file: ValueId, line: i32, column: i32) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Line);
        file.write_operand(&mut inst_builder);
        line.write_operand(&mut inst_builder);
        column.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_extension(&mut self, name: &str) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Extension);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ext_inst_import(&mut self, name: &str) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExtInstImport);
        result.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ext_inst(
        &mut self,
        result_type: Type,
        set: ValueId,
        instruction: u32,
        operand_1_operand_2: &[ValueId],
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExtInst);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        set.write_operand(&mut inst_builder);
        instruction.write_operand(&mut inst_builder);
        operand_1_operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_memory_model(
        &mut self,
        addressing_model0: spirv_headers::AddressingModel,
        memory_model1: spirv_headers::MemoryModel,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EntryPoint);
        execution_model0.write_operand(&mut inst_builder);
        entry_point.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        interface.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode(&mut self, entry_point: ValueId, mode: spirv_headers::ExecutionMode) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecutionMode);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capability(&mut self, capability: spirv_headers::Capability) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Variable);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        storage_class2.write_operand(&mut inst_builder);
        initializer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_texel_pointer(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        sample: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageTexelPointer);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_load(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory_access3: Option<spirv_headers::MemoryAccess>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Load);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_store(
        &mut self,
        pointer: ValueId,
        object: ValueId,
        memory_access2: Option<spirv_headers::MemoryAccess>,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyMemorySized);
        target.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        memory_access3.write_operand(&mut inst_builder);
        memory_access4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_access_chain(&mut self, result_type: Type, base: ValueId, indexes: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AccessChain);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_in_bounds_access_chain(&mut self, result_type: Type, base: ValueId, indexes: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::InBoundsAccessChain);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: ValueId,
        element: ValueId,
        indexes: &[ValueId],
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrAccessChain);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_array_length(&mut self, result_type: Type, structure: ValueId, array_member: i32) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArrayLength);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        structure.write_operand(&mut inst_builder);
        array_member.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_generic_ptr_mem_semantics(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericPtrMemSemantics);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_in_bounds_ptr_access_chain(
        &mut self,
        result_type: Type,
        base: ValueId,
        element: ValueId,
        indexes: &[ValueId],
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::InBoundsPtrAccessChain);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        element.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_decorate(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberDecorate);
        structure_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decoration_group(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorationGroup);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_decorate(&mut self, decoration_group: ValueId, targets: &[ValueId]) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupDecorate);
        decoration_group.write_operand(&mut inst_builder);
        targets.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_member_decorate(&mut self, decoration_group: ValueId, targets: &[(ValueId, i32)]) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupMemberDecorate);
        decoration_group.write_operand(&mut inst_builder);
        targets.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_vector_extract_dynamic(&mut self, result_type: Type, vector: ValueId, index: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorExtractDynamic);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_vector_insert_dynamic(
        &mut self,
        result_type: Type,
        vector: ValueId,
        component: ValueId,
        index: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorInsertDynamic);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_vector_shuffle(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        components: &[i32],
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorShuffle);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_composite_construct(&mut self, result_type: Type, constituents: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeConstruct);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        constituents.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_composite_extract(&mut self, result_type: Type, composite: ValueId, indexes: &[i32]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeExtract);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_composite_insert(
        &mut self,
        result_type: Type,
        object: ValueId,
        composite: ValueId,
        indexes: &[i32],
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CompositeInsert);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        object.write_operand(&mut inst_builder);
        composite.write_operand(&mut inst_builder);
        indexes.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_copy_object(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyObject);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_transpose(&mut self, result_type: Type, matrix: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Transpose);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_sampled_image(&mut self, result_type: Type, image: ValueId, sampler: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SampledImage);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        sampler.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleDrefImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleDrefExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_proj_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjDrefImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sample_proj_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleProjDrefExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_fetch(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageFetch);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        component: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageGather);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_dref_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageDrefGather);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_read(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageRead);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_write(
        &mut self,
        image: ValueId,
        coordinate: ValueId,
        texel: ValueId,
        image_operands3: Option<ImageOperands>,
    ) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageWrite);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        texel.write_operand(&mut inst_builder);
        image_operands3.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_image(&mut self, result_type: Type, sampled_image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Image);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_format(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryFormat);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_order(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryOrder);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_size_lod(
        &mut self,
        result_type: Type,
        image: ValueId,
        level_of_detail: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySizeLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        level_of_detail.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_size(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySize);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_lod(&mut self, result_type: Type, sampled_image: ValueId, coordinate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_levels(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQueryLevels);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_query_samples(&mut self, result_type: Type, image: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageQuerySamples);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_f_to_u(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertFToU);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_f_to_s(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertFToS);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_s_to_f(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSToF);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_u_to_f(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToF);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_convert(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UConvert);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_convert(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SConvert);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_convert(&mut self, result_type: Type, float_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FConvert);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        float_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_quantize_to_f16(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::QuantizeToF16);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_ptr_to_u(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertPtrToU);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_sat_convert_s_to_u(&mut self, result_type: Type, signed_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SatConvertSToU);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        signed_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_sat_convert_u_to_s(&mut self, result_type: Type, unsigned_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SatConvertUToS);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        unsigned_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_u_to_ptr(&mut self, result_type: Type, integer_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToPtr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        integer_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_cast_to_generic(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrCastToGeneric);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_generic_cast_to_ptr(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericCastToPtr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_generic_cast_to_ptr_explicit(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        storage: spirv_headers::StorageClass,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GenericCastToPtrExplicit);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bitcast(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Bitcast);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_negate(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SNegate);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_negate(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FNegate);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_add(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_add(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_sub(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISub);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_sub(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FSub);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_mul(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IMul);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_mul(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FMul);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDiv);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDiv);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_div(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FDiv);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_rem(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SRem);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SMod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_rem(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FRem);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_mod(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FMod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_vector_times_scalar(&mut self, result_type: Type, vector: ValueId, scalar: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorTimesScalar);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_matrix_times_scalar(&mut self, result_type: Type, matrix: ValueId, scalar: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesScalar);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        scalar.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_vector_times_matrix(&mut self, result_type: Type, vector: ValueId, matrix: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VectorTimesMatrix);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_matrix_times_vector(&mut self, result_type: Type, matrix: ValueId, vector: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesVector);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        matrix.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_matrix_times_matrix(
        &mut self,
        result_type: Type,
        left_matrix: ValueId,
        right_matrix: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MatrixTimesMatrix);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        left_matrix.write_operand(&mut inst_builder);
        right_matrix.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_outer_product(&mut self, result_type: Type, vector_1: ValueId, vector_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::OuterProduct);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dot(&mut self, result_type: Type, vector_1: ValueId, vector_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Dot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_add_carry(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAddCarry);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_sub_borrow(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISubBorrow);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_mul_extended(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMulExtended);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_mul_extended(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SMulExtended);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_any(&mut self, result_type: Type, vector: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Any);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_all(&mut self, result_type: Type, vector: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::All);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_is_nan(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsNan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_is_inf(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsInf);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_is_finite(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsFinite);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_is_normal(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsNormal);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_sign_bit_set(&mut self, result_type: Type, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SignBitSet);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_less_or_greater(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LessOrGreater);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ordered(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Ordered);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_unordered(&mut self, result_type: Type, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Unordered);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_logical_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_logical_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalNotEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_logical_or(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalOr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_logical_and(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalAnd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_logical_not(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LogicalNot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_select(
        &mut self,
        result_type: Type,
        condition: ValueId,
        object_1: ValueId,
        object_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Select);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        condition.write_operand(&mut inst_builder);
        object_1.write_operand(&mut inst_builder);
        object_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::INotEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UGreaterThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SGreaterThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_greater_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UGreaterThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_greater_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SGreaterThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ULessThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SLessThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ULessThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SLessThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdNotEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordNotEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdLessThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_less_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordLessThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdGreaterThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_greater_than(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordGreaterThan);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_less_than_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdLessThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_less_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordLessThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_ord_greater_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FOrdGreaterThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_f_unord_greater_than_equal(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FUnordGreaterThanEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_shift_right_logical(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftRightLogical);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_shift_right_arithmetic(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftRightArithmetic);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_shift_left_logical(&mut self, result_type: Type, base: ValueId, shift: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ShiftLeftLogical);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        shift.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bitwise_or(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseOr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bitwise_xor(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseXor);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bitwise_and(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitwiseAnd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_not(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Not);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bit_field_insert(
        &mut self,
        result_type: Type,
        base: ValueId,
        insert: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldInsert);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        insert.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bit_field_s_extract(
        &mut self,
        result_type: Type,
        base: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldSExtract);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bit_field_u_extract(
        &mut self,
        result_type: Type,
        base: ValueId,
        offset: ValueId,
        count: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitFieldUExtract);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bit_reverse(&mut self, result_type: Type, base: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitReverse);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_bit_count(&mut self, result_type: Type, base: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BitCount);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        base.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdx(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdx);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdy(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdy);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fwidth(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Fwidth);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdx_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdxFine);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdy_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdyFine);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fwidth_fine(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FwidthFine);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdx_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdxCoarse);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_dpdy_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DPdyCoarse);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fwidth_coarse(&mut self, result_type: Type, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FwidthCoarse);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_emit_vertex(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitVertex);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_primitive(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndPrimitive);
        self.append_inst(inst_builder);
    }
    pub fn emit_emit_stream_vertex(&mut self, stream: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitStreamVertex);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_stream_primitive(&mut self, stream: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndStreamPrimitive);
        stream.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_control_barrier(&mut self, execution: ScopeId, memory: ScopeId, semantics: MemorySemanticsId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ControlBarrier);
        execution.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_memory_barrier(&mut self, memory: ScopeId, semantics: MemorySemanticsId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicLoad);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_store(
        &mut self,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicExchange);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicCompareExchange);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        equal.write_operand(&mut inst_builder);
        unequal.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        comparator.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicCompareExchangeWeak);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        equal.write_operand(&mut inst_builder);
        unequal.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        comparator.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_i_increment(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIIncrement);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_i_decrement(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIDecrement);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_i_add(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicIAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_i_sub(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicISub);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_s_min(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicSMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_u_min(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicUMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_s_max(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicSMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_u_max(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicUMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_and(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicAnd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_or(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicOr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_xor(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicXor);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_phi(&mut self, result_type: Type, variable_parent: &[(ValueId, ValueId)]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Phi);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        variable_parent.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_loop_merge(
        &mut self,
        merge_block: ValueId,
        continue_target: ValueId,
        loop_control2: spirv_headers::LoopControl,
    ) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LoopMerge);
        merge_block.write_operand(&mut inst_builder);
        continue_target.write_operand(&mut inst_builder);
        loop_control2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_selection_merge(&mut self, merge_block: ValueId, selection_control1: spirv_headers::SelectionControl) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SelectionMerge);
        merge_block.write_operand(&mut inst_builder);
        selection_control1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_label(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Label);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_branch(&mut self, target_label: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BranchConditional);
        condition.write_operand(&mut inst_builder);
        true_label.write_operand(&mut inst_builder);
        false_label.write_operand(&mut inst_builder);
        branch_weights.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_switch(&mut self, selector: ValueId, default: ValueId, target: &[(i32, ValueId)]) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Switch);
        selector.write_operand(&mut inst_builder);
        default.write_operand(&mut inst_builder);
        target.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_kill(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Kill);
        self.append_inst(inst_builder);
    }
    pub fn emit_return(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Return);
        self.append_inst(inst_builder);
    }
    pub fn emit_return_value(&mut self, value: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReturnValue);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_unreachable(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::Unreachable);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_start(&mut self, pointer: ValueId, size: i32) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LifetimeStart);
        pointer.write_operand(&mut inst_builder);
        size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_lifetime_stop(&mut self, pointer: ValueId, size: i32) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAsyncCopy);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        destination.write_operand(&mut inst_builder);
        source.write_operand(&mut inst_builder);
        num_elements.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_wait_events(&mut self, execution: ScopeId, num_events: ValueId, events_list: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupWaitEvents);
        execution.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        events_list.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_all(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAll);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_any(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupAny);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_broadcast(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        local_id: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBroadcast);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        local_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_i_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_u_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_s_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_u_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_s_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_read_pipe(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadPipe);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_write_pipe(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        pointer: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePipe);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReservedReadPipe);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReservedWritePipe);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_reserve_read_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReserveReadPipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_reserve_write_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        num_packets: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReserveWritePipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_commit_read_pipe(
        &mut self,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CommitWritePipe);
        pipe.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_valid_reserve_id(&mut self, result_type: Type, reserve_id: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsValidReserveId);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        reserve_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_get_num_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetNumPipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_get_max_pipe_packets(
        &mut self,
        result_type: Type,
        pipe: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetMaxPipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupReserveReadPipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupReserveWritePipePackets);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        pipe.write_operand(&mut inst_builder);
        num_packets.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_commit_read_pipe(
        &mut self,
        execution: ScopeId,
        pipe: ValueId,
        reserve_id: ValueId,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EnqueueMarker);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        queue.write_operand(&mut inst_builder);
        num_events.write_operand(&mut inst_builder);
        wait_events.write_operand(&mut inst_builder);
        ret_event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EnqueueKernel);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
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
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelNDrangeSubGroupCount);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        nd_range.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelNDrangeMaxSubGroupSize);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        nd_range.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_get_kernel_work_group_size(
        &mut self,
        result_type: Type,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelWorkGroupSize);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_get_kernel_preferred_work_group_size_multiple(
        &mut self,
        result_type: Type,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelPreferredWorkGroupSizeMultiple);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_retain_event(&mut self, event: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RetainEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_release_event(&mut self, event: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReleaseEvent);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_create_user_event(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CreateUserEvent);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_is_valid_event(&mut self, result_type: Type, event: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsValidEvent);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        event.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_set_user_event_status(&mut self, event: ValueId, status: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SetUserEventStatus);
        event.write_operand(&mut inst_builder);
        status.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_capture_event_profiling_info(&mut self, event: ValueId, profiling_info: ValueId, value: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CaptureEventProfilingInfo);
        event.write_operand(&mut inst_builder);
        profiling_info.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_get_default_queue(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetDefaultQueue);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_build_nd_range(
        &mut self,
        result_type: Type,
        global_work_size: ValueId,
        local_work_size: ValueId,
        global_work_offset: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BuildNDRange);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        global_work_size.write_operand(&mut inst_builder);
        local_work_size.write_operand(&mut inst_builder);
        global_work_offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleDrefImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleDrefExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_proj_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_proj_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        image_operands4: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_proj_dref_implicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjDrefImplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_sample_proj_dref_explicit_lod(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: ImageOperands,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseSampleProjDrefExplicitLod);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_fetch(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        image_operands4: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseFetch);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        component: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseGather);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        component.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_dref_gather(
        &mut self,
        result_type: Type,
        sampled_image: ValueId,
        coordinate: ValueId,
        d_ref: ValueId,
        image_operands5: Option<ImageOperands>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseDrefGather);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        d_ref.write_operand(&mut inst_builder);
        image_operands5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_image_sparse_texels_resident(&mut self, result_type: Type, resident_code: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseTexelsResident);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        resident_code.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_no_line(&mut self) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFlagTestAndSet);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_flag_clear(&mut self, pointer: ValueId, memory: ScopeId, semantics: MemorySemanticsId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSparseRead);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        image_operands4.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_size_of(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SizeOf);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_constant_pipe_storage(
        &mut self,
        result_type: Type,
        packet_size: i32,
        packet_alignment: i32,
        capacity: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConstantPipeStorage);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        capacity.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_create_pipe_from_pipe_storage(&mut self, result_type: Type, pipe_storage: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CreatePipeFromPipeStorage);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pipe_storage.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelLocalSizeForSubgroupCount);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        subgroup_count.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_get_kernel_max_num_subgroups(
        &mut self,
        result_type: Type,
        invoke: ValueId,
        param: ValueId,
        param_size: ValueId,
        param_align: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GetKernelMaxNumSubgroups);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        invoke.write_operand(&mut inst_builder);
        param.write_operand(&mut inst_builder);
        param_size.write_operand(&mut inst_builder);
        param_align.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_named_barrier_initialize(&mut self, result_type: Type, subgroup_count: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::NamedBarrierInitialize);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        subgroup_count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_memory_named_barrier(&mut self, named_barrier: ValueId, memory: ScopeId, semantics: MemorySemanticsId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemoryNamedBarrier);
        named_barrier.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_module_processed(&mut self, process: &str) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ModuleProcessed);
        process.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_execution_mode_id(&mut self, entry_point: ValueId, mode: spirv_headers::ExecutionMode) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecutionModeId);
        entry_point.write_operand(&mut inst_builder);
        mode.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_id(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorateId);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_non_uniform_elect(&mut self, result_type: Type, execution: ScopeId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformElect);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_all(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAll);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_any(&mut self, result_type: Type, execution: ScopeId, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAny);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_all_equal(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformAllEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_broadcast(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        id: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBroadcast);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_broadcast_first(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBroadcastFirst);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_ballot(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        predicate: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_inverse_ballot(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformInverseBallot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_ballot_bit_extract(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        index: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotBitExtract);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_ballot_bit_count(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotBitCount);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_ballot_find_lsb(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotFindLSB);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_ballot_find_msb(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBallotFindMSB);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_shuffle(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        id: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffle);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_shuffle_xor(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        mask: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleXor);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        mask.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_shuffle_up(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleUp);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_shuffle_down(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformShuffleDown);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_i_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformIAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_f_add(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFAdd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_i_mul(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformIMul);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_f_mul(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMul);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_s_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformSMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_u_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformUMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_f_min(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMin);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_s_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformSMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_u_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformUMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_f_max(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformFMax);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_bitwise_and(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseAnd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_bitwise_or(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseOr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_bitwise_xor(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformBitwiseXor);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_logical_and(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalAnd);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_logical_or(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalOr);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_logical_xor(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        value: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformLogicalXor);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_quad_broadcast(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        index: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformQuadBroadcast);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_quad_swap(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformQuadSwap);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_copy_logical(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CopyLogical);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_not_equal(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrNotEqual);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_diff(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrDiff);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_terminate_invocation(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TerminateInvocation);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_ballot_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupBallotKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_first_invocation_khr(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupFirstInvocationKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_all_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAllKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_any_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAnyKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_all_equal_khr(&mut self, result_type: Type, predicate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAllEqualKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        predicate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_non_uniform_rotate_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        value: ValueId,
        delta: ValueId,
        cluster_size: Option<ValueId>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformRotateKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        cluster_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_read_invocation_khr(&mut self, result_type: Type, value: ValueId, index: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupReadInvocationKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExecuteCallableKHR);
        sbt_index.write_operand(&mut inst_builder);
        callable_data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_convert_u_to_acceleration_structure_khr(&mut self, result_type: Type, accel: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToAccelerationStructureKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        accel.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ignore_intersection_khr(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IgnoreIntersectionKHR);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_khr(&mut self) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_dot(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_su_dot(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDot);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_su_dot_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotAccSat);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_s_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SDotAccSatKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotAccSat);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UDotAccSatKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_su_dot_acc_sat(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotAccSat);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_su_dot_acc_sat_khr(
        &mut self,
        result_type: Type,
        vector_1: ValueId,
        vector_2: ValueId,
        accumulator: ValueId,
        packed_vector_format: Option<spirv_headers::PackedVectorFormat>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SUDotAccSatKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        vector_1.write_operand(&mut inst_builder);
        vector_2.write_operand(&mut inst_builder);
        accumulator.write_operand(&mut inst_builder);
        packed_vector_format.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_ray_query_khr(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeRayQueryKHR);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryTerminateKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_generate_intersection_khr(&mut self, ray_query: ValueId, hit_t: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGenerateIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        hit_t.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_confirm_intersection_khr(&mut self, ray_query: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryConfirmIntersectionKHR);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_ray_query_proceed_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryProceedKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_type_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionTypeKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_i_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIAddNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_add_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFAddNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMinNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_u_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMinNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_s_min_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMinNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMaxNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_u_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupUMaxNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_s_max_non_uniform_amd(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupSMaxNonUniformAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fragment_mask_fetch_amd(&mut self, result_type: Type, image: ValueId, coordinate: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FragmentMaskFetchAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fragment_fetch_amd(
        &mut self,
        result_type: Type,
        image: ValueId,
        coordinate: ValueId,
        fragment_index: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FragmentFetchAMD);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        fragment_index.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_read_clock_khr(&mut self, result_type: Type, scope: ScopeId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadClockKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        scope.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldToObjectNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_object_to_world_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectToWorldNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_object_ray_direction_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectRayDirectionNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_object_ray_origin_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetObjectRayOriginNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetShaderRecordBufferHandleNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_shader_binding_table_record_index_nv(
        &mut self,
        result_type: Type,
        hit_object: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetShaderBindingTableRecordIndexNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_record_empty_nv(&mut self, hit_object: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectExecuteShaderNV);
        hit_object.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_current_time_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetCurrentTimeNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_attributes_nv(&mut self, hit_object: ValueId, hit_object_attribute: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetAttributesNV);
        hit_object.write_operand(&mut inst_builder);
        hit_object_attribute.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_hit_object_get_hit_kind_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetHitKindNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_primitive_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetPrimitiveIndexNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_geometry_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetGeometryIndexNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_instance_id_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetInstanceIdNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_instance_custom_index_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetInstanceCustomIndexNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_world_ray_direction_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldRayDirectionNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_world_ray_origin_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetWorldRayOriginNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_ray_t_max_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetRayTMaxNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_get_ray_t_min_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectGetRayTMinNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_is_empty_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsEmptyNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_is_hit_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsHitNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_hit_object_is_miss_nv(&mut self, result_type: Type, hit_object: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::HitObjectIsMissNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit_object.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_reorder_thread_with_hit_object_nv(
        &mut self,
        hit_object: ValueId,
        hint: Option<ValueId>,
        bits: Option<ValueId>,
    ) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReorderThreadWithHitObjectNV);
        hit_object.write_operand(&mut inst_builder);
        hint.write_operand(&mut inst_builder);
        bits.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_reorder_thread_with_hint_nv(&mut self, hint: ValueId, bits: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReorderThreadWithHintNV);
        hint.write_operand(&mut inst_builder);
        bits.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_type_hit_object_nv(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeHitObjectNV);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ImageSampleFootprintNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        sampled_image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        granularity.write_operand(&mut inst_builder);
        coarse.write_operand(&mut inst_builder);
        image_operands6.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_emit_mesh_tasks_ext(
        &mut self,
        group_count_x: ValueId,
        group_count_y: ValueId,
        group_count_z: ValueId,
        payload: Option<ValueId>,
    ) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EmitMeshTasksEXT);
        group_count_x.write_operand(&mut inst_builder);
        group_count_y.write_operand(&mut inst_builder);
        group_count_z.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_set_mesh_outputs_ext(&mut self, vertex_count: ValueId, primitive_count: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SetMeshOutputsEXT);
        vertex_count.write_operand(&mut inst_builder);
        primitive_count.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_group_non_uniform_partition_nv(&mut self, result_type: Type, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupNonUniformPartitionNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_write_packed_primitive_indices4x8_nv(&mut self, index_offset: ValueId, packed_indices: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePackedPrimitiveIndices4x8NV);
        index_offset.write_operand(&mut inst_builder);
        packed_indices.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_report_intersection_nv(&mut self, result_type: Type, hit: ValueId, hit_kind: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReportIntersectionNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_report_intersection_khr(&mut self, result_type: Type, hit: ValueId, hit_kind: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReportIntersectionKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        hit.write_operand(&mut inst_builder);
        hit_kind.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ignore_intersection_nv(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IgnoreIntersectionNV);
        self.append_inst(inst_builder);
    }
    pub fn emit_terminate_ray_nv(&mut self) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAccelerationStructureNV);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_acceleration_structure_khr(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAccelerationStructureKHR);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_execute_callable_nv(&mut self, sbt_index: ValueId, callable_data_id: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeCooperativeMatrixNV);
        result.write_operand(&mut inst_builder);
        component_type.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        rows.write_operand(&mut inst_builder);
        columns.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_cooperative_matrix_load_nv(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        stride: ValueId,
        column_major: ValueId,
        memory_access5: Option<spirv_headers::MemoryAccess>,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixLoadNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        stride.write_operand(&mut inst_builder);
        column_major.write_operand(&mut inst_builder);
        memory_access5.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_cooperative_matrix_store_nv(
        &mut self,
        pointer: ValueId,
        object: ValueId,
        stride: ValueId,
        column_major: ValueId,
        memory_access4: Option<spirv_headers::MemoryAccess>,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixMulAddNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_cooperative_matrix_length_nv(&mut self, result_type: Type, r#type: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CooperativeMatrixLengthNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        r#type.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_begin_invocation_interlock_ext(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::BeginInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_end_invocation_interlock_ext(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::EndInvocationInterlockEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_demote_to_helper_invocation(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DemoteToHelperInvocation);
        self.append_inst(inst_builder);
    }
    pub fn emit_demote_to_helper_invocation_ext(&mut self) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DemoteToHelperInvocationEXT);
        self.append_inst(inst_builder);
    }
    pub fn emit_is_helper_invocation_ext(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IsHelperInvocationEXT);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_u_to_image_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToImageNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_u_to_sampler_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToSamplerNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_image_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertImageToUNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_sampler_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSamplerToUNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_u_to_sampled_image_nv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertUToSampledImageNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_convert_sampled_image_to_unv(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConvertSampledImageToUNV);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_sampler_image_addressing_mode_nv(&mut self, bit_width: i32) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SamplerImageAddressingModeNV);
        bit_width.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_subgroup_shuffle_intel(&mut self, result_type: Type, data: ValueId, invocation_id: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        invocation_id.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_shuffle_down_intel(
        &mut self,
        result_type: Type,
        current: ValueId,
        next: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleDownINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        current.write_operand(&mut inst_builder);
        next.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_shuffle_up_intel(
        &mut self,
        result_type: Type,
        previous: ValueId,
        current: ValueId,
        delta: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleUpINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        previous.write_operand(&mut inst_builder);
        current.write_operand(&mut inst_builder);
        delta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_shuffle_xor_intel(&mut self, result_type: Type, data: ValueId, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupShuffleXorINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_block_read_intel(&mut self, result_type: Type, ptr: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupBlockReadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ptr.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_block_write_intel(&mut self, ptr: ValueId, data: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageBlockReadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_image_block_write_intel(&mut self, image: ValueId, coordinate: ValueId, data: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageMediaBlockReadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_image_media_block_write_intel(
        &mut self,
        image: ValueId,
        coordinate: ValueId,
        width: ValueId,
        height: ValueId,
        data: ValueId,
    ) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupImageMediaBlockWriteINTEL);
        image.write_operand(&mut inst_builder);
        coordinate.write_operand(&mut inst_builder);
        width.write_operand(&mut inst_builder);
        height.write_operand(&mut inst_builder);
        data.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_u_count_leading_zeros_intel(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UCountLeadingZerosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_count_trailing_zeros_intel(&mut self, result_type: Type, operand: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UCountTrailingZerosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_abs_i_sub_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AbsISubINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_abs_u_sub_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AbsUSubINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_add_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAddSatINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_add_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAddSatINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_average_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAverageINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_average_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAverageINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_average_rounded_intel(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IAverageRoundedINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_average_rounded_intel(
        &mut self,
        result_type: Type,
        operand_1: ValueId,
        operand_2: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UAverageRoundedINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_sub_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ISubSatINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_sub_sat_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::USubSatINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_i_mul32x16_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::IMul32x16INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_u_mul32x16_intel(&mut self, result_type: Type, operand_1: ValueId, operand_2: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::UMul32x16INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        operand_2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_constant_function_pointer_intel(&mut self, result_type: Type, function: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ConstantFunctionPointerINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        function.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_function_pointer_call_intel(&mut self, result_type: Type, operand_1: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FunctionPointerCallINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        operand_1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_asm_target_intel(&mut self, result_type: Type, asm_target: &str) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmTargetINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        asm_target.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_asm_intel(
        &mut self,
        result_type: Type,
        asm_type: ValueId,
        target: ValueId,
        asm_instructions: &str,
        constraints: &str,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        asm_type.write_operand(&mut inst_builder);
        target.write_operand(&mut inst_builder);
        asm_instructions.write_operand(&mut inst_builder);
        constraints.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_asm_call_intel(&mut self, result_type: Type, asm: ValueId, argument_0: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AsmCallINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        asm.write_operand(&mut inst_builder);
        argument_0.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_f_min_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFMinEXT);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_f_max_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFMaxEXT);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_assume_true_khr(&mut self, condition: ValueId) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AssumeTrueKHR);
        condition.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_expect_khr(&mut self, result_type: Type, value: ValueId, expected_value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ExpectKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        expected_value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_decorate_string(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::DecorateString);
        target.write_operand(&mut inst_builder);
        decoration1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_decorate_string_google(&mut self, target: ValueId, decoration1: spirv_headers::Decoration) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::MemberDecorateStringGOOGLE);
        struct_type.write_operand(&mut inst_builder);
        member.write_operand(&mut inst_builder);
        decoration2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_vme_image_intel(&mut self, result_type: Type, image_type: ValueId, sampler: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VmeImageINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image_type.write_operand(&mut inst_builder);
        sampler.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_vme_image_intel(&mut self, image_type: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeVmeImageINTEL);
        result.write_operand(&mut inst_builder);
        image_type.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_payload_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImePayloadINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ref_payload_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcRefPayloadINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_sic_payload_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcSicPayloadINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_mce_payload_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcMcePayloadINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_mce_result_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcMceResultINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_result_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_result_single_reference_streamout_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultSingleReferenceStreamoutINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_result_dual_reference_streamout_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeResultDualReferenceStreamoutINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_single_reference_streamin_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeSingleReferenceStreaminINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ime_dual_reference_streamin_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcImeDualReferenceStreaminINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_ref_result_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcRefResultINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_type_avc_sic_result_intel(&mut self) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::TypeAvcSicResultINTEL);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_base_multi_reference_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterBaseMultiReferencePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_inter_base_multi_reference_penalty_intel(
        &mut self,
        result_type: Type,
        reference_base_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterBaseMultiReferencePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        reference_base_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_shape_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterShapePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_inter_shape_penalty_intel(
        &mut self,
        result_type: Type,
        packed_shape_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterShapePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_shape_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_direction_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterDirectionPenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_inter_direction_penalty_intel(
        &mut self,
        result_type: Type,
        direction_cost: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetInterDirectionPenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        direction_cost.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_luma_shape_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraLumaShapePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_inter_motion_vector_cost_table_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultInterMotionVectorCostTableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_high_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultHighPenaltyCostTableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_medium_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultMediumPenaltyCostTableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_low_penalty_cost_table_intel(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultLowPenaltyCostTableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_motion_vector_cost_function_intel(
        &mut self,
        result_type: Type,
        packed_cost_center_delta: ValueId,
        packed_cost_table: ValueId,
        cost_precision: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetMotionVectorCostFunctionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_cost_center_delta.write_operand(&mut inst_builder);
        packed_cost_table.write_operand(&mut inst_builder);
        cost_precision.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_luma_mode_penalty_intel(
        &mut self,
        result_type: Type,
        slice_type: ValueId,
        qp: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraLumaModePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        slice_type.write_operand(&mut inst_builder);
        qp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_non_dc_luma_intra_penalty_intel(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultNonDcLumaIntraPenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_default_intra_chroma_mode_base_penalty_intel(
        &mut self,
        result_type: Type,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetDefaultIntraChromaModeBasePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_ac_only_haar_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetAcOnlyHaarINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_source_interlaced_field_polarity_intel(
        &mut self,
        result_type: Type,
        source_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetSourceInterlacedFieldPolarityINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        source_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_single_reference_interlaced_field_polarity_intel(
        &mut self,
        result_type: Type,
        reference_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetSingleReferenceInterlacedFieldPolarityINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        reference_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_set_dual_reference_interlaced_field_polarities_intel(
        &mut self,
        result_type: Type,
        forward_reference_field_polarity: ValueId,
        backward_reference_field_polarity: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceSetDualReferenceInterlacedFieldPolaritiesINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        forward_reference_field_polarity.write_operand(&mut inst_builder);
        backward_reference_field_polarity.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_ime_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToImePayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_ime_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToImeResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_ref_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToRefPayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_ref_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToRefResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_sic_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToSicPayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_convert_to_sic_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceConvertToSicResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_motion_vectors_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetMotionVectorsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterDistortionsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_best_inter_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetBestInterDistortionsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_major_shape_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMajorShapeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_minor_shape_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMinorShapeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_directions_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterDirectionsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_motion_vector_count_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterMotionVectorCountINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterReferenceIdsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_mce_get_inter_reference_interlaced_field_polarities_intel(
        &mut self,
        result_type: Type,
        packed_reference_ids: ValueId,
        packed_reference_parameter_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcMceGetInterReferenceInterlacedFieldPolaritiesINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_parameter_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_initialize_intel(
        &mut self,
        result_type: Type,
        src_coord: ValueId,
        partition_mask: ValueId,
        sad_adjustment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeInitializeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        partition_mask.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_single_reference_intel(
        &mut self,
        result_type: Type,
        ref_offset: ValueId,
        search_window_config: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetSingleReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ref_offset.write_operand(&mut inst_builder);
        search_window_config.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_dual_reference_intel(
        &mut self,
        result_type: Type,
        fwd_ref_offset: ValueId,
        bwd_ref_offset: ValueId,
        id_search_window_config: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetDualReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        fwd_ref_offset.write_operand(&mut inst_builder);
        bwd_ref_offset.write_operand(&mut inst_builder);
        id_search_window_config.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_ref_window_size_intel(
        &mut self,
        result_type: Type,
        search_window_config: ValueId,
        dual_ref: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeRefWindowSizeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        search_window_config.write_operand(&mut inst_builder);
        dual_ref.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_adjust_ref_offset_intel(
        &mut self,
        result_type: Type,
        ref_offset: ValueId,
        src_coord: ValueId,
        ref_window_size: ValueId,
        image_size: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeAdjustRefOffsetINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ref_offset.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        ref_window_size.write_operand(&mut inst_builder);
        image_size.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeConvertToMcePayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_max_motion_vector_count_intel(
        &mut self,
        result_type: Type,
        max_motion_vector_count: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetMaxMotionVectorCountINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        max_motion_vector_count.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_unidirectional_mix_disable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetUnidirectionalMixDisableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_early_search_termination_threshold_intel(
        &mut self,
        result_type: Type,
        threshold: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetEarlySearchTerminationThresholdINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        threshold.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_set_weighted_sad_intel(
        &mut self,
        result_type: Type,
        packed_sad_weights: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeSetWeightedSadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_sad_weights.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streamin_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streamout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreamoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_dual_reference_streamout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreamoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_evaluate_with_single_reference_streaminout_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
        streamin_components: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithSingleReferenceStreaminoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeEvaluateWithDualReferenceStreaminoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        streamin_components.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeConvertToMceResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_single_reference_streamin_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetSingleReferenceStreaminINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_dual_reference_streamin_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetDualReferenceStreaminINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_strip_single_reference_streamout_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeStripSingleReferenceStreamoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_strip_dual_reference_streamout_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeStripDualReferenceStreamoutINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_motion_vectors_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeMotionVectorsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeDistortionsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_single_reference_major_shape_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutSingleReferenceMajorShapeReferenceIdsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_motion_vectors_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeMotionVectorsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_distortions_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeDistortionsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_streamout_dual_reference_major_shape_reference_ids_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
        major_shape: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetStreamoutDualReferenceMajorShapeReferenceIdsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        major_shape.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_border_reached_intel(
        &mut self,
        result_type: Type,
        image_select: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetBorderReachedINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        image_select.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_truncated_search_indication_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetTruncatedSearchIndicationINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_unidirectional_early_search_termination_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetUnidirectionalEarlySearchTerminationINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_weighting_pattern_minimum_motion_vector_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetWeightingPatternMinimumMotionVectorINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ime_get_weighting_pattern_minimum_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcImeGetWeightingPatternMinimumDistortionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcFmeInitializeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        major_shapes.write_operand(&mut inst_builder);
        minor_shapes.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        pixel_resolution.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcBmeInitializeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        major_shapes.write_operand(&mut inst_builder);
        minor_shapes.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        pixel_resolution.write_operand(&mut inst_builder);
        bidirectional_weight.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefConvertToMcePayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_set_bidirectional_mix_disable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefSetBidirectionalMixDisableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_set_bilinear_filter_enable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefSetBilinearFilterEnableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithSingleReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithDualReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_multi_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithMultiReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_evaluate_with_multi_reference_interlaced_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        packed_reference_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcRefEvaluateWithMultiReferenceInterlacedINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_ref_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcRefConvertToMceResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_initialize_intel(&mut self, result_type: Type, src_coord: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicInitializeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_coord.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureSkcINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        skip_block_partition_type.write_operand(&mut inst_builder);
        skip_motion_vector_mask.write_operand(&mut inst_builder);
        motion_vectors.write_operand(&mut inst_builder);
        bidirectional_weight.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureIpeLumaINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        luma_intra_partition_mask.write_operand(&mut inst_builder);
        intra_neighbour_availabilty.write_operand(&mut inst_builder);
        left_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_left_corner_luma_pixel.write_operand(&mut inst_builder);
        upper_edge_luma_pixels.write_operand(&mut inst_builder);
        upper_right_edge_luma_pixels.write_operand(&mut inst_builder);
        sad_adjustment.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConfigureIpeLumaChromaINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
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
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_motion_vector_mask_intel(
        &mut self,
        result_type: Type,
        skip_block_partition_type: ValueId,
        direction: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetMotionVectorMaskINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        skip_block_partition_type.write_operand(&mut inst_builder);
        direction.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_convert_to_mce_payload_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConvertToMcePayloadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_intra_luma_shape_penalty_intel(
        &mut self,
        result_type: Type,
        packed_shape_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraLumaShapePenaltyINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_shape_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_intra_luma_mode_cost_function_intel(
        &mut self,
        result_type: Type,
        luma_mode_penalty: ValueId,
        luma_packed_neighbor_modes: ValueId,
        luma_packed_non_dc_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraLumaModeCostFunctionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        luma_mode_penalty.write_operand(&mut inst_builder);
        luma_packed_neighbor_modes.write_operand(&mut inst_builder);
        luma_packed_non_dc_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_intra_chroma_mode_cost_function_intel(
        &mut self,
        result_type: Type,
        chroma_mode_base_penalty: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetIntraChromaModeCostFunctionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        chroma_mode_base_penalty.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_bilinear_filter_enable_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetBilinearFilterEnableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_skc_forward_transform_enable_intel(
        &mut self,
        result_type: Type,
        packed_sad_coefficients: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetSkcForwardTransformEnableINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packed_sad_coefficients.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_set_block_based_raw_skip_sad_intel(
        &mut self,
        result_type: Type,
        block_based_skip_type: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicSetBlockBasedRawSkipSadINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        block_based_skip_type.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_evaluate_ipe_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateIpeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_single_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithSingleReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_dual_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        fwd_ref_image: ValueId,
        bwd_ref_image: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithDualReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        fwd_ref_image.write_operand(&mut inst_builder);
        bwd_ref_image.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_multi_reference_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithMultiReferenceINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_evaluate_with_multi_reference_interlaced_intel(
        &mut self,
        result_type: Type,
        src_image: ValueId,
        packed_reference_ids: ValueId,
        packed_reference_field_polarities: ValueId,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::SubgroupAvcSicEvaluateWithMultiReferenceInterlacedINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        src_image.write_operand(&mut inst_builder);
        packed_reference_ids.write_operand(&mut inst_builder);
        packed_reference_field_polarities.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_convert_to_mce_result_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicConvertToMceResultINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_ipe_luma_shape_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetIpeLumaShapeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_best_ipe_luma_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetBestIpeLumaDistortionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_best_ipe_chroma_distortion_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetBestIpeChromaDistortionINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_packed_ipe_luma_modes_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedIpeLumaModesINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_ipe_chroma_mode_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetIpeChromaModeINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_packed_skc_luma_count_threshold_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedSkcLumaCountThresholdINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_packed_skc_luma_sum_threshold_intel(
        &mut self,
        result_type: Type,
        payload: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetPackedSkcLumaSumThresholdINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_subgroup_avc_sic_get_inter_raw_sads_intel(&mut self, result_type: Type, payload: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SubgroupAvcSicGetInterRawSadsINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        payload.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_variable_length_array_intel(&mut self, result_type: Type, lenght: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::VariableLengthArrayINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        lenght.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_save_memory_intel(&mut self, result_type: Type) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::SaveMemoryINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_restore_memory_intel(&mut self, ptr: ValueId) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinCosPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        from_sign.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastFromIntINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        from_sign.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCastToIntINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatAddINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSubINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatMulINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatDivINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_arbitrary_float_gtintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatGTINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_arbitrary_float_geintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatGEINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_arbitrary_float_ltintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLTINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_arbitrary_float_leintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLEINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_arbitrary_float_eqintel(
        &mut self,
        result_type: Type,
        a: ValueId,
        m1: i32,
        b: ValueId,
        m2: i32,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatEQINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatRecipINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatRSqrtINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCbrtINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatHypotINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSqrtINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLogINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog2INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog10INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatLog1pINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExpINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExp2INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExp10INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatExpm1INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinCosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatSinPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatCosPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatASinINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatASinPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatACosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatACosPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATanINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATanPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatATan2INTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowRINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        m2.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ArbitraryFloatPowNINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        m1.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        mout.write_operand(&mut inst_builder);
        enable_subnormals.write_operand(&mut inst_builder);
        rounding_mode.write_operand(&mut inst_builder);
        rounding_accuracy.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_loop_control_intel(&mut self, loop_control_parameters: &[i32]) {
        let next_id = self.next_op_id();
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::LoopControlINTEL);
        loop_control_parameters.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
    }
    pub fn emit_alias_domain_decl_intel(&mut self, name: Option<ValueId>) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasDomainDeclINTEL);
        result.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_alias_scope_decl_intel(&mut self, alias_domain: ValueId, name: Option<ValueId>) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasScopeDeclINTEL);
        result.write_operand(&mut inst_builder);
        alias_domain.write_operand(&mut inst_builder);
        name.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_alias_scope_list_decl_intel(&mut self, alias_scope1_alias_scope2: &[ValueId]) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AliasScopeListDeclINTEL);
        result.write_operand(&mut inst_builder);
        alias_scope1_alias_scope2.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSqrtINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedRecipINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedRsqrtINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedCosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinCosINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedCosPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedSinCosPiINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedLogINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FixedExpINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input_type.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        s.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        r_i.write_operand(&mut inst_builder);
        q.write_operand(&mut inst_builder);
        o.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ptr_cast_to_cross_workgroup_intel(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::PtrCastToCrossWorkgroupINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_cross_workgroup_cast_to_ptr_intel(&mut self, result_type: Type, pointer: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::CrossWorkgroupCastToPtrINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_read_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::ReadPipeBlockingINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_write_pipe_blocking_intel(
        &mut self,
        result_type: Type,
        packet_size: ValueId,
        packet_alignment: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::WritePipeBlockingINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        packet_size.write_operand(&mut inst_builder);
        packet_alignment.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_fpga_reg_intel(&mut self, result_type: Type, result: ValueId, input: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::FPGARegINTEL);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        input.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_ray_t_min_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetRayTMinKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_ray_flags_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetRayFlagsKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_tkhr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionTKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_instance_custom_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceCustomIndexKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_instance_id_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceIdKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_instance_shader_binding_table_record_offset_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder =
            InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionInstanceShaderBindingTableRecordOffsetKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_geometry_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionGeometryIndexKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_primitive_index_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionPrimitiveIndexKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_barycentrics_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionBarycentricsKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_front_face_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionFrontFaceKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_candidate_aabb_opaque_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionCandidateAABBOpaqueKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_object_ray_direction_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectRayDirectionKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_object_ray_origin_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectRayOriginKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_world_ray_direction_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetWorldRayDirectionKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_world_ray_origin_khr(&mut self, result_type: Type, ray_query: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetWorldRayOriginKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_object_to_world_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionObjectToWorldKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_ray_query_get_intersection_world_to_object_khr(
        &mut self,
        result_type: Type,
        ray_query: ValueId,
        intersection: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::RayQueryGetIntersectionWorldToObjectKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        ray_query.write_operand(&mut inst_builder);
        intersection.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_atomic_f_add_ext(
        &mut self,
        result_type: Type,
        pointer: ValueId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
        value: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::AtomicFAddEXT);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        pointer.write_operand(&mut inst_builder);
        memory.write_operand(&mut inst_builder);
        semantics.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_control_barrier_arrive_intel(
        &mut self,
        execution: ScopeId,
        memory: ScopeId,
        semantics: MemorySemanticsId,
    ) {
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
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
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupIMulKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_f_mul_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupFMulKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_bitwise_and_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseAndKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_bitwise_or_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseOrKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_bitwise_xor_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupBitwiseXorKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_logical_and_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalAndKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_logical_or_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalOrKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_group_logical_xor_khr(
        &mut self,
        result_type: Type,
        execution: ScopeId,
        operation: spirv_headers::GroupOperation,
        x: ValueId,
    ) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let mut inst_builder = InstBuilder::new(spirv_headers::Op::GroupLogicalXorKHR);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        execution.write_operand(&mut inst_builder);
        operation.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
}

// --- GLSL.std.450 ---
impl Builder {
    pub fn emit_glsl_round(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 1i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_round_even(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 2i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_trunc(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 3i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_abs(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 4i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_s_abs(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 5i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_sign(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 6i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_s_sign(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 7i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_floor(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 8i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_ceil(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 9i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_fract(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 10i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_radians(&mut self, degrees: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 11i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        degrees.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_degrees(&mut self, radians: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 12i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        radians.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_sin(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 13i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_cos(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 14i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_tan(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 15i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_asin(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 16i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_acos(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 17i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_atan(&mut self, y_over_x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 18i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        y_over_x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_sinh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 19i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_cosh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 20i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_tanh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 21i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_asinh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 22i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_acosh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 23i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_atanh(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 24i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_atan2(&mut self, y: ValueId, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 25i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pow(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 26i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_exp(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 27i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_log(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 28i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_exp2(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 29i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_log2(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 30i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_sqrt(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 31i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_inverse_sqrt(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 32i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_determinant(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 33i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_matrix_inverse(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 34i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_modf(&mut self, x: ValueId, i: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 35i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_modf_struct(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 36i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_min(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 37i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_u_min(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 38i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_s_min(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 39i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_max(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 40i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_u_max(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 41i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_s_max(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 42i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_clamp(&mut self, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 43i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_u_clamp(&mut self, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 44i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_s_clamp(&mut self, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 45i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_f_mix(&mut self, x: ValueId, y: ValueId, a: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 46i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_i_mix(&mut self, x: ValueId, y: ValueId, a: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 47i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_step(&mut self, edge: ValueId, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 48i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        edge.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_smooth_step(&mut self, edge0: ValueId, edge1: ValueId, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 49i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        edge0.write_operand(&mut inst_builder);
        edge1.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_fma(&mut self, a: ValueId, b: ValueId, c: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 50i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        a.write_operand(&mut inst_builder);
        b.write_operand(&mut inst_builder);
        c.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_frexp(&mut self, x: ValueId, exp: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 51i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_frexp_struct(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 52i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_ldexp(&mut self, x: ValueId, exp: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 53i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        exp.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_snorm4x8(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 54i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_unorm4x8(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 55i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_snorm2x16(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 56i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_unorm2x16(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 57i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_half2x16(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 58i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_pack_double2x32(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 59i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_snorm2x16(&mut self, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 60i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_unorm2x16(&mut self, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 61i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_half2x16(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 62i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_snorm4x8(&mut self, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 63i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_unorm4x8(&mut self, p: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 64i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_unpack_double2x32(&mut self, v: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 65i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        v.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_length(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 66i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_distance(&mut self, p0: ValueId, p1: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 67i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        p0.write_operand(&mut inst_builder);
        p1.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_cross(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 68i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_normalize(&mut self, x: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 69i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_face_forward(&mut self, n: ValueId, i: ValueId, nref: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 70i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        nref.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_reflect(&mut self, i: ValueId, n: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 71i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_refract(&mut self, i: ValueId, n: ValueId, eta: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 72i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        i.write_operand(&mut inst_builder);
        n.write_operand(&mut inst_builder);
        eta.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_find_i_lsb(&mut self, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 73i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_find_s_msb(&mut self, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 74i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_find_u_msb(&mut self, value: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 75i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        value.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_interpolate_at_centroid(&mut self, interpolant: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 76i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        interpolant.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_interpolate_at_sample(&mut self, interpolant: ValueId, sample: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 77i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        interpolant.write_operand(&mut inst_builder);
        sample.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_interpolate_at_offset(&mut self, interpolant: ValueId, offset: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 78i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        interpolant.write_operand(&mut inst_builder);
        offset.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_n_min(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 79i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_n_max(&mut self, x: ValueId, y: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 80i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        y.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
    pub fn emit_glsl_n_clamp(&mut self, x: ValueId, min_val: ValueId, max_val: ValueId) -> ValueId {
        let next_id = self.next_op_id();
        let result = self.alloc_value(Value::result(next_id, result_type));
        let ext_id = self.import_extended_instruction_set("GLSL.std.450");
        let mut inst_builder = InstBuilder::new_ext_inst(ext_id, 81i64);
        result_type.write_operand(&mut inst_builder);
        result.write_operand(&mut inst_builder);
        x.write_operand(&mut inst_builder);
        min_val.write_operand(&mut inst_builder);
        max_val.write_operand(&mut inst_builder);
        self.append_inst(inst_builder);
        result
    }
}
