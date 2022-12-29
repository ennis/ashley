use crate::{
    dialect::{
        base,
        base::{ScalarType, ScalarTypeKind, UnitType, UnknownType},
    },
    hir::{
        Attr, AttributeBase, Cursor, FloatAttr, HirCtxt, IntegerAttr, Location, Operation, OperationConstraint,
        OperationData, OperationId, RegionData, RegionId, StringAttr, Value, ValueId, ValueKind,
    },
};
use bumpalo::Bump;
use ordered_float::OrderedFloat;
use std::{collections::HashMap, fmt, hash::Hash};
use std::ops::Deref;

pub struct Builder<'a, 'hir> {
    pub ctxt: &'a mut HirCtxt<'hir>,
    diag: &'a Diagnostics,
    at: Cursor,
}

impl<'a, 'hir>  Deref for Builder<'a, 'hir> {
    type Target = HirCtxt<'hir>;

    fn deref(&self) -> &Self::Target {
        &*self.ctxt
    }
}

macro_rules! interned_attr_methods {
    ( $($(#[$m:meta])* $v:vis $name:ident -> $t:ty { $e:expr })* ) => {
        $($(#[$m])* $v fn $name(&self) -> Attr<'hir,$t> {
            Attr(self.ctxt.interner.intern(self.ctxt.arena, $e).0)
        })*
    };
}

impl<'a, 'hir> Builder<'a, 'hir> {
    /// Creates a new HIR builder that will append instructions to the specified region.
    pub fn new(ctxt: &'a mut HirCtxt<'hir>, diag: &'a Diagnostics, region: RegionId) -> Builder<'a, 'hir> {
        Builder {
            ctxt,
            diag,
            at: Cursor::End(region),
        }
    }

    /// Returns the underlying HirCtxt.
    pub fn ctxt_mut(&mut self) -> &mut HirCtxt<'hir> {
        self.ctxt
    }

    ///
    pub fn build_region<'b>(&'b mut self, region: RegionId) -> Builder<'b, 'hir>
    where
        'a: 'b,
    {
        Builder {
            ctxt: self.ctxt,
            diag: self.diag,
            at: Cursor::End(region),
        }
    }

    /// Returns the diagnostics instance associated with this builder.
    pub fn diagnostics(&self) -> &'a Diagnostics {
        self.diag
    }

    /// Returns the first subregion of an operation.
    pub fn first_region(&self, op: OperationId) -> Option<RegionId> {
        self.ctxt.ops[op].data.regions.get(0).cloned()
    }

    /// Returns the second subregion of an operation.
    pub fn second_region(&self, op: OperationId) -> Option<RegionId> {
        self.ctxt.ops[op].data.regions.get(1).cloned()
    }

    /// Returns the arguments of the specified region.
    pub fn region_arguments(&self, region: RegionId) -> &'hir [ValueId] {
        self.ctxt.regions[region].arguments
    }

    /// Returns the type of the specified value.
    pub fn value_type(&self, value: ValueId) -> Attr<'hir> {
        self.ctxt.values[value].ty
    }

    /// Returns the types of the specified values.
    pub fn value_types(&self, values: &[ValueId]) -> Vec<Attr<'hir>> {
        values.iter().map(|v| self.value_type(*v)).collect()
    }

    /// Returns the operands of the specified operation.
    pub fn operands(&self, op: OperationId) -> &'hir [ValueId] {
        self.ctxt.ops[op].data.operands
    }

    /// Returns the results of the specified operation.
    pub fn op_results(&self, op: OperationId) -> &'hir [ValueId] {
        self.ctxt.ops[op].data.results
    }

    /// Returns the location associated to the value.
    pub fn value_location(&self, value: ValueId) -> Location {
        match self.ctxt.values[value].kind {
            ValueKind::OpResult(op, index) => self.ctxt.ops[op].data.location,
            ValueKind::RegionArg(_, _) => {
                // TODO: region argument locations
                Location::Unknown
            }
        }
    }

    /// Creates an operation at the current insertion point.
    pub fn build_op(&mut self, operation_create_info: OperationCreateInfo<'hir>) -> (OperationId, &'hir [ValueId]) {
        self.ctxt.create_operation(self.at, operation_create_info)
    }

    /// Creates an operation at the current insertion point and applies the operation pattern.
    pub fn build_op2<U>(&mut self, operation_create_info: OperationCreateInfo<'hir>) -> U
    where
        U: OperationConstraint<'hir>,
    {
        self.ctxt
            .create_operation(self.at, operation_create_info)
            .0
            .pmatch(self)
            .expect("invalid operation format")
    }

    /// Creates a new region.
    pub fn create_region(&mut self) -> RegionId {
        self.ctxt.create_region()
    }

    /// Emits the "undef" instruction at the cursor position and returns the value.
    pub fn undef(&mut self) -> ValueId {
        let mut create_info = OperationCreateInfo::new("undef", Location::Unknown);
        create_info.add_result_types(&[self.unknown_type().upcast()]);
        self.build_op(create_info).1[0]
    }

    /// Emits the "undef" instruction at the cursor position and returns the value.
    pub fn undef_typed(&mut self, ty: Attr<'hir>) -> ValueId {
        let mut create_info = OperationCreateInfo::new("undef", Location::Unknown);
        create_info.add_result_types(&[ty]);
        self.build_op(create_info).1[0]
    }

    /// Emits a floating-point constant.
    pub fn fp_const(&mut self, value: f64) -> Attr<'hir> {
        let attr = self.ctxt.intern_attr(FloatAttr(OrderedFloat::from(value)));
        attr.upcast()
    }

    /// Emits an integer constant.
    pub fn int_const(&mut self, value: i128) -> Attr<'hir> {
        let attr = self.ctxt.intern_attr(IntegerAttr(value));
        attr.upcast()
    }

    /// Emits a boolean constant.
    pub fn bool_const(&mut self, value: bool) -> Attr<'hir> {
        let attr = self.ctxt.intern_attr(BooleanAttr(value));
        attr.upcast()
    }

    interned_attr_methods!(
        /// Returns the unknown type.
        pub unknown_type -> base::UnknownType { UnknownType }
        /// Returns the unit type.
        pub unit_type -> base::UnitType { UnitType }
        /// Returns the f32 type.
        pub f32_ty -> base::ScalarType { ScalarType(ScalarTypeKind::Float) }
        /// Returns the f64 type.
        pub f64_ty -> base::ScalarType { ScalarType(ScalarTypeKind::Double) }
        /// Returns the u32 type.
        pub u32_ty -> base::ScalarType { ScalarType(ScalarTypeKind::UnsignedInt) }
        /// Returns the i32 type.
        pub i32_ty -> base::ScalarType { ScalarType(ScalarTypeKind::Int) }
        /// Returns the bool type.
        pub bool_ty -> base::ScalarType { ScalarType(ScalarTypeKind::Bool) }
    );
}

/// Helper macro to define an operation format.
///
/// Given a list of operation patterns, this defines a struct with all the components matched by the patterns.
#[macro_export]
macro_rules! operation_format {

    // version with arena lifetime annotation
    ($(#[$m:meta])* $v:vis $ty:ident <$lt:lifetime>: $($patterns:pat),* $(, |$builder:ident| $constraint_block:block)? => { $($(#[$bm:meta])* $bindings:ident : $binding_types:ty),* } ) => {
        $(#[$m])*
        #[derive(Copy,Clone,Debug)]
        $v struct $ty <$lt> {
            $($(#[$bm])* $v $bindings : $binding_types),*
        }

        impl<$lt> $crate::hir::OperationConstraint<$lt> for $ty<$lt> {
            fn try_match(b: &$crate::hir::Builder<'_, $lt>, op: $crate::hir::OperationId) -> Option<$ty<$lt>> {
                $(let $patterns = op.pmatch(b)?;)*

                Some($ty {
                    $($bindings : $bindings),*
                })
            }
        }
    };

    // version without lifetimes
    ($(#[$m:meta])* $v:vis $ty:ident: $($patterns:pat),* =>  { $($(#[$bm:meta])* $bindings:ident : $binding_types:ty),* } ) => {
        $(#[$m])*
        #[derive(Copy,Clone,Debug)]
        $v struct $ty {
            $($(#[$bm])* $v $bindings : $binding_types),*
        }

        impl<'ir> $crate::hir::OperationConstraint<'ir> for $ty {
            fn try_match(b: &$crate::hir::Builder<'_, 'ir>, op: $crate::hir::OperationId) -> Option<$ty> {
                $(let $patterns = op.pmatch(b)?;)*
                Some($ty {
                    $($bindings : $bindings),*
                })
            }
        }
    };
}

/// Helper macro to define an operation formats.
///
/// See `operation_format`.
#[macro_export]
macro_rules! operation_formats {
    ( $($(#[$m:meta])* $v:vis $ty:ident $(<$lt:lifetime>)?: $($patterns:pat),* => { $($(#[$bm:meta])* $bindings:ident : $binding_types:ty),* };)* ) => {
        $( $crate::hir::operation_format!{ $(#[$m])* $v $ty $(<$lt>)?: $($patterns),* => { $($(#[$bm])* $bindings : $binding_types),* } } )*
    };
}

pub use operation_format;
pub use operation_formats;

/// Helper macro to build an operation.
#[macro_export]
macro_rules! build_operation {
    (@oplist $opb:expr;) => {};
    (@oplist $opb:expr; ~$operand:expr $(,$($rest:tt)*)? ) => {
          $opb.extend($operand);
          build_operation!(@oplist $opb; $($($rest)*)*);
    };
    (@oplist $opb:expr; $operand:expr $(,$($rest:tt)*)? ) => {
          $opb.extend(&[$operand]);
          build_operation!(@oplist $opb; $($($rest)*)*);
    };
    ($b:expr; $mnemonic:literal $([$($attributes:tt)*])? $(($($operands:tt)*))? $({ $($regions:ident($($region_arguments:tt)*)),* })? $(-> ($($result_types:tt)*))? $(at $loc:expr)?) => {
        {
            let loc = Location::Unknown;
            $(let loc = $loc;)?
            let mut opb = $crate::hir::OperationCreateInfo::new($mnemonic, loc);
            $(
                let mut attributes = vec![];
                build_operation!(@oplist attributes; $($attributes)*);
                opb.add_attributes(attributes);
            )?
            $(
                let mut operands = vec![];
                build_operation!(@oplist operands; $($operands)*);
                opb.add_operands(operands);
            )?
            $($(
                let mut region_args = vec![];
                build_operation!(@oplist region_args; $($region_arguments)*);
                opb.add_region(region_args);
            )*)?
            $(
                let mut result_types = vec![];
                build_operation!(@oplist result_types; $($result_types)*);
                opb.add_result_types(result_types);
            )?
            $b.build_op2(opb)
        }
    };
}
use crate::{diagnostic::Diagnostics, hir::OperationCreateInfo};
pub use build_operation;
use crate::utils::interner::{AttrInterner, BooleanAttr};
