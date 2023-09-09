//! Query system
mod database;
mod db_index;
mod revision;
mod runtime;
pub mod table;

// Re-export stuff used by our macros
pub use ashley_data_structures::{new_index, Idx};

pub use database::{Database, DatabaseExt};
pub use db_index::DbIndex;
pub use revision::Revision;
pub use runtime::Runtime;
pub use table::{AsIndex, Index, TableIndex, TableOps};

/// Big-ass macro to define data tables
#[macro_export]
macro_rules! define_database_tables {
    (@ $(#[$m:meta])* $v:vis $name:ident [$dbty:ty] { }
        -> ($($(#[$field_meta:meta])* $field_name:ident($field_index:expr): $field_type:ty = $field_init:expr,)*)
        -> ($bti:ident, $counter:expr)
    )
    => {
        $(#[$m])* $v struct $name {
            base_table_index: usize,
            $($(#[$field_meta])* $field_name: $field_type,)*
        }

        impl $name {
            $v fn new($bti: usize) -> $name {
                $(let $field_name : $field_type = $field_init;)*
                $name {
                    base_table_index: $bti,
                    $($field_name,)*
                }
            }

            // maybe_changed_after impl
            $v fn maybe_changed_after(&self, db: &$dbty, dbindex: $crate::DbIndex, after: $crate::Revision) -> Option<bool> {
                $(if dbindex.table == $crate::TableIndex::from(self.base_table_index + $field_index)  { return Some(self.$field_name.maybe_changed_after(db, dbindex.value, after)); } )*
                return None;
            }
        }
    };

    // intern
    (@ $(#[$m:meta])* $v:vis $name:ident [$dbty:ty] { $(#[$qm:meta])* intern $tabname:ident ($tykey:ty) -> $tyindex:ty; $($rest:tt)* }
        -> ($($r:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $name [$dbty] {
                $($rest)*
            }
            -> ($($r)* $(#[$qm])* $tabname($counter): $crate::table::InternTable<$tykey,$tyindex> = <$crate::table::InternTable<$tykey,$tyindex>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };

    // index
    (@ $(#[$m:meta])* $v:vis $name:ident [$dbty:ty] { $(#[$qm:meta])* index $tabname:ident($tyindex:ty); $($rest:tt)* }
        -> ($($r:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $name [$dbty] {
                $($rest)*
            }
            -> ($($r)* $(#[$qm])* $tabname($counter): $crate::table::InputIndex<$tyindex> = <$crate::table::InputIndex<$tyindex>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };

    // input
    (@ $(#[$m:meta])* $v:vis $name:ident [$dbty:ty] { $(#[$qm:meta])* input $tabname:ident ($tyindex:ty) -> $tydata:ty; $($rest:tt)* }
        -> ($($r:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $name [$dbty] { $($rest)* }
            -> ($($r)* $(#[$qm])* $tabname($counter): $crate::table::InputTable<$tyindex,$tydata> = <$crate::table::InputTable<$tyindex,$tydata>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };

    // query
    (@ $(#[$m:meta])* $v:vis $name:ident [$dbty:ty] { $(#[$qm:meta])* query $query_name:ident($argdb:ident: &$dbty2:ty, $argkey:ident: &$tykey:ty) -> $tyresult:ty $query_body:block $($rest:tt)* }
        -> ($($r:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $name [$dbty] {
                $($rest)*
            }
            -> ( $($r)* $(#[$qm])* $query_name($counter): $crate::table::DerivedQueryTable<$tykey,$tyresult,$dbty> = <$crate::table::DerivedQueryTable<$tykey,$tyresult,$dbty>>::new($crate::TableIndex::from($bti+$counter), |$argdb: &$dbty2, $argkey: &$tykey| -> $tyresult {$query_body}), )
            -> ($bti, $counter+1)
        );
    };

    ($(#[$m:meta])* $v:vis struct $name:ident [$dbty:ty] { $($rest:tt)* } ) => {
        define_database_tables!(@ $(#[$m])* $v $name  [$dbty] { $($rest)* }
            -> (/* #[query_meta] field_name(field_index): field_ty = field_initializer, */)
            -> (base_table_index /* base table index ident */, 0 /* field index */)
        );
    };
}

/*define_database_tables!(
    /// Compiler DB
    #[derive(Debug)]
    struct CompilerDbData [dyn CompilerDb]
    {
        intern module_id: String -> ModuleId;
        intern def_id: DefName -> DefId;

        index source_file_id: SourceFileId;

        input module_source: ModuleId -> SourceFileId;
        input source_file: SourceFileId -> SourceFile;
        input module_data: ModuleId -> ModuleData;

        query syntax_tree(db: &dyn CompilerDb, source_file_id: &SourceFileId) -> SyntaxTree {}
        query module_definitions(db: &dyn CompilerDb, module_id: &ModuleId) -> Vec<ModuleId> {}
    }
);*/

////////////////////////////////////////////////////////////////////////////////////////////////////

//
// DbKey: identifies a value in the database, comprised of a table index identifying the table, and the index within the table.
// Table: a generic term for the storage for queries, inputs, interners
//

////////////////////////////////////////////////////////////////////////////////////////////////////

// Should the query system be extensible?
// Clients other than the compiler would be able to add their own queries and derived computations.
// Could be useful for analyses.

// Q: when should nodes be removed?
// Example: a bunch of lines are removed from the source, resulting in removed definitions
// But the old definitions themselves remain in the query cache (some DefIds may be reused though).
// -> note that affected definitions will be marked "red", so at some point we can "cull" red cache entries

// Should query results be re-evaluated eagerly?
// -> not sure

// Algorithm:
// - each DepNode has a revision index
// - when some data is queried, traverse the inputs of the queries; if any input DepNode has a more recent rev index,
//   then the query must be recomputed (do that recursively). The rev index of the dep node is updated with the max
//   rev index of the inputs

// On input change:
// - update the rev index of the input (we have one global rev counter, increase it and set the input rev to that value)
// -

// How are DefIds produced?
// First, package sources are provided as input.
// Then, the `definitions(package_id)` query is invoked.
// The query scans the AST and produces a complete list of definitions in an `IndexVec<DefId, Definition>`.
// This vec is the output value of the query, and is cached for future reference.
//
// This invalidates any previous queries that depend on `definitions`. Notably, "projection queries" that
// return a definition from a DefId are all invalidated. However, when re-running projection queries, we can
// compare the result with the previous value. If it's the same we don't need to re-run the queries that depend on the projection
// (see https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html#the-projection-query-pattern).
//
// The main issue is that the data of the definitions is effectively duplicated: once in the output value of
// the `definitions` query (the IndexVec), and once in the outputs of each projection queries over the definitions.
// This is wasteful, as we could have only one IndexVec containing the definitions.
//
// Alternative: some queries can produce more than one value.
//
