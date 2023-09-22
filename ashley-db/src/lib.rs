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
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { }
        -> ($($input_name:ident [$input_setter:ident] ($input_key_name:ident: $input_key_ty:ty) -> $input_result_ty:ty;)*)
        -> ($($intern_name:ident [$lookup_name:ident] ($intern_key_name:ident: $intern_key_ty:ty) -> $intern_result_ty:ty;)*)
        -> ($($query_name:ident ($query_key_name:ident: $query_key_ty:ty) -> $query_result_ty:ty;)*)
        -> ($($specify_name:ident [$specify_query_name:ident] ($specify_key_ty:ty, $specify_result_ty:ty);)*)
        -> ($($(#[$field_meta:meta])* $field_name:ident($field_index:expr): $field_type:ty = $field_init:expr,)*)
        -> ($bti:ident, $counter:expr)
    )
    => {
        $(#[$m])* $v struct $storage {
            base_table_index: usize,
            $($(#[$field_meta])* $field_name: $field_type,)*
        }

        impl $storage {
            $v fn new($bti: usize) -> $storage {
                $(let $field_name : $field_type = $field_init;)*
                $storage {
                    base_table_index: $bti,
                    $($field_name,)*
                }
            }

            // maybe_changed_after impl
            $v fn maybe_changed_after(&self, db: &dyn $db_trait, dbindex: $crate::DbIndex, after: $crate::Revision) -> Option<bool> {
                $(if dbindex.table == $crate::TableIndex::from(self.base_table_index + $field_index)  { return Some(self.$field_name.maybe_changed_after(db, dbindex.value, after)); } )*
                return None;
            }

            // on_new_revision impl
            $v fn on_new_revision(&mut self, revision: $crate::Revision) {
                $(<$field_type as $crate::TableOps<dyn $db_trait>>::on_new_revision(&mut self.$field_name, revision);)*
            }
        }

        $v trait $db_trait: $crate::Database {
            $(fn $input_name (&self, $input_key_name: $input_key_ty) -> &<&$input_result_ty as ::std::ops::Deref>::Target;)*
            $(fn $input_setter (&mut self, $input_key_name: $input_key_ty, value: $input_result_ty);)*
            $(fn $intern_name (&self, $intern_key_name: $intern_key_ty) -> $intern_result_ty;)*
            $(fn $lookup_name (&self, key: $intern_result_ty) -> $intern_key_ty;)*
            $(fn $query_name (&self, $query_key_name: $query_key_ty) -> &<&$query_result_ty as ::std::ops::Deref>::Target;)*
            $(fn $specify_name (&self, key: $specify_key_ty, value: $specify_result_ty);)*
        }

        impl<DB> $db_trait for DB
        where DB: $crate::table::HasTables<$storage> {
            $(
            fn $input_name (&self, $input_key_name: $input_key_ty) -> &<&$input_result_ty as ::std::ops::Deref>::Target {
                self.tables().$input_name.fetch(self, $input_key_name)
            }

            fn $input_setter (&mut self, $input_key_name: $input_key_ty, value: $input_result_ty) {
                self.with_new_revision(move |db, rev| {
                    db.tables_mut()
                        .$input_name
                        .update(rev, $input_key_name, move |v| { *v = value; });
                });
            })
            *

            $(
            fn $intern_name (&self, $intern_key_name: $intern_key_ty) -> $intern_result_ty {
                self.tables().$intern_name.intern($intern_key_name)
            }

            fn $lookup_name (&self, key: $intern_result_ty) -> $intern_key_ty {
                self.tables().$intern_name.fetch(key)
            }
            )*

            $(
            fn $query_name (&self, $query_key_name: $query_key_ty) ->  &<&$query_result_ty as ::std::ops::Deref>::Target {
                &self.tables().$query_name.fetch(self, $query_key_name)
            }
            )*

            $(
            fn $specify_name (&self, key: $specify_key_ty, value: $specify_result_ty) {
                self.tables().$specify_query_name.specify(self, key, value);
            }
            )*
        }
    };

    // intern
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { $(#[$qm:meta])* intern $tabname:ident [$lookup:ident] ($argkey:ident: $tykey:ty) -> $tyindex:ty; $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($specify_methods:tt)*)
        -> ($($storage_fields:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] {
                $($rest)*
            }
            -> ( $($input_methods)* )
            -> ( $($intern_methods)* $tabname [$lookup] ($argkey: $tykey) -> $tyindex; )
            -> ( $($query_methods)* )
            -> ( $($specify_methods)* )
            -> ($($storage_fields)* $(#[$qm])* $tabname($counter): $crate::table::InternTable<$tykey,$tyindex> = <$crate::table::InternTable<$tykey,$tyindex>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };

    /*// index
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { $(#[$qm:meta])* index $tabname:ident($tyindex:ty); $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($r:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] {
                $($rest)*
            }
            -> ( $($input_methods)* )
            -> ( $($intern_methods)* )
            -> ( $($query_methods)* )
            -> ($($r)* $(#[$qm])* $tabname($counter): $crate::table::InputIndex<$tyindex> = <$crate::table::InputIndex<$tyindex>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };*/

    // input
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { $(#[$qm:meta])* input $tabname:ident [$setter:ident] ($argkey:ident: $tykey:ty) -> $tydata:ty; $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($specify_methods:tt)*)
        -> ($($storage_fields:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] { $($rest)* }
            -> ( $($input_methods)* $tabname [$setter] ($argkey: $tykey) -> $tydata; )
            -> ( $($intern_methods)* )
            -> ( $($query_methods)* )
            -> ( $($specify_methods)* )
            -> ($($storage_fields)* $(#[$qm])* $tabname($counter): $crate::table::InputTable<$tykey,$tydata> = <$crate::table::InputTable<$tykey,$tydata>>::new($crate::TableIndex::from($bti+$counter)),)
            -> ($bti, $counter+1)
        );
    };

    // query
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { $(#[$qm:meta])* query $query_name:ident($argkey:ident: $tykey:ty) -> $tyresult:ty => $query_fn:path ; $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($specify_methods:tt)*)
        -> ($($storage_fields:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] {
                $($rest)*
            }
            -> ( $($input_methods)* )
            -> ( $($intern_methods)* )
            -> ( $($query_methods)* $query_name ($argkey: $tykey) -> $tyresult; )
            -> ( $($specify_methods)* )
            -> ( $($storage_fields)* $(#[$qm])* $query_name($counter): $crate::table::DerivedQueryTable<$tykey,$tyresult,dyn $db_trait> = <$crate::table::DerivedQueryTable<$tykey,$tyresult,dyn $db_trait>>::new($crate::TableIndex::from($bti+$counter), $query_fn), )
            -> ($bti, $counter+1)
        );
    };

    // indirect query - terminal node
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { query ($argkey:ident: $tykey:ty) -> {} => $query_fn:path; $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($specify_methods:tt)*)
        -> ($($storage_fields:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] {
                $($rest)*
            }
            -> ( $($input_methods)*  )
            -> ( $($intern_methods)* )
            -> ( $($query_methods)* )
            -> ( $($specify_methods)* )
            -> ( $($storage_fields)* )
            -> ($bti, $counter)
        );
    };

    // indirect query
    (@ $(#[$m:meta])* $v:vis $db_trait:ident [$storage:ident] { query ($argkey:ident: $tykey:ty) -> { $(#[$qm:meta])* $query_part_name:ident : $query_part_result:ty [$query_part_setter:ident] $(, $($query_rest:tt)*)? } => $query_fn:path ; $($rest:tt)* }
        -> ($($input_methods:tt)*)
        -> ($($intern_methods:tt)*)
        -> ($($query_methods:tt)*)
        -> ($($specify_methods:tt)*)
        -> ($($storage_fields:tt)*)
        -> ($bti:ident, $counter:expr)
    ) => {
        define_database_tables!(
            @ $(#[$m])* $v $db_trait [$storage] { query ($argkey: $tykey) -> { $($($query_rest)*)? } => $query_fn; $($rest)* }
            -> ( $($input_methods)* )
            -> ( $($intern_methods)* )
            -> ( $($query_methods)* $query_part_name ($argkey: $tykey) -> $query_part_result; )
            -> ( $($specify_methods)* $query_part_setter [$query_part_name] ($tykey, $query_part_result); )
            -> ( $($storage_fields)* $(#[$qm])* $query_part_name($counter): $crate::table::DerivedQueryTable<$tykey,$query_part_result,dyn $db_trait> = <$crate::table::DerivedQueryTable<$tykey,$query_part_result,dyn $db_trait>>::new_indirect($crate::TableIndex::from($bti+$counter), $query_fn), )
            -> ($bti, $counter+1)
        );
    };

    // main rule
    ($(#[$m:meta])* $v:vis trait $db_trait:ident [$storage:ident] { $($rest:tt)* } ) => {
        define_database_tables!(@ $(#[$m])* $v $db_trait [$storage] { $($rest)* }
            -> (/* input_name[set_input_name](key: KeyType) -> input_return_ty;*/ )
            -> (/* intern_name(key: KeyType) -> intern_return_ty;*/ )
            -> (/* query_name(key: KeyType) -> query_return_ty;*/ )
            -> (/* specify_name[spec_query_name](KeyType, ValueType);*/ )
            -> (/* #[query_meta] field_name(field_index): field_ty = field_initializer, */)
            -> (base_table_index /* base table index ident */, 0 /* field index */)
        );
    };
}

/*define_database_tables!(
    /// Compiler DB
    #[derive(Debug)]
    trait CompilerDb [CompilerDbStorage]
    {
        intern module_id(module_name: String) -> ModuleId;
        intern def_id(def_name: DefName) -> DefId;

        input module_source [set_module_source] (module: ModuleId) -> SourceFileId;
        input source_file   [set_source_file]   (source_file: SourceFileId) -> SourceFile;
        input module_data   [set_module_data]   (module: ModuleId) -> ModuleData;

        query syntax_tree(source_file_id: SourceFileId) -> SyntaxTree => query_syntax_tree;
        query module_definitions(module_id: ModuleId) -> Vec<ModuleId> => query_module_definitions;

        query  (module_id:ModuleId) -> {
            module_items: ModuleItems,
            module_ast_id_map: AstIdMap
        } => item::module_items_and_ast_map_query;
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
