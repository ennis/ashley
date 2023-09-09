use ashley_db::{
    database_tables, new_index,
    table::{DerivedQueryTable, HasTables, InputIndex, InputTable, InternTable},
    Database, DbIndex, Idx, Revision, Runtime, TableOps,
};

struct Package;
struct PackageDefinitions;
struct DefIdTable;

new_index! { pub struct PackageId; }
new_index! { pub struct DefId; }

/// Namespace of a definition name.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum Namespace {
    Type,
    Function,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DefName {
    package: PackageId,
    namespace: Namespace,
    name: String,
}

// Types of objects:
// - Inputs: Query where Q::Storage = Input<Self>
// - Derived queries: Q::Storage = DerivedQueryData<Self>
// - Interned structs: not a query?

// How to identify tables?
// - input tables may not have a unique type
// - derived query tables can have one if we represent the Query as a struct with a trait impl
// - interning tables may not have a unique type

// From the user POV:
// To run a query, use `db.the_query(key)`. Query is specified as an extension trait on the Database.
// For inputs, use `db.set_input(...)`
// Identify tables with unique types (not necessarily public).
//
// Explicit query/input registration?
// Database is not accessed directly, there's a wrapper type for the queries added on top.
// e.g. `MyDatabase(Database, query_index, query_index_2)`.
//
// Add things on top:
// `MyRefinedDatabase(MyDatabase, query_index_3)`, derefs to MyDatabase
//
// => Saves a hash map lookup, can be macro-generated
//
// Issue: this means that the final type of the database must be propagated to TableOps
// (because the query functions will expect the full database type)
// I.e. table types must implement TableOps<Self>.
//
// Now the table storage is not "type-erased" anymore, they live as explicit fields in structs.
// We still need a way to refer to them by some numerical ID (for dependency tracking),
// but it's not an index into anything.
//

trait MyDatabaseTrait: HasTables<MyDatabaseTables> {}

// #[derive(Database)]
struct MyDatabase {
    input_rev: Revision,
    runtime: Runtime,
    tables: MyDatabaseTables,
}

struct MyDatabaseTables {
    package_ids: InputIndex<PackageId>,
    package_source: InputTable<PackageId, String>,
    package_definitions: DerivedQueryTable<PackageId, Vec<DefId>, dyn MyDatabaseTrait>,
    def_ids: InternTable<DefName, DefId>,
}

impl MyDatabaseTables {
    fn new(base_index: usize) -> MyDatabaseTables {
        let package_ids = InputIndex::new(base_index + 0);
        let package_source = InputTable::new(base_index + 1);
        let package_definitions = DerivedQueryTable::new(base_index + 2, package_definitions);
        let def_ids = InternTable::new(base_index + 3);

        MyDatabaseTables {
            package_ids,
            package_source,
            package_definitions,
            def_ids,
        }
    }

    // should be automatically generated
    fn maybe_changed_after(&self, db: &dyn MyDatabaseTrait, dep_index: DbIndex, after: Revision) -> bool {
        match dep_index.table_index {
            x if x == <_ as TableOps<dyn MyDatabaseTrait>>::table_index(&self.package_ids) => {
                self.package_ids.maybe_changed_after(db, dep_index.index, after)
            }
            x if x == <_ as TableOps<dyn MyDatabaseTrait>>::table_index(&self.package_source) => {
                self.package_source.maybe_changed_after(db, dep_index.index, after)
            }
            x if x == <_ as TableOps<dyn MyDatabaseTrait>>::table_index(&self.package_definitions) => {
                self.package_definitions.maybe_changed_after(db, dep_index.index, after)
            }
            x if x == <_ as TableOps<dyn MyDatabaseTrait>>::table_index(&self.def_ids) => {
                self.def_ids.maybe_changed_after(db, dep_index.index, after)
            }
            _ => false,
        }
    }
}

impl MyDatabase {
    fn new() -> MyDatabase {
        MyDatabase {
            input_rev: 0,
            runtime: Runtime::new(),
            tables: MyDatabaseTables::new(0),
        }
    }

    fn create_source_package(&mut self, source: String) -> PackageId {
        let rev = self.next_revision();
        let package = self.tables.package_ids.new_input();
        self.tables.package_source.set(rev, package, source);
        package
    }

    fn update_source(&mut self, package: PackageId, new_source: String) {
        let rev = self.next_revision();
        self.tables.package_source.set(rev, package, new_source);
    }

    fn package_source(&self, package_id: PackageId) -> &String {
        self.tables.package_source.fetch(self, package_id)
    }

    fn package_definitions(&self, package_id: PackageId) -> &Vec<DefId> {
        self.tables.package_definitions.fetch(self, package_id)
    }

    fn def_id(&self, package: PackageId, name: String, namespace: Namespace) -> DefId {
        self.tables.def_ids.intern(DefName {
            package,
            namespace,
            name,
        })
    }
}

impl Database for MyDatabase {
    fn current_revision(&self) -> Revision {
        self.input_rev
    }

    fn next_revision(&mut self) -> Revision {
        self.input_rev += 1;
        <_ as TableOps<dyn MyDatabaseTrait>>::new_revision(&mut self.tables.package_source);
        <_ as TableOps<dyn MyDatabaseTrait>>::new_revision(&mut self.tables.package_ids);
        <_ as TableOps<dyn MyDatabaseTrait>>::new_revision(&mut self.tables.package_definitions);
        <_ as TableOps<dyn MyDatabaseTrait>>::new_revision(&mut self.tables.def_ids);
        self.input_rev
    }

    // should be automatically generated
    fn maybe_changed_after(&self, dep_index: DbIndex, after: Revision) -> bool {
        self.tables.maybe_changed_after(self, dep_index, after)
    }

    fn runtime(&self) -> &Runtime {
        &self.runtime
    }
}

impl HasTables<MyDatabaseTables> for MyDatabase {
    fn tables(&self) -> &MyDatabaseTables {
        &self.tables
    }
}

impl<DB> MyDatabaseTrait for DB where DB: ?Sized + HasTables<MyDatabaseTables> {}

fn package_definitions(db: &dyn MyDatabaseTrait, package_id: &PackageId) -> Vec<DefId> {
    eprintln!("computing package definitions");
    let _source = db.tables().package_source.fetch(db, *package_id);
    vec![]
}

////////////////////////////////////////////////////////////////////////////////////////////////////

struct ExtDatabaseTables {
    package_analysis: DerivedQueryTable<PackageId, (), dyn ExtDatabaseTrait>,
}

fn package_analysis(db: &dyn ExtDatabaseTrait, package: &PackageId) -> () {
    ()
}

impl ExtDatabaseTables {
    fn new(base_index: usize) -> ExtDatabaseTables {
        ExtDatabaseTables {
            package_analysis: DerivedQueryTable::new(base_index, package_analysis),
        }
    }

    // should be automatically generated
    fn maybe_changed_after(&self, db: &dyn ExtDatabaseTrait, dep_index: DbIndex, after: Revision) -> bool {
        match dep_index.table_index {
            x if x == <_ as TableOps<dyn ExtDatabaseTrait>>::table_index(&self.package_analysis) => {
                self.package_analysis.maybe_changed_after(db, dep_index.index, after)
            }
            _ => false,
        }
    }
}

trait ExtDatabaseTrait: MyDatabaseTrait + HasTables<ExtDatabaseTables> {}

struct ExtDatabase {
    input_rev: Revision,
    runtime: Runtime,
    base: MyDatabaseTables,
    additional: ExtDatabaseTables,
}

impl ExtDatabase {
    fn new() -> ExtDatabase {
        ExtDatabase {
            input_rev: 0,
            runtime: Runtime::new(),
            base: MyDatabaseTables::new(0),
            additional: ExtDatabaseTables::new(4),
        }
    }
}

impl<DB> ExtDatabaseTrait for DB where DB: ?Sized + MyDatabaseTrait + HasTables<ExtDatabaseTables> {}

impl Database for ExtDatabase {
    fn current_revision(&self) -> Revision {
        self.input_rev
    }

    fn next_revision(&mut self) -> Revision {
        self.input_rev += 1;
        self.input_rev
    }

    fn maybe_changed_after(&self, dep_index: DbIndex, after: Revision) -> bool {
        if dep_index.table_index < 4 {
            self.base.maybe_changed_after(self, dep_index, after)
        } else {
            self.additional.maybe_changed_after(self, dep_index, after)
        }
    }

    fn runtime(&self) -> &Runtime {
        &self.runtime
    }
}

impl HasTables<MyDatabaseTables> for ExtDatabase {
    fn tables(&self) -> &MyDatabaseTables {
        &self.base
    }
}

impl HasTables<ExtDatabaseTables> for ExtDatabase {
    fn tables(&self) -> &ExtDatabaseTables {
        &self.additional
    }
}

#[test]
fn test_query_system_input() {
    let mut db = MyDatabase::new();

    let pkg = db.create_source_package("test package".to_string());
    let defs = db.package_definitions(pkg);
    let defs2 = db.package_definitions(pkg);
    assert_eq!(defs, defs2);

    db.update_source(pkg, "test package, updated".to_string());
    let defs3 = db.package_definitions(pkg);
}

//
// Defining tables:
// - Storage struct name
// - Trait
// - Required tables trait
//

/*database_tables! {
    struct ExtTables : dyn ExtTablesTrait {
        package_ids: InputIndex<PackageId>,
        package_source: InputTable<PackageId, String>,
        package_definitions: DerivedQueryTable<PackageId, Vec<DefId>, dyn ExtTablesTrait> = package_definitions,
        def_ids: InternTable<DefName, DefId>
    }
}

pub trait ExtTablesTrait: HasTables<ExtTables> + MyDatabaseTrait {}
impl<DB> ExtTablesTrait for DB where DB: ?Sized + MyDatabaseTrait + HasTables<ExtTables> {}
*/
