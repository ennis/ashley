use ashley::{
    new_index,
    query::{
        derived::DerivedQueryTable,
        input::{InputIndex, InputTable},
        interned::InternTable,
        Database, DatabaseImpl, DbKey, Revision, TableOps,
    },
    utils::Idx,
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

// #[derive(Database)]
struct MyDatabase {
    //#[inner_db]
    db: DatabaseImpl,
    package_ids: InputIndex<PackageId>,
    package_source: InputTable<PackageId, String>,
    package_definitions: DerivedQueryTable<PackageId, Vec<DefId>, Self>,
    def_ids: InternTable<DefName, DefId>,
}

/*derive_database! {
    database MyDatabase {
        id      PackageId;
        id      DefId(DefName);

        input   package_source       (PackageId) -> String;
        query   package_definitions  (PackageId) -> Vec<DefId>,
        query   definition           (DefId) -> Definition;
    }
}*/

fn package_definitions(db: &MyDatabase, package_id: &PackageId) -> Vec<DefId> {
    eprintln!("computing package definitions");
    let _source = db.package_source(*package_id);
    vec![]
}

impl MyDatabase {
    fn new() -> MyDatabase {
        let mut base_db = DatabaseImpl::new();
        let package_ids = InputIndex::new(&mut base_db);
        let package_source = InputTable::new(&mut base_db);
        let package_definitions = DerivedQueryTable::new(&mut base_db, package_definitions);
        let def_ids = InternTable::new(&mut base_db);

        MyDatabase {
            db: DatabaseImpl::new(),
            package_ids,
            package_source,
            package_definitions,
            def_ids,
        }
    }

    fn create_source_package(&mut self, source: String) -> PackageId {
        let rev = self.next_rev();
        let package = self.package_ids.new_input();
        self.package_source.set(rev, package, source);
        package
    }

    fn update_source(&mut self, package: PackageId, new_source: String) {
        let rev = self.next_rev();
        self.package_source.set(rev, package, new_source);
    }

    fn package_source(&self, package_id: PackageId) -> &String {
        self.package_source.fetch(self, package_id)
    }

    fn package_definitions(&self, package_id: PackageId) -> &Vec<DefId> {
        self.package_definitions.fetch(self, package_id)
    }

    fn def_id(&self, package: PackageId, name: String, namespace: Namespace) -> DefId {
        self.def_ids.intern(DefName {
            package,
            namespace,
            name,
        })
    }
}

// issue: another query uses the result of an aleady computed query
// -> must return the same dependencies

impl Database for MyDatabase {
    fn next_table_index(&mut self) -> usize {
        self.db.next_table_index()
    }

    fn current_rev(&self) -> Revision {
        self.db.current_rev()
    }

    fn next_rev(&mut self) -> Revision {
        let r = self.db.next_rev();
        <_ as TableOps<Self>>::new_revision(&mut self.package_source);
        <_ as TableOps<Self>>::new_revision(&mut self.package_ids);
        <_ as TableOps<Self>>::new_revision(&mut self.package_definitions);
        <_ as TableOps<Self>>::new_revision(&mut self.def_ids);
        r
    }

    // should be automatically generated
    fn maybe_changed_after(&self, dep_index: DbKey, after: Revision) -> bool {
        match dep_index.table_index {
            x if x == <_ as TableOps<Self>>::table_index(&self.package_ids) => {
                self.package_ids.maybe_changed_after(self, dep_index.index, after)
            }
            x if x == <_ as TableOps<Self>>::table_index(&self.package_source) => {
                self.package_source.maybe_changed_after(self, dep_index.index, after)
            }
            x if x == <_ as TableOps<Self>>::table_index(&self.package_definitions) => self
                .package_definitions
                .maybe_changed_after(self, dep_index.index, after),
            x if x == <_ as TableOps<Self>>::table_index(&self.def_ids) => {
                self.def_ids.maybe_changed_after(self, dep_index.index, after)
            }
            _ => self.db.maybe_changed_after(dep_index, after),
        }
    }

    fn runtime(&self) -> &ashley::query::Runtime {
        self.db.runtime()
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
