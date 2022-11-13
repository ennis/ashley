# Examples

GLTF asset, imported as a hierarchy of nodes with geometry elements
Flattened to (transform, draw calls)
draw calls are indices into VB and IBs + per-object data

Global inputs:
- a buffer containing vertex data
- a buffer containing index data
- an array (or stream) of draw items


```

%0 = buffer<VT> vertex_data { variability=constant };
%1 = buffer<i32> index_data { variability=constant };

%2 = struct DrawItem {
    base_vertex, first_index, index_count, first_instance, ...
}

DrawItem draw_item { variability=object };
PerObjectData object_data { variability=object };  


%4 = main {

    // vertex input setup => creates a vertex stream from draw calls, for each attribute 
    %position, %normals, %texcoord = gpu.vertex_fetch %0 %1 < ... vertex attribute shape ... > draw_item
    %transform = draw_item.transform()
     
    
    // .. now do something with pos, norm, texcoord ..
    
    

}

```

# Passes:
- split into pipelines
- extract pipeline config (vertex input, rasterizer, etc.)
- 


# HIR low-level structure
* basic blocks to represent control flow
* inside blocks: "instructions" in SSA form
  * inputs, outputs, location
    * operands: `&[ValueId]` (slice of value ID), allocated in dropless arena
    * adding an operand => copy slice and append
* each instruction can have metadata ("attributes") attached to it
* attributes can be added to an existing instruction, or modified
* compact representation for attributes
* instruction table: `Vec<Instr>`
  * instructions are ordered
  * should be easy to insert instructions in the middle
  * alternative design: dynamically-sized instrs that hold their operands and attributes inline
    * add operand => push a new instruction 
    * instruction table: `Vec<InstrPtr>` (vec of pointers to instruction headers)
    * cannot dynamically add attributes to instructions without rewriting the whole function => meh


Data shape:

```

Attribute = 
    AttributeId
    + attribute data

Arena = 
    DroplessArena,

Block = 
    Vec<Instr>
    SlotMap<ValueId, Value>
        
Instr = 
    InstrDefId,     // ID into the instruction definition table
    &[Operand],
    &[&dyn Attribute],      // Attributes are immutable & may be shared, allocated in an arena
    
Operand = ValueID
```

Alternative shape:
* all instrs & values into global slotmaps
* analysis results are stored in global SecondaryMaps, possibly sparse
    

  
* SSA value table: `SlotMap<ValueId, Value>`

Should the instructions of a block be a `Vec<Instr>`? Yes, don't refer to an instr, refer to the SSA values for it.
 

```
    let instr : &Instruction;
    if let Some(attr : MyAttributeType) = instr.attribute() {
        // do something with attribute
    } 
    
    impl Instruction {
        pub fn attribute<T: Attribute>(&self) -> Option<&Attribute> {}
        pub fn attribute_mut<T: Attribute>(&mut self) -> Option<&mut Attribute> {}
        pub fn add_attribute(&mut self, attr: impl Attribute) {}
    }
```

Issue: can't downcast a non-static attribute.


# Defining an instruction

`gpu.vertex_input`

# Identifying an instruction
-> with a 32-bit ID (set+instr)
Instruction set ID: identifies the dialect. 
There's a map from dialect name + version to dialect ID.
Dialect are registered in a root context object (name -> ID). 


# Types?
`&'hir dyn Type`
-> but then "Type" cannot contain 'hir references, which is annoying
  -> makes complex types annoying to traverse

Q: How to make them unique given the possibility of user-defined types?

Type:
  TypeId + Parameters (array of attributes)
To make unique: hash TypeId and all parameters

```
struct Type<'hir> {
  id: u32,
  params: &'hir [Attribute],
}
```

Current sketch:
- each type has an associated unique "kind" ID
- all "type" types (structs) are bounded by the `'hir` lifetime
- `Type<'hir>` is a pointer to an entry in the arena

Unresolved question: serialization/deserialization
- as soon as references are involved, this will be pretty hard to serialize


# Attributes

A set of base attribute types, with views on them

enum Attribute<'hir> {
  String(&'hir str),
  I32(i32),
  Type(Type<'hir>),
}

`&'hir Attribute` -> points to header with attribute type ID, followed by attribute data, 
which can contain other references

MyAttribute::cast(attr)

fn allocate(arena: )

fn cast(attr: Attribute<'hir>) -> Option<&'hir MyAttribute<'hir>> {
  // basically an ID check + transmute
  // attr MUST be allocated in an arena
}

Same with instructions?
Vec<&'hir Instr> 


# Three options
A. everything is an `Id<Type>`

Need to pass around a `Db` object to access the data, more indirections (syntactically), but manageable.
Better if not extensible.
Extensibility: type instances are stored in their own arena, which must be created by hand, annoyingly.

B. Erased types `Type<'hir>`, downcastable to `&'hir ConcreteType<'hir>`

Interfaces? Implement traits (e.g. `MyInterface`) on `ConcreteType<'hir>`. Register interfaces alongside the type?
 - `dyn MyInterface` has a typeid, call `type.query<dyn MyInterface>()`

Serialization? Possibly more complicated than with IDs, since this is an object graph. Could work around that by assigning a unique ID to each type
and serializing that.

C. Erased types IDs 
Types are represented by `Id<Type<'static>>`, stored in `IdVec<Type(&'hir dyn TypeImpl)>`, downcastable without too much fuss. The `impl TypeImpl` instance is allocated in a dropless arena. 

Extensibility? Just impl `trait TypeImpl`. Everything is `'static` though.


Basically this boils down to: 
do we absolutely want object graphs with direct pointers and thus lifetimes, or are we OK with giving up, making everything `'static`, and working with IDs instead?
OR?
we could use Arcs


## Option A: `Id<Type>`

references to instances: `Id<Type>` (u32 index, possibly u64 with version) - 4 bytes
instance storage: `IdVec<Box<Type>>` or `IdVec<&'hir dyn Type>` + dropless arena, or a slotmap, eventually
instance access: `ty.cast::<ConcreteType>(&hir_db)`
(+) there's already an ID for serialization 
(+/-) drop impls possible if necessary, with boxing (but in this case it might make sense to use arcs directly?)


## Option B: `Type<'hir>`
references to instances: `Type<'hir> = *const u32` (pointer inside HIR arena) - 8 bytes
instance storage: dropless arena 
instance access: `ty.cast::<ConcreteType>()`
(+) clean API for downcasting, no need to pass &db around
(-) no drop impls
(-) instances need a separate ID for serialization

## Option C: `Arc<dyn Type>`
references to instances: `Arc<dyn Type>` (fat pointer) - 16 bytes
instance storage: free Arcs
instance access: `ty.cast::<ConcreteType>()`
(+) clean API for downcasting
(+) no lifetime annotations
(+) possible to have drop impls
(+) easy to require interfaces like Debug (just define `Type: Debug`)
(-) Arc overhead: allocation and pointer size
(-) instances need a separate ID for serialization 

The choice is only meaningful for Attributes, which are the only thing that can contain truly arbitrary data. 
The other concepts can be modeled as an identifier with attributes, with "wrapper" classes that interpret the data:
* types: type class identifier + list of attributes 
  * e.g. VectorType = IDVectorType(0x...) + U8Attrib(element_type) + U8Attrib(rows) + U8Attrib(columns) 
* instructions: instruction opcode + list of operands + list of results + list of attributes


We can also avoid the choice for attributes as well, if we constrain their shapes (like a json::Value type).

Attribute serialization?

## Attribute representation

Examples of attributes:
- ScalarType (data-less enums)
- StructField (record type)
- Struct (list of records)

-> More generally, arbitrary data?

# Extensibility
Do we need extensibility? Removing extensibility would simplify the design a lot, but might miss some opportunities for re-use in other places.
Also, we'd need to move more things into HIR:
* concepts for the execution graph (shader stages, composition graph, types)
