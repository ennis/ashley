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
(+) support for static traits by defining `Type: Trait`
(+) the most compact storage for references
(-) extra indirection if using polymorphic data (deref `&IdVec[index]`, then deref pointer), otherwise it's equivalent

## Option B: `Type<'hir>`
references to instances: `Type<'hir> = *const u32` (pointer inside HIR arena) - 8 bytes
instance storage: dropless arena 
instance access: `ty.cast::<ConcreteType>()`
(+) clean API for downcasting, no need to pass &db around
(+) compact storage for references (compared to fat ptrs)
(-) instances need a separate ID for serialization
(-) no support for trait objects
(-) need a separate table to keep track of all types, for serialization

## Option C: `Arc<dyn Type>`
references to instances: `Arc<dyn Type>` (fat pointer) - 16 bytes
instance storage: free Arcs
instance access: `ty.cast::<ConcreteType>()`
(+) clean API for downcasting
(+) no lifetime annotations
(+) possible to have drop impls
(+) support for trait objects
(-) Arc overhead: allocation and pointer size
(-) instances need a separate ID for serialization
(-) need a separate table to keep track of all types, for serialization

## Option D: `&'hir dyn Type + 'hir`
references to instances: `&'hir dyn Type + 'hir` (fat pointer) - 16 bytes
instance storage: dropless arena
instance access: `ty.cast::<ConcreteType>()`
Like `Arc<dyn Type>` but allocated in an arena.
(+) clean API for downcasting
(+) support for trait objects
(+) Less overhead than Arc
(-) instances need a separate ID for serialization
(-) need a separate table to keep track of all types, for serialization (`IdVec<Type<'hir>>`)

## Example: representing a struct type

Field: &'str name, Type

    StructType
      0: name(8)
      8: field count(8)
      16: field ptr(8)

Total 24 bytes
    
    Field
      0: name(8)
      8: type(16)

Total 24 bytes
      
    

The choice is only meaningful for Attributes, which are the only thing that can contain truly arbitrary data. 
The other concepts can be modeled as an identifier with attributes, with "wrapper" classes that interpret the data:
* types: type class identifier + list of attributes 
  * e.g. VectorType = IDVectorType(0x...) + U8Attrib(element_type) + U8Attrib(rows) + U8Attrib(columns) 
* instructions: instruction opcode + list of operands + list of results + list of attributes

We can also avoid the choice for attributes as well, if we constrain their shapes (like a json::Value type).

Attribute serialization?

## Decision:

A pointer-based approach would be nice, if we didn't need serialization.
With references to objects in arena, need to either:
- keep an `IdVec<Type<'hir>>` of all types in a session
- during serialization, build a map: `Type<'hir> -> ID` by traversing the whole context (in-order) for referenced types 
Feels simpler to just reference with IDs.

Do we need serialization that badly?
-> caching intermediate results to disk

# Extensibility
Do we need extensibility? Removing extensibility would simplify the design a lot, but might miss some opportunities for re-use in other places.
Also, we'd need to move more things into HIR:
* shader stages
* composition graph, types


# Instructions
Q: are they polymorphic? 
A: if they are, then we must add an indirection in the instruction table, i.e:
```
// within block:
Vec<&'hir dyn Instruction>
```
instead of 
```
Vec<Instruction<'hir>>
```

not sure if they need to be: they are all the same form: opcode + operands + attributes

I guess they could? A block would be `Vec<&'hir dyn Instruction>`.

## Instruction-agnostic operations on instructions 
1. Replace a value in the operands (with another value of the same type)
2. Traverse child blocks

Issue: need to modify operands of instructions without knowing their specifics

=> see with usage, go with opcode + operands + attributes (named)



# Extensibility 2

Do we need extensibility? It's possible to abandon extensibility, if we're willing to introduce ImageEffect concepts into the IR. This includes:

* ImageEffect resource types (abstract images)
* ImageEffect image effect definitions (RoI calculation)
* GPU pipeline types: vertex attributes, rasterizer config, image formats, blending, compute local size, etc.

We can separate all that into modules, but there'll be several big enums that tie all concepts together in one type.

Anything else, apart from ImageEffect?
=> anything that targets the GPU in some way (otherwise might as well run rust)

- 3D scene processing?
- debugging/instrumentation/probing attributes, lowered to shaders
- audio?

## ImageEffect in HIR?
Not sure that it's necessary. What kind of operations / transformations / optimizations at the ImageEffect level would benefit from an IR?
* transform propagation? No, transform should be explicitly handled by the code of the operator
  * any optimization will be dependent on the implementation of the operator
* collapse shaders (intermediate storage elimination)? Could be handled at a lower level of the HIR (gfx).
* RoD / Input RoI calculation? Would have to be specified outside the IR.
  * So, would have to write application code in rust if the effect has a non-standard input RoI

## Conclusion
Go extensible (or rather, "open-ended")


```
circle generator (pos) -> antialiased result

scaling: apply transformation on position before
-> position is not really a position, but a pixel footprint in screen space
-> pass around pos and (analytical) position derivatives
 
Derivatives are already available in fragment shaders.
BUT derivatives are not relative to the final screen-space if there's texture resampling
BUT the size in pixels of the rasterized intermediate will have pixels of the same size


anyway, not specific to ImageEffects
```

IR example:

	%blur = imf.image_effect {
		// I/O
		in $radius:f64;
		in $i0:image;
		out $o0:image;

        %roi       = im.roi       %o0
        %i_roi     = im.expand    %roi $radius 
        %input     = im.iroi      $i0          // requests a RoI from the input 

        %gfx_input = im.gfx_image %input       // request a gfx.image for it (
		
        %fragCoord = gfx.dispatch_screen_rect %gfx_input      // dispatch a screen quad rendering operation, returns fragCoord stream rate:frag
        %i0_size   = gfx.image_size %i0 				          // rate:uniform
        %uv        = base.div       %fragCoord %i0_size			          // rate:frag
        %i         = base.var<!i32> #0 		     // rate: frag
        %color = base.var<!vec4>		 // rate: frag
        base.loop {
            %1 = base.load %i
            %2 = base.iadd %1 #1
            %3 = base.store %i
            // ... sample %i0 at uv+i and accumulate into color ...
        }
        %o0 = gfx.out %fragCoord %color  	// fragment output (expects the fragCoord and a value that flows from the same fragcoord)
		
	}

	%main = {
		%blur_radius = 5.0
		%colorcorrection = imf.apply %merge ()
		%blur = imf.apply %blur (%blur_radius, %colorcorrection)
		%root = imf.apply %tonemap (%blur)			// abstract image
	}


This process is done on the fly every time the user selects an image to show. 



First pass: extract code to compute the IRoI / RoD of all effects.
Can do it separately for all effects. It generates functions inside of image effects.

Second pass: 

...


Final result:
- an object, with an interface to set all input parameters
- a function that returns the RoD of the root effect given all inputs
- a function that schedules device & CPU work, and produces a device (or CPU) image as a result (one or more) 
=> does not hold the HIR
=> can be cached, invalidated when the structure of the graph changes
The HIR only lives temporarily.

When the user selects an image / buffer to show, or the structure of the graph has changed:
- a fresh, empty HIR context is generated
- the graph of effects is traversed, the HIR is generated into the context (the HIR of individual effects may be cached and serialized somewhere) 
- the structure of the graph is encoded in the HIR
- lowering passes are applied to get the final evaluation plan

