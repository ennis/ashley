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


# ID-based objects

## SSA value IDs
Q: Should they be unique across the whole program to simplify SPIR-V emission?
A: Not necessarily, this makes allocating ID maps for functions/regions difficult (need hash maps)

In general, should be able to access values defined in a parent region.
=> so, global SSA value IDs

How to efficiently represent maps keyed by value IDs, considering that:
* maps may be sparse (e.g. information calculated only for the values in a block or function)
* the value IDs may grow 


Instructions may have multiple outputs.
Analysis passes often need to go to the instruction that produced the value

    Op {
      outputs: Vec<ValueId>,
      ...
    }
  
    OperationId -> Operation
  
    Value {
      source: OperationId,
    }


Alternative:

    OperationId == ValueId -> Operation
    
    Value { 
        defining operation (pointer?) OR block argument index
    }

    Operation {
        output values (pointers?)
    }


# IR transformations

* replace all uses with ("RAUW")
* region inlining (INLINE)
* value: go to defining operation  (GTDO)
* insert operation before/after another (INS)
* attach values to entities per region (REGION-MAP)
* attach values to all entities in the context (GLOBAL-MAP)


# List of IRs

* [cranelift IR](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md)
  * https://docs.rs/cranelift-codegen/latest/cranelift_codegen/ir/entities/index.html
  * 32-bit local entity IDs (not globally unique)
  * slotmaps, with ID linked-list in secondary slotmap to define order
  
* [SPIRT](https://github.com/EmbarkStudios/spirt)
  * Chunked ID allocators (can have "local maps" for all ID ranges in a function)
  * Element storage in parent element, but ID allocation is global
  * Linked lists with IDs
  
* [MLIR](https://mlir.llvm.org/docs/Tutorials/UnderstandingTheIRStructure/)
  * Pointer linked lists
  * def-use chains

* [MIR](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/index.html)
  * `Vec<Statement>` that refer to `Local`s inside functions by index

* naga
  * Function-local ID vectors



# Region tree representation

* Owned: local slotmap `RegionId -> RegionData`
* Owned: global slotmap `RegionId -> RegionData`, and `Vec<RegionId>` for child regions

# Possible instruction representations

## MLIR-like: pointer linked lists
Value reference: `&'hir Value`
Region: `Vec<&'hir Instruction>`

Extensive interior mutability

-> RAUW: find & replace in successors, or maintained def-use chain (interior mutability)
-> INLINE: OK (?)
-> GTDO: values have a backpointer to the defining op
-> INS: OK (linked-list), but must use interior mutability
-> REGION-MAP: values don't have IDs, so not straightforward; `HashMap<&'hir Value, Data>`, or linked-list of pass results directly on the value
-> GLOBAL-MAP: same as above

## Region-local IDs
Each region has an IdVec of values. Values don't have a global ID, are visible & accessible only in the containing region.

-> RAUW: find & replace in successors
-> INLINE: OK (?)
-> GTDO: each value has an operation ID
-> INS: ID linked-list
-> REGION-MAP: per-region SecondaryMap, should be relatively efficient
-> GLOBAL-MAP: nested SecondaryMaps 

## Global IDs
Operations are allocated in global slotmaps. Values are still only visible in their defining regions.

-> RAUW: find & replace in successors within region
-> INLINE: OK (?)
-> GTDO: same as above
-> INS: ID linked-list
-> REGION-MAP: SparseSecondaryMap
-> GLOBAL-MAP: SecondaryMap


## Decision:

Either global IDs or pointer linked-lists

### Global IDs:
(+) serialization is easy
(+) less lifetimes
(+) no interior mutability required
(-) syntactical indirection to access the data (slightly more noisy)


### Pointer linked-lists
(+) less syntactical noise (although not necessarily more efficient)
(-) possibly less compact (64-bit pointers vs 32-bit indices)
(-) need interior mutability to replace value refs (implications?) - absolutely no mutable borrows possible
(-) mapping always needs a hash map


# Operation definition

Several things:
* opcodes
* mnemonics
* global registration
* builder extension trait
* wrapper type
* matcher
* custom printing & parsing

Wrapper type:
* associate positional arguments 

# Projectional editors?
(for artifice, no relation to the HIR)
https://enso.org/


# Operation types
A wrapper around operations for type-safe access.
* `&OperationData`? can't borrow HIR because it prevents all access
* copy of the results

The workflow will probably go something like this:
* iterate over the ops in a region
* big match on opcodes
* opcode matches => deconstruct operands, attributes with the op type, borrow is released there

# TODO
* IR builder traits (essential)
* terse operation definitions
  * code generation?
* location propagation
* define the base dialect
* unify FileId/SourceId

# Full-fat pointers for attributes may be wasteful
Especially if just storing a boolean constant -> 128 bits

Instead of storing `&[&dyn Attribute]` in instructions, how about just `&dyn InstructionData`? 


# Loss of pattern matching

The loss of pattern matching on attributes & types hits hard

# Issue: ArenaAny cannot work with generics at all 

(except with a static bound)
We need generics inside attributes for our dynamic pattern matching

Alternatives:
* separate arena types and matcher types, e.g. `FunctionTypeAttr<'hir>` vs `FunctionTypeAttrM`
  * Will need that anyway
* no custom Attribute types (only constructions from base types)
  * easier to serialize, in a way
* Arc<Attribute>
  * will need separate matcher types anyway
  * pay overhead of atomic refcounting

Arena allocation VS arcs?
Q: Does it matter?
A: not sure


Short version:
safely downcasting dyn trait objects with a lifetime is too sketchy

Alternatives:
* use Arcs
* make all Attributes `'static` (and thus `: Any`), store Attribute ID instead
  * can then downcast safely with `Any`
  * but what about arrays in attribute types? (e.g. Fields of a StructType)
    * can't do `&'hir [Field]`
    * store inline? this makes the type ?Sized and thus can't use Any
* no generics?
  * that's what is done right now, seems to work OK

Decision: use Arcs
* we get the ability to store objects with drop impls in them
* we lose a lot of complexity related to lifetimes
* we pay the cost of refcounting, but not sure that's a lot
* we lose `Copy`-able attributes


# Validation rules for attributes / attribute constructors

Should be able to construct types / attributes from a list of other attribs


# Operation definition
Why are operations not trait objects? 

## Option A:
Operation is a non-generic struct, specific ops are wrappers over a reference to an op, like `syntax::SyntaxNode`.

## Option B: `&dyn Operation`
Need methods to traverse operands & results.Generic
Traversal and storage more costly (additional indirection)



# Operation patterns

An operation pattern is used:
* to extract relevant elements from a generic operation
* to build a new instance of the operation

It describes:
* the results (their types)
* the operands
* the attributes
* constraints between all of those

e.g.

    let 
      !fty = FunctionType(_, arguments)
    in
      result = "base.func" @body { arguments } : !fty 
     
      

* `fty` is a builder input, which has to match `FunctionType(_,_)`
* `arguments` is bound to the pattern output of `FunctionType`
* `@body` is a declared region at index 0, with arguments given by `arguments`
* `result` is bound to the result of the operation
* additional constraints can be bound in a subsequent `where` clause

For matching this pattern:
* `result` is the first result of the operation
* `body` is the first region
* `arguments` are the arguments of the first region
* `fty` is the result type

https://grosser.science/static/0c315060e8f3d8454de831910fbb6dd6/fehr-2022-irdl.pdf

Another simpler example:

    // Basic arithmetic operations
    let ty : IsArithmeticType;
    let lhs: ValueOfType(ty);
    let rhs: ValueOfType(ty);

    "base.add"(lhs, rhs) -> result:ty;
    "base.sub"(lhs, rhs) -> result:ty;
    "base.mul"(lhs, rhs) -> result:ty;
    "base.div"(lhs, rhs) -> result:ty;    

Builder:

```rust
fn build(lhs: ValueId, rhs: ValueId) {
   let Some(ValueOfType(ty)) = b.match_value(lhs) else { return None };
   let Some(ValueOfType(ty__0)) = b.match_value(rhs) else { return None };
   if ty != ty__0 { return None };
   let Some(IsArithmeticType) = ty else { return None };
    
   let mut op = OperationBuilder::new();
   op.add_operand(lhs);
   op.add_operand(rhs);
   op.add_result_type(ty);   
}
```

Function:

    let arguments;
    let function_type: IsFunctionType(_, arguments);
    "base.func"<function_type> @body(arguments) -> result:function_type;

```rust
fn build(function_type: Attr) {
  let arguments;
  let Some(IsFunctionType(_, arguments__0)) = ty else { return None };
  arguments = arguments__0;
  // ...
}
```

Some variables are builder inputs.
Some variables are local to the pattern.
Some variables are pattern outputs.


# Patterns-only syntax (no builders)

They should just be functions returning optional values
  
    // matches the first & second operands, respectively
    // LHS is a function over an operand

    let root : Operation; // matches any operation
    
    lhs := LHS(root);
    rhs := RHS(root);
    
    IsArithmetic(ty);
    ty := TypeOf(lhs);
    ty := TypeOf(rhs);

    result := SingleResult(root);
    ty := TypeOf(result);

    // function call
    let op : Operation;
    let function_type = FunctionType(Operand(op,0)); 
    let arg_types = Types(OperandRange(op,1..));
    let result_ty = Type(SingleResult(op));

    Callable(function, 

    let (ret_ty, arg_types) = FunctionType(Type(function)); 
    let call_site_arg_types = Types(arguments);

    Types(arguments) == arg_types;
    ret_ty == result_ty;

Definition in rust:

    
  pub fn callable(b: &Builder, value: ValueId) -> Option<()>;
  pub fn function_type(b: &Builder, ty: Type) -> Option<(...)>;
  // same for lhs, rhs

```rust
fn test() {
  |b, root| -> Option<_> {
    let lhs = lhs(b, root)?;
    let rhs = rhs(b, root)?;
    let ty;
    ty = type_of(b, lhs)?;
    let ty__0 = type_of(b, rhs)?;
    if ty != ty__0 {
        b.diag().error("failed equality constraint").emit();
        return None;
    }
    
    let Some(_) = is_arithmetic_type(b, root) else {
        b.diag().error("failed constraint").emit();
        return None;
    };
    
    let result = single_result(b, root)?;
    let ty__0 = type_of(result);
    
    (lhs, rhs)
  }
}
```

=> separate pattern matching part and constraint matching part

    
# Error reporting in constraints

Unlike other error paths, the error path in a constraint must be cheap, because searches with patterns naturally result in a lot of match failures.
-> it shouldn't allocate
-> failure can happen in a nested pattern


# GPU dialect

* `gpu_dispatch_screen_rect(viewport:Rect2D) -> fragCoords:vec2@fragment`: generates fragment coordinates over a 2D rectangle
* `gpu_collect(fragStream...) -> image...`: collects fragment stream outputs into one or more render target images
  * all fragment streams must originate from the same rasterizing operation
    

## General form of GPU programs

It starts with a dispatch operation producing a varying value (a _stream_).
It ends with a _collection_ operation that terminates processing and writes the result into a resource.

Rasterization is an operation on vertex streams that converts a vertex stream into a fragment stream (interpolation).

```glsl
#version 450

layout(location=0) in vec2 a_position;
layout(location=0) out vec2 v_position;

void main() {
  gl_Position = vec4(a_position, 1.0, 1.0);
  v_position = a_position;
}

/////////////////////////

#version 450

layout(set=0,binding=0,std140) uniform Globals {
  vec2 u_resolution;
  vec2 u_scroll_offset;
  float u_zoom;
};

layout(location=0) in vec2 v_position;
layout(location=0) out vec4 out_color;

void main() {
  vec2 px_position = v_position * vec2(1.0, -1.0) * u_resolution * 0.5;
  // #005fa4
  float vignette = clamp(0.7 * length(v_position), 0.0, 1.0);
  out_color = mix(
  vec4(0.0, 0.47, 0.9, 1.0),
  vec4(0.0, 0.1, 0.64, 1.0),
  vignette
  );
  // TODO: properly adapt the grid while zooming in and out.
  float grid_scale = 5.0;
  if (u_zoom < 2.5) {
    grid_scale = 1.0;
  }
  vec2 pos = px_position + u_scroll_offset * u_zoom;
  if (mod(pos.x, 20.0 / grid_scale * u_zoom) <= 1.0 ||
  mod(pos.y, 20.0 / grid_scale * u_zoom) <= 1.0) {
    out_color *= 1.2;
  }
  if (mod(pos.x, 100.0 / grid_scale * u_zoom) <= 2.0 ||
  mod(pos.y, 100.0 / grid_scale * u_zoom) <= 2.0) {
    out_color *= 1.2;
  }
}
```


```
(vertex_buffer: gpu.buffer)

              a_position = gpu.vtxin   <vec2,8>                           // ty, stride
                     p_0 = base.vcons  <vec4>       a_position,1.0,1.0
(frag_coord, v_position) = gpu.raster  <#rsstate0>  p_0                   // vec2
...
               out_color = ...
              prev_color = gpu.texel   img, frag_coord 
                   color = ....
                   image = gpu.wrt     color              // collect operation
```

## Features & optimizations
* Many-channel images (arbitrary, not limited to 3)
* Intermediate image elimination
* Iterative filters (downscaling, pyramids, etc.)

## Design choices
* domain-specific variables live in separate scopes (don't mix unrelated fragment shader invocations together)

### Intermediate image elimination / pass fusion
If an image has only one use, and it is a read, and it is not a sampling operation:

- we have a single fetch op `texelFetch(image, coordinate, lod)`
- check that lod=0, otherwise abort
- check that coordinate is a fragCoord stream (or an offset thereof)
- determine the bounds of fragCoord from the viewport size, if they are not the same, continue
- otherwise: replace the read operation by a reference to the fragment-domain value

Problem:
- pass P1 produces I1 and I2
- pass P2 reads from I1 but samples from I2
- image elimination will replace the I1 read by a reference to the value in pass P1, effectively fusing P2 and P1 together
- but there's still the I2 sample, dependent on the fragCoords from P2


Solution:
* each image value has an associated pass variable

Is it really useful to interleave values from different passes in the same blocks?
-> they can't interact with each other anyway
-> makes fusion easier though



- detect whether each pixel is read only once: if not, abort elision
  - some kind of loop analysis?
  - use high-level constructs in the source code to simplify analysis!
- replace the read operation with a reference to



Validation rules:
* can't mix streams that originate from different GPU sources:
  * e.g. can't mix vertex streams originating from different vertex buffers
  * or fragment streams from different `interp` operations
    * would be useful though...



Things to consider:
- downscaling loops

TODO: separate struct definitions from operation patterns?

# Operation patterns updates
- differentiate patterns from type constraints
  - type constraints applied to a variable actually specify the type of the pattern variable
  - patterns don't change the type of the variable
  - use '@' for patterns and ':' for type constraints
- placeholder syntax '_' is annoying
  - use variable sigils '%' instead to denote a variable name

## Attribute patterns
`name : Pattern`: tries to match `Pattern` against the attribute, and produces a variable `name` of type `Pattern`
`binding @ name : Pattern`: name as above, but also binds the value of the attribute before downcasting (the scrutinee) to `binding`.
`binding @ Pattern`: only binds the value of the attribute

Examples:
- `condition_code : ConditionCode`: matches an attribute value of type `ConditionCode` (implements `AttributeBase`).
- `coefficient : f32`: matches a value of type `f32`, `coefficient` has type `f32`

## Value type patterns
`name : Type`: matches a value with the specified type, `name` is a variable of type `ValueId`

## Value patterns
`name = Pattern(...)`: pattern matching, `name` is a variable of type `ValueId`


# Scaling down?

Kinda lost track of the main application, artifice.
There are a lot of unknowns for this project. 

## Minimum viable product

Scale back to a simple language replacing GLSL and a simple IR, with the following goals:
- easy to merge shader snippets
- supports closures


Q: should we drop the extensible IR?
A: maybe, but then:

Q: what would be the advantages of having a fixed IR?

- Fixed set of types to consider => straight `match` statements
- Probably easier to define new operations => just add an enum variant
- Probably easier to operate on the IR => straight `match` statements
- easier serialization, since there's no need to register implementations at runtime

Without extensibility, the set of all possible types would be a big enum:

```rust 
enum Type {
    //--------------------
    Scalar,
    Vector,
    Array,
    
    //--------------------
    Image,
    SampledImage, 
    Buffer,

    //--------------------
    VertexStream,
    FragmentStream,
    PrimitiveStream,
}

```

=> Drop extensibility for now (branch out). May reintroduce it later if necessary. It will be easier to design it when the use cases become clearer.
And again, extensibility is only useful if:
1. we need IRs for other things
2. we want to reuse the same infrastructure for different IRs

Again, MLIR is there, but it's such a pain to build something with it outside their tree.
-> would need to write 90% of the logic in C++

YAGNI: Don't write a reusable or extensible infrastructure until we have multiple IRs: it will probably be completely useless and need to be redesigned anyway


# Non-extensible IR: storage of operations
- No need for type-erased attributes anymore: drop ArenaAny
- Interners per-type, as needed
- Gap buffer for regions?

Should we switch to Arcs instead of arena allocation?
For types, yes.

Operations in a SlotMap, organized in a linked list

Value ID -> producing operation + containing region

Operation storage:
- linked-list + concrete structs allocated in Arena

Keep a generic representation for Operations?
Generic representations:
- make generic operations easier, like traversing operands and results, traversing use chains, replacing values


# Do we need any special instructions in functions/programs, other than what SPIR-V offers?

We won't do any transformations on instructions within functions, that's for sure (other than maybe replacing accesses).

* Do we need structured select/loops?
  * if we don't do any analysis on them, no
* Get rid of types?
  * use SPIR-V instructions instead
* Maybe interpolation, but that's not really super useful (could be a separate program)
* closures? instead, just 


# Required IR transformations

* fuse programs in the same domain
  * hoist program inputs to function parameters
* generate shader main body
* instrumentation

Keep function/constant IDs

# Idea: do all transformations at the lowering stage
* hoist program inputs to function parameters
* instrumentation
* (except fusion)

=> A superset, and in-memory representation of SPIR-V


# Bolder idea: just use SPIR-V (with extensions)
Use the `Linkage` capability to merge modules together.
Modules can be serialized to the SPIR-V binary format.
Can read and extract interface from SPIR-V directly.

# Sharing functions between modules?
Use a prelude inserted into all modules


# Uniforms
Q: Does this declare a new parameter?

```
uniform normal_map: Texture2D;
```
A: Yes, if instead you want to use a visible uniform, use: `import uniform normal_map: Texture2D;`
to make it visible: `export uniform normal_map: Texture2D;`

Set of values visible in the pipeline:
```
vec2 position;
vec4 color;
uniform texture2D normal_map; // "export uniform" 
```


# Use cases

## Add a new local control on a mesh, with a vertex color

* In the shader, click-drag the "Vertex Color" component onto the pipeline


# Is there a way to get rid of the custom language entirely?

Need to find a shader library, in rust, which outputs a kind of typed AST, 
or directly outputs SPIR-V with linkage attributes, and is lenient with (or simply ignores) Vulkan restrictions on input types

* Naga frontend with relaxed verification rules and "extern" functions.
    * GLSL is still dumb, though

* Base the syntax on WGSL, follow its semantics

# Problem: the IR types are not enough to represent AbstractFloat and AbstractInt

AbstractFloat & AbstractInt: type of numeric literals before they are "materialized" to a concrete type. They have different
implicit conversion rules than the rest of types. Can't represent them in TypeImpl. 

For instance: AbstractFloat (64-bit float value, possibly more) can implicitly convert to f32 (when used as an operand or function argument),
but a value of type `double` (64-bit float value, materialized in the IR) cannot.

Q: Are there "AbstractVecN", "AbstractIVecN" types? 
A: Possibly, not directly constructible in code though
=> This means that `ScalarType` itself is not sufficient: must have a new `AbstractScalarType`, and variants for Vecs, Matrices, etc.

Q: Add `AbstractFloat`, `AbstractInt` to the IR?
A: ???

Q: Should operators operate on `Abstract` types?
A: Yes

## Alternative: no abstract types

Literals are i32 or f32, respectively.
Constant evaluation is done on f32 values.


# Overload resolution

1. Determine all candidates
  For each argument, find an implicit conversion from the 


`hir::Type` is not convenient for overload resolution: if we represent a built-in signature with `[hir::Type]`, all possible
types in the overloads end up in the arena, even if the overload is not selected. The HIR module is polluted with all possible vector and matrix types, all the time.

Q: Maybe we can use `hir::TypeData` to represent signatures instead of `hir::Type`?
A: This could work, except that we may have struct and image types in signatures, and thus requires allocation.
  For example, when checking image sampling ops, it will allocate a dozen times to build TypeData for each `imageXXX` or `textureXXX` in the overloads. 

Q: In this case, have our own `AbstractType` that we use during lowering, with the built-in image types?
A: Now we have to store user function signatures as `AbstractType` 

1. Store images directly in `hir::TypeData`, no need for allocations
2. Somehow, have a `hir::TypeData` that is not allocated? With `Cow` maybe?

As a rule of thumb, don't intern a type to `hir::Type` if not going to use it in an instruction.

Contrary to SPIR-V, we have different types of IDs for different objects: `Constant`, `Value`, `Function`, `Global`.
SPIR-V has only one ID type (`<id>` in the docs). Some ops can take `<id>` referring to different kinds of objects, as long as they have the expected type.

Q: Should we require all constant & global references in functions to go through special instructions so that we only operate on `Value`s at the basic level?
A: As much as possible, avoid polluting the result SPIR-V too much; if we can avoid the indirection, avoid it


# HIR builder refactor (or alternate API)

- allow void-typed values for easier codegen
- take and return TypedValues instead of IdRefs
- Function calls take `Function`s instead of IdRefs
- infer result types (function calls, arithmetic ops)
  - no implicit conversions though
- easy-to-use, low syntactical overhead
  - EDSL-like API?

Instead of another API, maybe add functions with result type inference?
How to generate that?

```
let v = b.clamp(v1,v2);
```


# TODOs
- hir/function: local variables in FunctionData 
- lower: 
- lower: let-binding type inference
- diag: fix `impl WriteColor + 'static` ugliness
- lower: block termination
- lower/typecheck: implicit conversions in the builtin table (constructor forms)
- hir: remove `Cow` in TypeData, as it's pretty much unusable as hashmap keys
- syntax: automatic whitespace consumption
- syntax: nth(2) lookahead
- syntax: method call syntax 
- lower: for places, figure out whether to prefer the deref type or the pointer type  

# Automatic whitespace in the syntax
- eat whitespace before `start_node`, `checkpoint`, `expect`
- `current()` never returns whitespace

# Function places
There must be a separate lower_expr function for function calls, since in our IR functions are not first-class values.

# Refactor: typed AST?
Instead of going directly from untyped AST to HIR, first create a typed, resolved, immutable AST:
- has source locations
- implicit conversions are made explicit
- overloads are resolved
- values can be void
- no SSA ids, expression tree instead (generic operations)
- types are not converted to IDs
- pointers to the syntax tree

Benefits: more readable syntax when checking lowering rules. 


# GLSL instead?
Rewrite the parser so that it's GLSL-like instead.
Things to change:
- let bindings
- functions
- globals
- need lookahead
- type symbol table

Do not implement:
- multiple declarations
- array declarators

No change:
- as much as possible, keep the AST structure unchanged

Why? 
- people already familiar with GLSL, plus we're already using GLSL semantics
- bigger database of examples


Rowan doesn't include a source ID in TextRange, and it's extremely annoying. 

# Issue: the parser needs the imports in order to build a symbol table for types
fcking useless

Need to query the module provider for a list of symbols
Issue: if the import module imports stuff from the module being parsed -> infinite loop

# Should builtin functions be definitions?
Pros:
* somewhat more uniform code to type-check builtins and regular functions

Cons:
* pollutes modules with hundreds of functions

Alternative:
* register builtin functions in the name resolution

# Should operators be definitions?
No

# Packages

# Query system

There should be set of functions in `Session` to query:
- the AST of some source code (this creates a new package)
- the definitions exposed by a package
- the type-checked body of a definition (identified by DefId)

Problem: these return references, but they can't lock the session object since it may be used recursively.

Q: should there be caching in `Session`?
A: maybe, after all the goal is to use this compiler in an interactive context

This is related to the issue of incremental recomputation for the GUI

Too many unknowns: a query-based system is needed, but just go pass-based for now, so that we can continue building the rest of the compiler.
For now, consider `Session` to be a package resolution cache. Don't cache `TypedBody`s, or `hir::Module`s in it.

# Session = cache

# Next steps
- specify uniform set, binding numbers in generated code
  - Q: session API for that? A: implement in HIR first
    - `sess.shader_interface(pkg) -> ShaderInterface`
  - do it after HIR generation
- specify execution context

```
struct ShaderInterface {
  inputs: Vec<hir::GlobalVariable>,
  outputs: Vec<hir::GlobalVariable>,
  uniforms: Vec<hir::GlobalVariable>,
}

impl ShaderInterface {
}

// Put specified uniforms in a buffer and replace accesses to them.
hir::transform::bufferize_uniforms(...);

// Assign a binding location to the specified uniform var.
hir::transform::assign_binding_locations(...);

```


## Linking

Provide a way to group uniforms into buffers.

```rust
const SCENE_UNIFORMS: &[&str] = &[
  "u_camera",
  "u_resolution"
];

fn test() {
  let mut sm = ShaderInterfaceMap::builder(&hir);
  let camera_data_type = hir.ty_struct_by_name("CameraData");
  
  // provide "u_camera" into a UBO at set 0, binding 0
  sm.provide_uniform("u_camera", Some(camera_data_type), 0, 0);
  
  let mut ubo_desc = todo!(); 
  ubo_desc.field("...", ty);
  ubo_desc.field("...", "");
  ubo_desc.indirect_index("...", );
  
  sm.provide_texture_from_descriptor_array("...", 0, 0);
  
  
  // FIXME: could probably provide the whole uniform buffer at once
  
  
  
  //
  let custom_uniforms = sm.unmapped_uniforms();
  for u in custom_uniforms {
    // decide what to do with them
  }
  
  // apply remapping
  let bsm = sm.build();
  transform::remap(&mut hir, bsm);
  
  
  sm.textures_by_name(&[""]);
  
  // FIXME: with the uniform locations not specified in shader, we now must 
  // specify interfaces at 3 locations:
  // 1. in the shader
  // 2. in the remapping code
  // 3. in the struct
  
}
```


```
struct Material {
  ptr<Material> next;
  vec4 color;
}

struct Data {
   // How to specify that this is a pointer?
   Material material;
   
   
   // 1: buffer Material mat;
   // 2: Material* mat;
   // 3: [buffer_reference] Material mat;
   // 4: buffer_reference Material mat;
   // 5: buffer_reference<Material> mat;
} 

// Alternative, specified on the type:
[[buffer_reference(align=16)]] struct Material { ... };
buffer_reference struct Material { ... };

// Issue: must create a specific type, can't just declare `buffer float[] data;`
```

Avoid pointer syntax, because then we'd need to use `->` for consistency with C.
For now, go with `ptr<type>`.


```
textures descriptor_array (set = 0, binding = 0) {
   // match all textures and put them in a descriptor array 
   // in turn, this produces more values (texture indices) that have to be bound somewhere
   *
}

camera_info uniform_buffer (set=1, binding=0) {
   // match one uniform variable named "u_camera_info", of type "CameraInfo"
   CameraInfo u_camera_info;  
}

object_info indexed_buffer (set=1, binding=1) {
   // match one uniform variable named "u_camera_info", of type "CameraInfo"
   CameraInfo u_camera_info;  
}

push_constants {
   textures
}
```

Two situations with shaders:
- either the application expects a fixed interface, in which case it's simply a matter of remapping the uniforms to buffers & push constants
- it's a user-provided shader, and it has custom uniforms, in which case the app should reflect them and expose them in some kind of UI (or figure out how to provide the data for them)

Two steps:
1. query groups of interface items (e.g. all textures, etc.)
2. set the binding strategy for the group:
  - descriptor array: for lists of textures, specify set, binding and an entry in the push constants for the index
  - storage buffer indirect: specify a storage buffer and an entry in the push constants for the index
  - push constants: pass them in push constants

Or:
1. apply static shader interfaces
2. decide what to do with the remaining items
  - reflect them 
  - group them
  - set the binding strategy for the group


```
#[derive(ShaderInterface)]
#[push_constants]
struct PushConstants {
  #[texture_array_index("textures")]
  diffuse: u32,
}


```


Q: should `utils::StructuredBufferData` return `hir::Type` or `tast::Type`? Or something else?

`tast::Type` is specific to the AST (contains DefIds). So `hir::Type` makes the most sense.
What do we need to describe? The type of the fields of structs that are passed to the shader, and their offsets and sizes.

Don't like the fact that we need to derive two traits: `utils::HirType` and `utils::StructuredBufferData`.

TODO:
* add offsets to `hir::types::Field`s
* add a pass to generate offsets for struct types in HIR

Suggestion: one of the goals is to abstract the strategy used to pass uniforms (or, more generally, data) to the shaders.
Instead of rewriting code, it's better to use functions (externally defined) in the code to access data, and then link the concrete implementation later.


Issue: Vulkan expects a `Block` annotation on structure types that are used as memory interfaces. 
But in our case, there are no "uniform blocks" with ad-hoc structures to represent memory interfaces. 
I.e. a uniform buffer may be declared like this:

    uniform StructureType uniforms;

Vulkan requires that `StructureType` is annotated with the `Block` decoration, but `StructureType` can be used in other ways,
and is not *specifically* an interface type.

# Interpolation qualifiers

Q: Can those be put on struct fields? Does the struct type become "special" if this is the case?

    // this is a "stage interface" struct
    struct Inputs {
        noperspective vec3 pos;
        centroid vec3 color;
    };

Note: in GLSL, structs cannot be used as I/O variables. However, they can in WGSL and HLSL.
Some languages put them on entry point arguments:

    vec4 main(flat in vec3 color, noperspective in vec2 uv, uniform float4x4 transform);


slang: I think they allow both entry-point and global uniforms, but not sure about stage interface structs, or interpolation qualifiers in structs
MSL: everything passed as entry-point arguments. No explicit interface structs
HLSL: interpolation qualifiers on both fields and entry-point arguments
GLSL: interpolation qualifiers on interface blocks & individual in/out globals; no entry-point arguments
WGSL: uniforms as global args, input as entry-point arguments and interface structs

Inputs/outputs should be function parameters, so that multiple entry points can coexist. Also see https://github.com/gpuweb/gpuweb/issues/1155#issuecomment-714801969

WGSL: user defined IO types can be used outside of shader interfaces. In this case, interpolation qualifiers and `builtin` annotations are ignored (https://github.com/gpuweb/gpuweb/issues/1526).

Alternatives: return interface struct with annotations

    struct VertexOutputs {
        @position vec4 position;
        @interpolate(noperspective) vec2 uv;
    }

    VertexOutput vertexMain(in vec3 position, uniform mat4 transform) {
        // ...
        return VertexOutput(...);
    }

No interface structs, only params:

    void vertexMain(in vec3 position, 
                    uniform in mat4 transform, 
                    out builtin(position) vec4 position, 
                    out noperspective vec2 uv)
    {
      // ...
    }

Structs are nice for the interface between different stages, because we don't have to modify multiple entry point signatures
if we want to add a new variable propagated between stages.

Do the same as WGSL here: ignore interpolation and builtin qualifiers when the struct is not used in a IO context.


Issue: struct used in different uniform blocks:

    struct @layout(std140) Stroke {
        vec2 a;
        vec2[10] stride(16) b;
        float taper;
    };
    
    layout(set=0,binding=0,std140) uniform Test {
        Stroke s1;
        Stroke s2;
    };
    
    layout(set=0,binding=1,std430) buffer Test2 {
        Stroke s1;
        Stroke s2;
    };

There are two versions of `Stroke` in the emitted SPIR-V: one with std140 layout, another with std430 layout.


## Attribute syntax

Before the item:

    @position vec4 pos;

Problem with that syntax is that it's ambiguous if we want to put attributes on the type, e.g.:

    @location(0) @stride(16) vec4[10] data;   // is "@stride(16)" referring to the type `vec4`, the type `vec4[10]`, or the field `data`?

Solution: put attribute after the item:

    vec4[10] stride(16) data @location(0);

There might be an ambiguity with function types:
    
    void(vec4 @location(0));  // is the attribute on the type or the parameter?

Alternatives: 
* no attributes on types: the main use case for attributes on types is the `stride` decoration to disambiguate between different array types. If attributes on types are not allowed, then would need a special syntax for the stride.
=> Attributes on type *declarations* are OK actually, the issue is attributes on *type references*. The `@stride` attribute is more like a qualifier (like `const` or `volatile`) than an attribute.
* special types (aligned to 16-byte boundaries) for std140 buffers:
  * `aligned_vec2`, `aligned_float` or `std140_vec2`, `std140_float`
  * this changes the element type instead of the array type
  * the required changes are minimal on the syntax, but need to add implicit conversions


Issue: attributes on functions
    
    void vertexMain(in vec3 position,
                    uniform in mat4 transform,
                    out builtin(position) vec4 position,
                    out noperspective vec2 uv) @vertex
    {
        // ...
    } 

# Attributes on fields / functions, etc.

One type per attribute, e.g.:

    pub struct GuiWidgetAttribute {
    }
    
    sess.register_custom_attribute::<GuiWidgetAttribute>("gui");

Query an attribute on a package:

    sess.has_attribute::<GuiWidgetAttribute>(def: DefId);



# TODOs

1. Determine shader interface
   Where to do this? 
    - Post-HIR (when emitting SPIR-V)
    - Pre-HIR (when lowering TAST to HIR)
   Would make sense to do this post-HIR, so that we can do interface transformations on the HIR without worrying about the interface?
Makes sense to do this on HIR.
 
2. Refactor attribute handling
   * Define attributes by specifying their ident and syntax 
   * Attributes are put in an enum type, with a filter to constrain where they may appear (structures, members, functions, global variables, arguments)
3. qualifiers on global variables
4. Test and review layout calculations
5. Back-links from TAST to original syntax
6. Would be nice to have a syntax highlighter and some IDE integration with LSP

# Should function parameters be pointers in the HIR?

glslang -> SPIR-V does that, since in GLSL parameters are mutable, so they are like variables.
However, this means that at call sites we must materialize arguments into variables => avoid that


# Moving the query system in a separate crate

ashley-db?
ashley-query?
ide-db?

# Query system extensibility

1. Define a struct type containing the tables.
2. If extending an existing database, include the `Tables` of the underlying db in the struct:
```
struct MyDatabaseTables {
   base: BaseTables,
   query_1: DerivedTable<...>
}
```
3. Implement `DatabaseOps` for the new type, deferring to the base tables when necessary.
4. Wrap the tables in `Database<Tables>`
5. Define the queries in an extension trait on `Database<Tables>`

Issue: when extending an existing DB, how to access the queries of the underlying DB?
E.g. we have a `Database<Tables>, impl TablesTrait`, extending a `BaseTables`, but `Database<Tables>` doesn't impl `BaseTablesTrait`

Need a way to go from `Database<Tables>` to `Database<BaseTables>`, ideally without too much noise.

# Query system: errors

Option 1: return dummy/empty results so that dependents can still proceed
Option 2: propagate errors (store `Result<T,QueryError>`)

    /// Returns the `DefId` corresponding to the specified name and namespace.
    fn def_id(&self, package: PackageId, name: String, namespace: Namespace) -> DefId;

    // OK, but should return "Option"
    fn package_source(&self, package_id: PackageId) -> Result<SourceId, QueryError>;

    // Source code, should not fail. 
    fn source_file(&self, id: SourceId) -> &SourceFile;

    // All definitions in a package. Should not fail. If package cannot be parsed, return empty vector, or best-effort result.
    fn package_definitions(&self, package_id: PackageId) -> Result<Vec<DefId>, QueryError>;

    // Returns the package of the specified definition. Cannot fail.
    fn package(&self, def_id: DefId) -> PackageId;

    // Returns the transitive list of dependencies of the specified package. 
    // If package cannot be parsed, return empty vector, or best-effort result.
    fn dependencies(&self, id: PackageId) -> Result<Vec<PackageId>, QueryError>;

    // Returns the packages directly imported by the specified package.
    // If package cannot be parsed, return empty vector, or best-effort result.
    fn imports(&self, id: PackageId) -> Result<Vec<PackageId>, QueryError>;

    // Returns the specified definition.
    // Cannot fail.
    fn definition(&self, id: DefId) -> &Def;

    /// Retrieves the syntax tree of a source file.
    fn syntax_tree(&self, id: SourceId) -> Result<SourceFileSyntaxTree, QueryError>;

    /// Returns the type-checked module for the given package.
    ///
    /// May trigger type-checking and resolution of definitions (but not bodies) if necessary.
    /// The module may have errors.
    fn typechecked_module(&self, package: PackageId) -> Result<&tast::Module, QueryError>;

    /// Returns the type-checked body of the specified definition.
    fn typechecked_def_body(&self, def: DefId) -> Result<&tast::TypedBody, QueryError>;

    fn resolve_package(&mut self, name: &PackageName) -> Result<PackageId, PackageResolutionError>;


# Next steps

### Queries that "set" the value in another table.
E.g. `typechecked_module` also sets the definitions in the module (`QueryTable<DefId->Definition>`).
It's more efficient this way because when updating the definition, we can check whether it has changed, and
skip updating the revision if it hasn't. Whereas right now all definitions are considered dirty whenever the module must be typechecked again.

### Clearer syntax for defining the CompilerDbTables

### Clean API for mutations / inputs

Restrict mutable access to tables.
Idea: HasTables::tables_mut() increases the rev counter (it has access to the runtime, so use that).

### Module system

Q: Do we have to "declare" modules? (like `mod submodule;` in rust)
Side-question: why do we have `mod submodule;` in rust? 

A: if you do `import module;` how do we know where to find the module? If the thing is file-based, then how do we know whether to look in the current directory, or in another search path?

Q: do we import modules or individual items within modules?
A: individual items

Q: How do we make items private to the module?
A: Several options. Naming convention (e.g. name begins with an underscore), visibility modifier (`public` or `private`), or everything visible. For now, start with having everything visible, except imported symbols.
 

Strategies:
* java-like: each file defines a module (or "package" in java). At the top of each file, a declaration specifies the full path of the module (e.g. root.module.submodule). All source files must be scanned to discover modules and build the module hierarchy. The module hierarchy may not follow the directory structure on disk.
* rust-like: modules are declared in the source file of the parent module. Declaring them means that a corresponding source file must be found at a well-known location on disk (typically, a source file with the module name in the same directory, or a `mod.rs` file in a subdirectory with the module name)
* implicit mapping to file system: modules are not declared in code; the module hierarchy reflects the structure on disk. Imports are resolved to file paths. 
  * Q: If some file is never imported, does that mean that it shouldn't be compiled? Because otherwise (all files should be compiled), then we must scan the directory structure to discover files to compile. A: They *must* be compiled even if they are not imported, since they might export symbols.

Tentative module system:
* one file = one module (at least for source packages)
* modules are identified by URIs, with optional schemes
  * custom schemes can be used to load application-provided modules
  * file URIs are "canonicalized" somehow (so that the same module isn't imported twice)
    * need to know whether two paths are equivalent
    * otherwise you get things like https://stackoverflow.com/questions/47121411/flutter-retrieving-top-level-state-from-child-returns-null/47142052#47142052
    * flexible
* in "package compilation mode", all source files in the "package directory" are compiled (recursively). 
* in "single module mode", only referenced modules are compiled
* schemes
  * default to "file"
  * "package": path in current package. If compiling in "single module mode", will try to find the package root dir.
  * any other custom scheme: as defined by the application
    * may include http
    * example: `texture:path/to/image.png` to use a texture file inside 


Refactoring:
* Cache URI resolution / canonicalization
* Module entry has canonicalized URI, source code

# Collect diagnostics for definitions, modules 

* Add `InFile<AstNode>`, like r-a
* Do diagnostics need to be an enum? Or can we use ad-hoc definitions?
  * allow ad-hoc definitions?
* Actually diags should be an enum, because they should be `PartialEq` for the query engine, and we'd need string comparisons for ad-hoc diagnostics. That's what r-a does anyway.
  * rustc has `PartialEq` for ad-hoc diagnostics (https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_errors/diagnostic.rs.html#1041-1061), compares messages, suggestions, etc.
  * Do diags really need to be partialeq?

FIXME: I don't really like having enums for all diags, it feels like useless busywork.

Also, more importantly, the diags contain pointers to the AST, which means that they will compare different on every source code change => the diags are not stable!
Solution: diags should only have stable pointers, and should be resolved late.
Example of stable pointers: SourceFileId, 

The whole TypedBody is not stable anyway (because it contains the expr map).


## List of diags
* "condition must be a boolean expression"

# Split name resolution to a separate stage?

Right now name resolution & type checking (and inference) is done when lowering AST to TAST (which is like rust's THIR).
Not sure what we gain by doing it later.

What we could do though, is defer/split layout calculation away.

Currently, compilation is done in the following stages:
* syntax (parsing) -> produces an AST
* TAST items -> lowers AST definitions to TAST items; 
   Internally, it resolves definition types, computes type layout, and may also lower bodies of constant expressions appearing in types 
* TAST bodies -> lowers AST bodies (statements & exprs) to TAST bodies (also resolves types)

## New stages:

Naively:
* syntax
* definitions
* bodies
* typecheck 
* layout check

But there's a problem: name resolution depends on typeck (to resolve struct fields).

Alternative:
* syntax -> AST
* definitions -> Item tree?
* non-dependent name resolution (variable names, types)
* typecheck / dependent name resolution (fields)
* layout check

Note that r-a has a HIR tree with unresolved names.

TODO:
* keep typecheck/nameres/consteval in the same stage

* There should be "Bodies" for (const) expressions appearing within types

// Issue: parsing a custom attribute is involved
// -> an attribute checker may want to evaluate a constant expression
// -> attribute resolution (e.g. location(expr)) currently happens at the definition lowering stage
// -> resolving the location expr needs name resolution, which in turn will invoke definition lowering -> infinite loop
//
// Solution:
// - delay attribute value resolution: possible, but the same issue can happen with constant-dependent types (e.g. arrays with explicit strides)
// - resolve names in a separate pass, before def check / type check
//

# Issue: parsing def-dependent types of declarations

Assume that we're parsing the declarations of a module. 
We encounter a global variable decl, with type `float[16] stride(N*4)`, with `N` another global variable.
We can't resolve this type now, because we'd need to resolve `N`, but we're still parsing the declarations.


New model:
- In a first pass, the AST is lowered to `Items`, which are defs but without their types.
- This is computed for each module. I.e. each module has an "ItemMap", which can be used to resolve a name to an `Item`. The item data is simply the kind of item (struct, function, global, etc.) ~~and a pointer to the AST~~ and data extracted from the AST, but not resolved.
  - Issue: the pointer to the AST is not stable across changes
  - Solution: instead of storing direct pointers to the AST, store a stable ID derived from either:
    - the name of the item
    - its position in the file
  - each source file then has an associated "AST ID to Node" map.

The item tree can just be a map from "Name" to AstId, and a separate map for getting the AstNodes from AstIds.

How does this help with parsing def-dependent types?
Now we can type-check individual items in any order.

## What about expressions in attributes and inside types?
They will be represented by stable AST pointers to expressions.


Queries:

```
ItemTree {
  types: Map<Name,AstNodeId>,
  values: Map<Name,AstNodeId>,
  
  ast_map: Map<AstNodeId, AstNodePtr<>>,
}
```

```


BodyOwnerId: AttrParam(AttributeId,index) | 

intern def_body(parent: BodyOwnerId, index: usize) -> BodyId       // ID of a body owned by a definition (through attributes, fields, or types inside the definition, except inside another body). 


query body(body_owner: BodyId) -> Arc<Body>      // fully type-checked body

query item_tree(module: ModuleId) -> Arc<ItemTree>    // item tree with 
query definition(def: DefId) -> Arc<Definition>  // full definition information with resolved type
query expr_ty(expr: )
query evaluate_constant_expression(expr: AstExprId)   // Option<ConstantValue>


struct Uniforms {
  @offset(N) vec4 a;
  @offset(N+32) vec3[16] stride(16) b;
}

// attribute on field: AttributeId (AttributeOwnerId + index)
// AttributeOwnerId: StructId | MemberId | GlobalId | FunctionId | FunctionArgument(FunctionId, u32) | FunctionReturnValue(FunctionId)
// BodyOwnerId: Attribute

```


## A principled approach for a query system usable with GUIs?

Inspired by the query system of rustc, rust-analyzer, salsa.

Recall the requirements:
- no serialization code by hand except for tricky cases
- serialize to whatever
- ordered collections, works well with UI
- undo/redo
- objects cheap to copy
- able to extract a diff between revisions
- cascading deletes

TODO:

* efficient storage: IndexVecs, not HashMaps
  * all keys must be Idx
* more efficient tracking of dependency lists: smallvec?

```

// "input" => input data, not tracked separately

table SourceFile (SourceFileId) -> {
  // fields
}




// "query struct" => calculated together, but tracked separately
query ModuleData(ModuleId) -> {
    source_file: SourceFileId,    // implicit dependency on SourceFile table?
    items: ModuleItems,
    map: ModuleAstIdMap,
} 
[item::module_data_query] 

// Maybe we can specify explicit dependencies on individual queries?
// So that we don't need to track stuff.

```

## Minimal invalidation on changes

```

struct S {
  float scale;
  float[16] data;
}

float dot(vec2 a, vec2 b) {
  return a.x * b.x + a.y * b.y;
}

void main() {
  // ...
}
```


Goals:
1. changing anything inside a function body should not invalidate either the item tree or other bodies.
2. changing the "16" const expr inside S should not invalidate bodies that don't depend on it.
3. 4.changing any field inside S should not invalidate bodies that don't depend on it.


    // now lower_ty depends on the def map, which is invalidated on every change, even trivia:
    // types are recomputed on every keystroke!
    //
    // Conclusion:
    // -> lower_ty cannot depend on the AST directly
    // -> lower_ty must only depend on module_items data
    // -> module_items must contain types
    //      -> issue: adding a field to a struct will invalidate module_items, which in turn will dirty all tys, even unrelated
    //      -> a projection query is necessary: extract one definition (struct, function, global) from module_items
    // Actually, lower_ty can directly get the &def::Type, it doesn't need to depend on module_items
    //
    // Is it a problem if types are invalidated on every trivial change?
    //
    // After all, it's not certain that type inference is actually more costly than building AST maps and intermediate representations.
    // It might be possible to put the "firewall" at a higher level: i.e. compare the typed IR instead of
    // the unresolved HIR.
    //
    // The question is: are name resolution and type checking so costly that we want to avoid doing it on every keystroke?
    // => probably yes?
    //
    // Also: how do we represent a def::Type in diagnostics? We don't know it's location since there's no AstId associated to it.
    // We could use an ID enum to represent e.g. Function parameter types, field, types, etc. but in general the ID is going to have
    // a tree structure (path inside the item tree).
    //
    // r-a doesn't emit any diagnostic during lowering of types? why? is it even implemented? (TyLoweringContext has no "diagnostics" field)
    // => it doesn't emit any diagnostic (t
    //
    // Alternatives:
    // * store AstId<ast::Type> alongside def::Types; the Ids are relative to the definition so they are going to be relatively stable.
    //      * basically, put an AstId alongside anything that can fail during name resolution / const evaluation
    // * allocate def::Types in an arena, in Struct/Function/Global, store mapping to AstPtr<ast::Type> in DefMap (roughly equivalent to the first solution)
    // * don't emit diagnostics during type lowering: after all, the calling code knows best the context in which the type appears



## Proposal: lower and type-check bodies (structs, etc.) at the same time (but still on-demand)

Concretely:
- There's no more def::StructData, def::FunctionData, and def::Body
- the "module items" pass only builds data for name resolution
  - doesn't build `def::Type`s or `StructFields`

This means that typechecking and name resolution will be done on every keystroke. Can we do that?

Is there a way to avoid that while still going directly from AST to type-checked body?
We'd need some kind of "relative" AST pointers, which we could compare for equivalence.

## Alternate proposal for simplification:

Lower bodies eagerly, on every reparse? (in `Arc<Body>`) That's what's done already anyway.
Then, use projection queries to avoid unnecessarily rebuilding the types.

## Diagnostic-producing queries

- function_signature
- struct_field_ty
- global_ty

## Refactor builtins

Put all signatures of builtins into a "std" module.

```
import 'std:core' // implicitly added

// ...

```

Contents of `std:core` (a real file somewhere on disk)
```
// module std:core

@spirv_intrinsic("ExtInst GLSL450 Dot $0, $1") extern vec2 dot(vec2 a, vec2 b);
@spirv_intrinsic("ExtInst GLSL450 Dot $0, $1") extern vec3 dot(vec3 a, vec3 b);
@spirv_intrinsic("ExtInst GLSL450 Dot $0, $1") extern vec4 dot(vec4 a, vec4 b);



```

Generated with a script.

Builtins will be treated the same way as other functions. No generics for now.

### SPIR-V intrinsic mini-language

```
@spirv_intrinsic("ImageSampleImplicitLod(SampledImage($0,$1),$2)")
extern vec4 textureSample(texture2D, sampler, vec2);
@spirv_intrinsic("ImageSampleImplicitLod(SampledImage($0,$1),$2)")
extern ivec4 textureSample(itexture2D, sampler, vec2);
@spirv_intrinsic("ImageSampleImplicitLod(SampledImage($0,$1),$2)")
extern uvec4 textureSample(utexture2D, sampler, vec2);

@spirv_intrinsic("FMod($0, ComponentConstruct($1,$1))")
extern vec2 modf(vec2, float);
@spirv_intrinsic("FMod($0, ComponentConstruct($1,$1,$1))")
extern vec3 modf(vec3, float);
@spirv_intrinsic("FMod($0, ComponentConstruct($1,$1,$1,$1))")
extern vec4 modf(vec4, float);
```

## Streamlined module items

* Goal: one AstMap for the module index, then one AstMap per definition
* Module index: list of visible definitions in a module. Sufficient for name resolution.
* Don't need to parse struct bodies or global initializers for the module index
* Issue: functions:
  * Modifying the function arguments shouldn't affect other definitions => function arguments are stored in a different AstMap than the body.
  * However, we may not want to parse the function body at the same time as the arguments => function body has a separate AstMap