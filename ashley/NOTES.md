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
