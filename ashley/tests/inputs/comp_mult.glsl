

//-------------------------------------
// module stroke;

// public: visible to other modules, external linkage
// declaration provided elsewhere, possibly in another module
public vec4 stroke_main();

// public: visible to other modules, implies external linkage
public vec4 stroke()
{
    vec4 color = stroke_main();
    return color;
}

// private: visible to this module only, implies internal linkage
vec4 stuff() {

}

// name is visible to this module only, explicit external linkage
// (another module can import it with `extern vec4 stuff1();`)
extern vec4 stuff1() {
    // ...
}

// forward declaration, internal linkage
// currently not allowed
vec4 stuff2();

//-------------------------------------
// module impl;

// how to define stroke_main?

// issue: syntax says that this has internal linkage, but it is not
// solution: `import stroke;` makes the `public vec4 stroke_main()` decl visible, the compiler knows that this has external linkage and that we're providing the definition for it
// another solution: `extern vec4 stroke_main()` tells the compiler that this has external linkage
extern vec4 stroke_main() {
    return vec4(1,0,0,1);
}
