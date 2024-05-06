# Poppy
A proof-of-concept compiler for a type system I am creating as part of my Honours project. For a description of the theory, [check out my thesis here](https://github.com/os12345678/Poppy/blob/main/Honours_Thesis_Oliver_Salman_13881750.pdf)

### Goal
- To create a novel, expressive type system that has higher degrees of concurrency support than Rust

### Aims
- Design an minimally viable object oriented programming language which implements fork-join parallelism.
- Design a static type system, implementing the core subset of the Kappa type system.

### TODO
- Finish linking LLVM backend
- Introduce generics/polymorphism
- Algebraic data types
- Refine language syntax/structure


### What it does
- From the surface, Poppy is like any other Java-esque langugage which supports everything from structs, traits, control flow, functions, and variable assignment ([see here for some language interoperability](https://github.com/os12345678/Poppy/tree/main/examples)). But Poppy differs from traditional languages in two ways, 1) capability annotations for fields and function/method type signatures, and 2) a structured approach to concurrency, see below for a simple example, or [here](https://github.com/os12345678/Poppy/blob/main/examples/poppy/immut_refs_in_multiple_threads.poppy) for a working example.
```
fn concurrentSearch() -> int {
  ...
  finish {
    async { // thread spawn
    ...
    }
    ... // executed here concurrently
  } // joins here
  ... // no longer executing here
}
```


### How to use
Poppy is still in development until I find some more time to work on it. So it is not at production level quality yet, and running a program will output LLVM IR. The next TODO is to finish linking the LLVM backend to execute and optimise the LLVM IR. As a result, running a program with the below template will output the LLVM IR. Though, this can then be compiled using clang (or similar) to create an executable binary.

Poppy is built using OCaml for the compiler frontend, and LLVM for its backend. So having OCaml, opam, and LLVM 14 installed is necessary. 

Once these are installed, the Makefile has all basic commands. 
- `make install` - install dependencies
- `make build` - build the project

To compile a program, run the following command: 
`dune exec bin/main.exe examples/poppy/<.poppy program>`

Mentioned previously, currently, the output is LLVM IR. To compile this to an executable, run the following command:
`clang -o <output file> <output file>.ll`
