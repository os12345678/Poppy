# Poppy
A proof-of-concept compiler for a type system I am creating as part of my Honours project.

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


### How to use
- Poppy is still in development until I find some more time to work on it. So it is not at production level quality yet, and running a program will output LLVM IR. The next TODO is to finish linking the LLVM backend to execute and optimise the LLVM IR. As a result, running a program with the below template will output the LLVM IR. Though, this can then be compiled using clang (or similar) to create an executable binary.
  
dune exec bin/main.exe examples/poppy/<.poppy program> 





