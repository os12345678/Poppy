default:
	opam update
	opam install . --deps-only
	make build

build:
	make pre-build
	dune build

# Generate platform-specific LLVM IR for the C runtime bindings.
# This must run before the main build so bindings.ll matches the current target.
pre-build:
	clang -S -emit-llvm core_lib/bindings.c -o core_lib/bindings.ll

install:
	eval $(opam config env)
	opam update
	opam install --yes . --deps-only
	eval $(opam env)

test:
	dune runtest

clean:
	dune clean

.PHONY: default build pre-build install test clean
