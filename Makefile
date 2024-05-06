default:
	opam update
	opam install . --deps-only
	make build

build:
	make pre-build
	dune build

install:
	eval $(opam config env)
	opam update
	opam install --yes . --deps-only
	eval $(opam env)