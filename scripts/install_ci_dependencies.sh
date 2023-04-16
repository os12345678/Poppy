#!/bin/bash

opam switch create poppy ocaml-base-compiler.4.12.0
eval $(opam env)

# Read from the opam_dependencies.txt file
dependencies=$(cat opam_dependencies.txt)

# Install only the available libraries
for dep in $dependencies; do
  if opam info $dep > /dev/null 2>&1; then
    opam install -y $dep
  else
    echo "Warning: Skipping the unavailable dependency $dep"
  fi
done