#!/bin/bash


cd bin && dune exec ./main.exe ../test.poppy | awk 'BEGIN {printed=0} {if (printed) print $0; else if (/;/) {split($0,a,";"); print ";"a[2]; printed=1}}' > ../t.ll

cd .. && clang t.ll -o output && ./output