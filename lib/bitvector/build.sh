#!/usr/bin/env bash

# Basic compilation for bitvector.ml, this is used for testing until we've
# chosen an approprate build system.

rm -f bv8.c bv16.c bv32.c bv64.c *.o *.cmi *.cmo *.so *.a *.cma

sed -e "s/bv_/bv8_/g"  < bv.c > bv8.c
sed -e "s/bv_/bv16_/g" < bv.c > bv16.c
sed -e "s/bv_/bv32_/g" < bv.c > bv32.c
sed -e "s/bv_/bv64_/g" < bv.c > bv64.c
ocamlc.opt -c -ccopt -DWIDTH=8  -ccopt '-O2 -Wall -pedantic -Wextra' bv8.c
ocamlc.opt -c -ccopt -DWIDTH=16 -ccopt '-O2 -Wall -pedantic -Wextra' bv16.c
ocamlc.opt -c -ccopt -DWIDTH=32 -ccopt '-O2 -Wall -pedantic -Wextra' bv32.c
ocamlc.opt -c -ccopt -DWIDTH=64 -ccopt '-O2 -Wall -pedantic -Wextra' bv64.c
ocamlc.opt -c ../phyloc.c
ocamlmklib -o bv_lib bv8.o bv16.o bv32.o bv64.o phyloc.o

ocamlc.opt -c -w @a bitvector.ml
ocamlc.opt -w @a -a -custom -o bitvector.cma bitvector.cmo -dllib dllbv_lib.so
