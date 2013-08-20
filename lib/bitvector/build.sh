#!/usr/bin/env bash

# Basic compilation for bitvector.ml, this is used for testing until we've
# chosen an approprate build system.
rm -f bv8*.[ch] bv16*.[ch] bv32*.[ch] bv64*.[ch] *.o *.cmi *.cmo *.so *.a *.cma

sed -e "s/bv_/bv8_/g"  -e "s/bv\.h/bv8.h/"  < bv.c > bv8.c
sed -e "s/bv_/bv16_/g" -e "s/bv\.h/bv16.h/" < bv.c > bv16.c
sed -e "s/bv_/bv32_/g" -e "s/bv\.h/bv32.h/" < bv.c > bv32.c
sed -e "s/bv_/bv64_/g" -e "s/bv\.h/bv64.h/" < bv.c > bv64.c

sed -e "s/bv_/bv8_/g"  < bv.h > bv8.h
sed -e "s/bv_/bv16_/g" < bv.h > bv16.h
sed -e "s/bv_/bv32_/g" < bv.h > bv32.h
sed -e "s/bv_/bv64_/g" < bv.h > bv64.h

ocamlc.opt -c ../phyloc.c

## NATIVE COMPILATION
ocamlc.opt -g -c -ccopt '-DWIDTH=8  -O2 -Wall -pedantic -Wextra' bv8.c
ocamlc.opt -g -c -ccopt '-DWIDTH=16 -O2 -Wall -pedantic -Wextra' bv16.c
ocamlc.opt -g -c -ccopt '-DWIDTH=32 -O2 -Wall -pedantic -Wextra' bv32.c
ocamlc.opt -g -c -ccopt '-DWIDTH=64 -O2 -Wall -pedantic -Wextra' bv64.c
ocamlmklib -o bv_lib bv8.o bv16.o bv32.o bv64.o phyloc.o

## SSE COMPILATION
#sed -e "s/bv_/bv8_/g"  < bv_sse.c > bv8_sse.c
#sed -e "s/bv_/bv16_/g" < bv_sse.c > bv16_sse.c
#sed -e "s/bv_/bv32_/g" < bv_sse.c > bv32_sse.c
#sed -e "s/bv_/bv64_/g" < bv_sse.c > bv64_sse.c
#sed -e "s/bv_/bv8_/g"  -e "s/bv\.h/bv8.h/"  < bv_sse.h > bv8_sse.h
#sed -e "s/bv_/bv16_/g" -e "s/bv\.h/bv16.h/" < bv_sse.h > bv16_sse.h
#sed -e "s/bv_/bv32_/g" -e "s/bv\.h/bv32.h/" < bv_sse.h > bv32_sse.h
#sed -e "s/bv_/bv64_/g" -e "s/bv\.h/bv64.h/" < bv_sse.h > bv64_sse.h
#ocamlc.opt -c -ccopt '-DWIDTH=8  -DSSE -msse4 -O2 -Wall -pedantic -Wextra' bv8_sse.c  bv8.c
#ocamlc.opt -c -ccopt '-DWIDTH=16 -DSSE -msse4 -O2 -Wall -pedantic -Wextra' bv16_sse.c bv16.c
#ocamlc.opt -c -ccopt '-DWIDTH=32 -DSSE -msse4 -O2 -Wall -pedantic -Wextra' bv32_sse.c bv32.c
#ocamlc.opt -c -ccopt '-DWIDTH=64 -DSSE -msse4 -O2 -Wall -pedantic -Wextra' bv64_sse.c bv64.c
#ocamlmklib -o bv_lib bv8.o bv8_sse.o bv16.o bv16_sse.o bv32.o bv32_sse.o bv64.o bv64_sse.o phyloc.o

# LINKING
ocamlc.opt -g -c -w @a bitvector.mli
ocamlc.opt -g -c -w @a bitvector.ml
ocamlc.opt -g -w @a -a -custom -o bitvector.cma bitvector.cmo -dllib dllbv_lib.so

ocamlc.opt -g bitvector.cma libbv_lib.a test_bv.ml -o test.native

