.PHONY: clean byte native test tests all

.DEFAULT: native

BUILD=ocamlbuild -use-ocamlfind

all : native

tests : native

test : native

native :
	$(BUILD) test.native

byte :
	$(BUILD) test.byte

clean :
	$(BUILD) -clean
