.PHONY: clean byte native test tests

.DEFAULT: byte

BUILD=ocamlbuild -use-ocamlfind -pp "camlp4of -DUSE_EXTERNAL_LINKING" -tags "package(phylocaml),package(oUnit),debug"
 
tests : native

test : native

native :
	$(BUILD) test.native

byte :
	$(BUILD) test.byte

clean :
	$(BUILD) -clean
