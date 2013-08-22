
.PHONY: clean byte native phyloc docs

.DEFAULT: native

BUILD=ocamlbuild -use-ocamlfind

native :
	$(BUILD) phylocaml.cmxa

byte :
	$(BUILD) phylocaml.cma

phyloc :
	$(BUILD) libphyloc.a

docs :
	$(BUILD) phylocaml.docdir/index.html

clean :
	$(BUILD) -clean
