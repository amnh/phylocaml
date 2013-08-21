BUILD=ocamlbuild -use-ocamlfind

docs :
	$(BUILD) phylocaml.docdir/index.html

clean :
	$(BUILD) -clean

native :
	$(BUILD) phylocaml.cmxa

byte :
	$(BUILD) phylocaml.cma

phyloc :
	$(BUILD) libphyloc.a

all : native

.PHONY: clean byte native phyloc docs all
