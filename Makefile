.PHONY: clean byte native phyloc docs top

.DEFAULT: native

BUILD=ocamlbuild -use-ocamlfind

native :
	$(BUILD) phylocaml.cmxa

byte :
	$(BUILD) phylocaml.cma

top :
	$(BUILD) phylocaml.top

phyloc :
	$(BUILD) libphyloc.a

docs :
	$(BUILD) phylocaml.docdir/index.html

%.mli :
	$(BUILD) $*.inferred.mli && cp _build/lib/$*.inferred.mli lib/$*.mli

clean :
	$(BUILD) -clean
