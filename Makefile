BUILD = ocamlbuild -use-ocamlfind

phylocaml.cma:
	$(BUILD) phylocaml.cma

phylocaml.cmxa:
	$(BUILD) phylocaml.cmxa

libphyloc.a:
	$(BUILD) libphyloc.a

# ----------
clean: 
	$(BUILD) -clean $(CLEANFLAGS)

native: phylocaml.cmxa

byte : phylocaml.cma

phyloc : libphyloc.a

docs :
	$(BUILD) phylocaml.docdir/index.html

.PHONY: phylocaml.cma phylocaml.cmxa libphyloc.a clean byte native phyloc docs
