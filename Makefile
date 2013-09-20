.PHONY: clean byte native phyloc docs top install uninstall all

.DEFAULT: all

BUILD=ocamlbuild -use-ocamlfind
OFIND=ocamlfind

INST_BYT=_build/phylocaml.cma _build/dllphyloc.so
INST_NAT=_build/phylocaml.cmxa _build/libphyloc.a
INST_OTH=_build/lib/*.mli _build/lib/*.cm[iox]

all : native byte

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

install : native byte
	$(OFIND) install phylocaml META $(INST_BYT) $(INST_NAT) $(INST_OTH)

uninstall :
	$(OFIND) remove phylocaml
