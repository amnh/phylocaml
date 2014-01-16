.PHONY: clean byte native phyloc docs top install uninstall tests all dot extests

.DEFAULT: all

BUILD=ocamlbuild -use-ocamlfind
OFIND=ocamlfind

INST_BYT=_build/phylocaml.cma _build/dllphyloc.so
INST_NAT=_build/phylocaml.cmxa _build/libphyloc.a _build/phylocaml.a _build/phylocaml.cmx
INST_OTH=_build/lib/*.mli _build/phylocaml.cm[io]

# -----------------------------------

all : phyloc native byte

native :
	$(BUILD) phylocaml.cmxa

byte :
	$(BUILD) phylocaml.cma

top :
	$(BUILD) phylocaml.top

phyloc :
	$(BUILD) libphyloc.a

# -----------------------------------

extests :
	cd test && $(MAKE)

tests :
	$(BUILD) test.native

# -----------------------------------

clean :
	cd test && $(MAKE) clean
	$(BUILD) -clean

# -----------------------------------
 
install :
	$(OFIND) install phylocaml META $(INST_BYT) $(INST_NAT) $(INST_OTH)

uninstall :
	$(OFIND) remove phylocaml

# -----------------------------------

docs :
	$(BUILD) phylocaml.docdir/index.html

man :
	$(BUILD) -docflags "-man -man-mini" phylocaml.docdir/man

%.mli :
	$(BUILD) $*.inferred.mli && cp _build/lib/$*.inferred.mli lib/$*.mli

dot :
	$(BUILD) -docflag -dot phylocaml.docdir/dot && cp _build/phylocaml.docdir/dot phylocaml.dot
