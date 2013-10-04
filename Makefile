.PHONY: clean byte native phyloc docs top install uninstall tests all dot

.DEFAULT: all

BUILD=ocamlbuild -use-ocamlfind
OFIND=ocamlfind

INST_BYT=_build/phylocaml.cma _build/dllphyloc.so
INST_NAT=_build/phylocaml.cmxa _build/libphyloc.a _build/phylocaml.a
INST_OTH=_build/lib/*.mli _build/lib/*.cm[iox]

# -----------------------------------

all : native byte

tests :
	cd test && $(MAKE)

native :
	$(BUILD) phylocaml.cmxa

byte :
	$(BUILD) phylocaml.cma

top :
	$(BUILD) phylocaml.top

phyloc :
	$(BUILD) libphyloc.a

clean :
	$(BUILD) -clean && cd test/

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
