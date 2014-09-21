.PHONY: clean distclean all byte native phyloc app bench test top install uninstall dot coverage
.DEFAULT: all

BUILD=ocamlbuild -use-ocamlfind -classic-display
OFIND=ocamlfind
DOCDIR=phylocaml.docdir/

# files that, if exist, define a compilation path happened
BYTE_SMOKE=_build/phylocaml.cma
NATIVE_SMOKE=_build/phylocaml.cmxa

# associate files to different compilation paths
INST_BYT=_build/phylocaml.cma _build/dllphyloc.so
INST_NAT=_build/phylocaml.cmxa _build/libphyloc.a _build/phylocaml.a _build/phylocaml.cmx
INST_OTH=_build/lib/*.mli _build/phylocaml.cm[io]

# compilation to pdf; references need to be replaced
LATEX=latex
DVIPS=dvips

# additional tags for compiling with coverage
COVERAGE_TAGS=package\(bisect\),syntax\(camlp4o\),syntax\(bisect_pp\)

# -----------------------------------

all : phyloc native byte

native : phyloc
	$(BUILD) phylocaml.cmxa

byte : phyloc
	$(BUILD) phylocaml.cma

top : phyloc
	$(BUILD) phylocaml.top

phyloc :
	$(BUILD) libphyloc.a

coverage : phyloc
	$(BUILD) -tags $(COVERAGE_TAGS) test.byte

# -----------------------------------

%.byte : phyloc
	$(BUILD) $*.byte

%.native : phyloc
	$(BUILD) $*.native

%.cmo :
	$(BUILD) $*.cmo

# -----------------------------------

app :
	cd app && $(MAKE)

bench : 
	cd bench && $(MAKE)

test :
	cd test && $(MAKE)

# -----------------------------------

distclean :
	cd bench && $(MAKE) clean
	cd test && $(MAKE) clean
	cd app && $(MAKE) clean

clean : distclean
	rm -f *.native *.byte *.top $(DOCDIR)
	$(BUILD) -clean

# -----------------------------------

install.byte : install.init
ifeq ("$(wildcard $(BYTE_SMOKE))","$(BYTE_SMOKE)")
	$(OFIND) install -add phylocaml $(INST_BYT)
endif

install.native : install.init
ifeq ("$(wildcard $(NATIVE_SMOKE))","$(NATIVE_SMOKE)")
	$(OFIND) install -add phylocaml $(INST_NAT)
endif

install.init :
	$(OFIND) install phylocaml META $(INST_OTH)

install : install.init install.native install.byte

uninstall :
	$(OFIND) remove phylocaml

# -----------------------------------

phylocaml.html :
	$(BUILD) $(DOCDIR)/index.html

phylocaml.tex :
	$(BUILD) $(DOCDIR)/phylocaml.tex

phylocaml.pdf : phylocaml.tex
	cd _build/$(DOCDIR)/ && \
	$(LATEX) phylocaml.tex && \
	$(LATEX) phylocaml.tex && \
	$(DVIPS) phylocaml.dvi -o $(@F)

dot :
	$(BUILD) $(DOCDIR)/phylocaml.dot

%.mli :
	$(BUILD) $*.inferred.mli && cp _build/lib/$*.inferred.mli lib/$*.mli
