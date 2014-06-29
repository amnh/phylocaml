.PHONY : clean all

.DEFAULT : all

BUILD=ocamlbuild
BUILDARGS=-use-ocamlfind -tags "package(phylocaml,core_bench,core),thread,debug,annot"

all : align.native

%.native :
	$(BUILD) $(BUILDARGS) $*.native

%.byte :
	$(BUILD) $(BUILDARGS) $*.byte

clean :
	$(BUILD) -clean
