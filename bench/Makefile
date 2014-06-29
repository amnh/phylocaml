.PHONY : clean

BUILD=ocamlbuild
BUILDARGS=-use-ocamlfind -tags "package(phylocaml,core_bench,core),thread,debug,annot"

%.native :
	$(BUILD) $(BUILDARGS) $*.native

%.byte :
	$(BUILD) $(BUILDARGS) $*.byte

clean :
	$(BUILD) -clean
