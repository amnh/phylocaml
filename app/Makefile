.PHONY : clean

BUILD=ocamlbuild
BUILDARGS=-use-ocamlfind

%.native :
	$(BUILD) $(BUILDARGS) $*.native

%.byte :
	$(BUILD) $(BUILDARGS) $*.byte

clean :
	$(BUILD) -clean
