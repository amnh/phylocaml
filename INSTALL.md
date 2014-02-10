Phylocaml Installation Instructions
===================================

From Source
-----------

Uncompress source distribution. Go to the root directory of the folder and run,

    make
    make install

One can choose to build only the bytecode or native library distributions by running,

    make phylocaml.native
    make phylocaml.byte
    make install


From OPAM
---------

Add the amnh OPAM repository, update cache, and execute installation of Phylocaml,

    opam repository add AMNH git://github.com/amnh/opam-amnh.git
    opam update
    opam install phylocaml


Documentation
-------------

HTML or LaTeX type-set docs can be built using the following commands,

    make phylocaml.html
    make phylocaml.tex
    make phylocaml.pdf


Uninstalling Instructions
=========================

Run from the source distribution,

    make uninstall

or without the source,

    ocamlfind remove phylocaml

