PhylOCaml
=========
A Phylogenetic Library based on a refactoring of [POY](http://code.google.com/p/poy).

The **goal** of *PhylOCaml* is to use the experience from *POY 3/4/5* to develop a
library. A major reason for this refactoring is that we need the ability to
support different types of topologies from our previous implementations.
Although our utility is to develop a rooted network approach, we believe the
framework can be used in general for other types of topologies and offers us a
way to write a powerful library for bioinformatics research.

We are also taking the approach to utilize many of the new features that the
OCaml team has developed over the years, prefer a standard library (Core), and
become more idiomatic to the standards developed since the initial versions.


Table of Contents
=================

+ [Dependencies](#dependencies)
+ [Installation](#installation)
+ [Testing Framework](#testing-framework)
+ [Documentation](#documentation)
+ [Benchmarks](#benchmarks)
+ [Applications](#applications)
+ [Contact Information](#contact-information)
+ [License](#license)


Dependencies
============

Required -
+ [OCaml](http://ocaml.org) (4.00.1+)
+ [Pareto](http://github.com/superbobry/pareto/)
+ [GSL](https://bitbucket.org/mmottl/gsl-ocaml) (via Pareto)
+ [Findlib](http://projects.camlcity.org/projects/findlib.html)

Optional -
+ [OUnit2](http://ounit.forge.ocamlcore.org/)
+ [Core\_bench](https://blogs.janestreet.com/ocaml-core/110.01.00/doc/core_bench/)
+ [OPAM](http://opam.ocaml.org)
+ [OCamion](http://github.com/amnh/ocamion/)


Installation
=====================

Currently, there is no configuration step.

Typing 'make' will initiate native and bytecode compilation of the Phylocaml
library.  After building, 'make install' will do a findlib installation.  To
generate the ocamldoc API documentation, use 'make phylocaml.html'.

To remove Phylocaml type 'make uninstall' or it can be done directly via the
findlib command, 'ocamlfind remove phylocaml'.

Installation via OPAM is available by including our opam-repo. This can be added
via, 'opam repository add AMNH git://github.com/amnh/opam-amnh.git' and then
installing the provided package.


Testing Framework
=================
[![Build Status](https://travis-ci.org/amnh/phylocaml.png?branch=master)](https://travis-ci.org/amnh/phylocaml)
[![Coverage Status](https://coveralls.io/repos/amnh/phylocaml/badge.png)](https://coveralls.io/r/amnh/phylocaml)

The tests can be built by installing the package and linking, or by compiling it
directly together. Doing both ensures everything is packaged and installed
properly. This switch is done with USE\_EXTERNAL\_LINKING pre-processor flag.

The externally linked tests are compiled by,

    make test
    ./tests/test.native

and the locally linked build is done by,

    make test.native
    ./test.native


Documentation
=================

The api documentation (PDF and HTML) is built using ocamlbuild and can be built from,

    make phylocaml.html
    make phylocaml.tex
    make phylocaml.pdf

Further developer documentation can be found on the github wiki, and complete examples
are maintained in the app/ directory.


Benchmarks
==========

Benchmarks for different implementations of C and OCaml functions, as well as comparisons
of analogous methods can be found in the bench/ directory. Running,

    make bench

will produce an entry point for all the benchmarks which can be determined through the
command,

    ./bench --help

The benchmark suite requires Core\_bench.


Applications
============

A number of applications can be found in the app/ directory. These are used as entry
points for users to experiment initially and to prove the framework we've developed.
Within that directory is further documentation regarding the individual applications.


Contact Information
===================
+ Nicholas Lucaroni (nlucaroni at amnh dot org)
+ Ward Wheeler (wheeler at amnh dot org)
+ [Support Forum](https://groups.google.com/forum/?fromgroups#!forum/poy4)
+ [Site on AMNH](http://www.amnh.org/our-research/computational-sciences/)


License
===================
Mozilla Public License, version 2.0


-------------------------------------------------------------------------------
