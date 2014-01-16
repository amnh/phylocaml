phylocaml
=========
A Phylogenetic Library based on a refactoring of [POY](http://code.google.com/p/poy).

The **goal** of *phylocaml* is to use the experience from *POY 3/4/5* to develop a
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

+ Dependencies
+ Configure, Install, and Uninstall
+ Test Framework
+ Quick Start
  + Overview of Diagnosis Modules
  + Overview of Search Modules
  + Overview of Utility Modules
+ Design Choices and Rationale
+ Contact/Location Information


Dependencies
============
+ [OCaml](http://caml.inria.fr/ocaml/) (4.00.1+)
+ [Pareto](http://github.com/superbobry/pareto/)
+ [GSL](https://bitbucket.org/mmottl/gsl-ocaml)
+ [Findlib](http://projects.camlcity.org/projects/findlib.html)
+ [ctypes] (https://github.com/ocamllabs/ocaml-ctypes)
+ [oUnit](http://ounit.forge.ocamlcore.org/) (optional)

Configure, Install, and Uninstall
=====================

Currently, there is no configuration step.

Typing 'make' will initiate native and bytecode compilation of the Phylocaml
library.  After building, 'make install' will do a findlib installation.  To
generate the ocamldoc API documentation, use 'make docs'.

To remove Phylocaml type 'make uninstall' or it can be done directly via the
findlib command, 'ocamlfind remove phylocaml'.

Installation via OPAM is available by including our opam-repo. This can be added
via, 'opam repository add AMNH git://github.com/amnh/opam-amnh.git' and then
installing the provided package.


Testing Framework
=================
[![Build Status](https://travis-ci.org/amnh/phylocaml.png?branch=master)](https://travis-ci.org/amnh/phylocaml)

The tests can be built by installing the package and linking, or by compiling it
directly together. Doing both ensures everything is packaged and installed
properly. This switch is done with USE\_EXTERNAL\_LINKING pre-processor flag.

The externally linked tests are compiled by,

    make extests
    ./tests/tests.native

and the locally linked build is done by,

    make tests
    ./tests.native

Documentation
=================

The documentation is built using ocamlbuild and can be built from,

    make docs


Quick Start
===========

Overview of Diagnosis Modules
-----------------------------

The diagnosis module contains the abilities to take a topology and node data and
assign optimal states, model parameters and assign a utility function (cost)
to the topology.

Currently, in POY 5, we encapsulate this pretty well. There is some leaking with
modules that do things to very specific data, but they are few and far between.
This refactoring will be an effort to eliminate this leakage. We believe a
functor approach of a diagnosis module will be effective. In this way we can
implement a parallel diagnosis module that can plug into whatever the diagnosis
module uses (the Search module for example) as well as deal with unforeseen
issues with networks or more complex topologies in optimizing states.

                                                            +-------+
       +------------+----------+-------------+----------+---+---+   |
       | Likelihood | Sequence | NonAdditive | Additive |  Set  |<--+--+
       +------------+----------+-------------+----------+-------+      |
                                                                       |
          +--------------+-----------+         +--------+----------+   |
          |              < Node      |-------->|        | NodeData |---+
          |              +-----------+         | Node/  +----------+
          | Diagnosis    | Topo      |----+    |  Root  | Compare  |
          |   of Model   +-----------+    |    +--------+----------+
          |              |           |    |    +----------+   |
          +--------------+-----------+    +--->| Topology |<--+
                    |                          +----------+
                    |                          +----+----+-------+
                    +------------------------->| ML | MP | Kolmo |
                                               +----+----+-------+

           Figure 1. Basic Module dependency diagram of a Diagnosis


In Figure 1, the diagnosis module is a functor about a model and topology.
The topology designates a way to traverse the topology, and ptopology attaches
data to the nodes and roots. This allows separation between structure and gives
a common and basic interface to attach data to any topology. Topology additionally
contains a compare module for traversing a topology in a consistent way[^1].

The diagnosis module is dependent on a specific type of Node type because it
manages the data through the topological manipulations. The Node/Root module is
a functor itself to apply a previously mentioned compare module and a data
module that contains all the functions in the optimality criteria (Likelihood,
Additive, Chromosome, Genome, Sankoff, et cetera) all share a common module
interface (NodeData). Very close to what we have now with some added components.

Nodes can also be abstracted to contain further features --laziness has been
important, as well as a directional node for un-rooted trees in selecting the
best downpass assignments to perform an uppass on (each internal node of a
binary tree has three of these corresponding to each combination of 2 children).
You can see these implementations in the Node module.

Example code to diagnose a tree with known data taxon data and tree file,

    module Node = Node.Make3D (Tree.NodeComparator) (SequenceData)
    module Diag = MPDiagnosis.Make (Node) (Tree)
    let diagnose_tree taxa_data edges =
        let nodes = List.map (fun x -> Node.of_data (SequenceData.of_string x)) taxa_data in
        let tree = PTree.of_parsed edges nodes in
        let tree = Diag.diagnose tree in
        Printf.printf "%s:[%d]" (PTree.to_string tree) (Diag.total_cost tree)


Overview of Search Modules
--------------------------

We provide module implementations that support the [OCamion](https://github.com/AMNH/ocamion)
project, although we do not directly depend on that library --it is thus highly
recommended to install though.


Overview of Utility Modules
---------------------------

These are a list of modules that can be composed and used to handle a wide range
of data for different optimality criterion. These are oft interfaces to C
data-types and would be scripted to handle floats, doubles, ints, chars, long,
et cetera to accommodate a changing need. Special casing some would be
appropriate to add vectorization to the mix, but previous/general
implementations and pure OCaml implementations would be built for verification
purposes.

+ CostMatrix - Handles median and cost assignments
+ Sequence - Handles an ordered continuous set of characters in an alphabet
+ Align - Algorithms to align sequences 
+ Alphabet - Handles codes and names association and type of alphabet to
  generate a cost\_matrix when paired with transformation matrix.
+ FingerPrinting - Interface to create short tags of data-structures for tabu
  managers in a search. Not required, else entire topology may be stored and
  compared, or no tabus implemented. Also can be used for fusing to name
  potential clades of the topology.
+ LikelihoodModel - General storage for a likelihood model.


Design Choices and Rationale
============================

+ In nodes containing multiple directions and because of heuristics in
  diagnosing un-rooted topologies, we need to know which directional node is
  being queried. This can be done two different ways: by the parent or by the
  children. This variant type is located in the node module and can be used in
  any type of node for any topology.
+ Traversing a topology in a consistent way is important for reproducibility
  but this particular issue has to do with the ordering in which nodes enter a
  function to produce the same results. For example, although
  cost(a,b) = cost(b,a), it does not hold that median(a,b) = median(b,a). This
  is because the traceback in the alignment matrix when faced with two equally
  parsimonious alignments, must choose a direction. As these different
  traversals build up the tree to the root, vastly different assignments and
  costs can result. The different assignments/costs are correct none-the-less.
  The final differences are taken care of in the uppass and the assignment
  functions that optimize the internal states of the topology.
+ Neighborhoods are defined by a first-class module passed to an enumeration
  function. This neighborhood module will define how breaks and deltas are
  generated. Deltas are a series of steps to transform a tree. Break, Join,
  and re-root currently seem to be the only ones necessary for a full featured
  neighborhood search, but this can be expanded if other options are needed
  (for example, optimization of the tree if this doesn't have a place
  elsewhere in the design of the application). The delta is associated with the
  diagnosis module so the search can be as abstract as possible.
+ The NodeComparator is built to generally satisfy the left/right or ordering
  of children for medians[^1]. The design is abstract and depends on proper
  functorizing of the node module with the topology. Currently it is not
  known if this issue will arise with multiple parents in a topology
  (reticulate nodes of networks), or if there is a better way to allow
  consistency between functorized modules.


Contact/Location Information
===================
+ Nicholas Lucaroni (nlucaroni at amnh dot org)
+ Ward Wheeler (wheeler at amnh dot org)
+ Lavanya Kannan (lkannan at amnh dot org)
+ [Support Forum](https://groups.google.com/forum/?fromgroups#!forum/poy4)
+ [Site on AMNH](http://www.amnh.org/our-research/computational-sciences/)

-------------------------------------------------------------------------------
