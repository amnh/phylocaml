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
+ [oUnit](http://ounit.forge.ocamlcore.org/) (optional)

Configure, Install, and Uninstall
=====================

To compile and install phylocaml run,
    make
    make install

To uninstall phylocaml run,
    make uninstall


Testing Framework
=================

We use oUnit. See the test/ directory for details.


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
          |              | Node      |-------->|        | NodeData |---+
          |              +-----------+         | Node   +----------+
          | Diagnosis    | Root      |-------->|        | Compare  |
          |              +-----------+         +--------+----------+
          |              | PTopology |----+    +----------+   |
          +--------------+-----------+    +--->| Topology |<--+
                                               +----------+

           Figure 1. Basic Module dependency diagram of a Diagnosis


In Figure 1, the diagnosis module is a functor about a node, root, and ptopo.
The topology designates a way to traverse the topology, and ptopology attaches
data to the nodes and roots. This allows separation between structure and gives
a common and basic interface to attach data to a topology. Topology additionally
contains a compare module for traversing a topology in a consistent way[^1].

The Node/Root module is a functor itself to apply a previously mentioned compare
module and a data module that contains all the functions in the optimality
criteria (Likelihood, Additive, Chromosome, Genome, Sankoff, et cetera) all
share a common module interface (NodeData). Very close to what we have now with
some added components. 

Nodes can also be abstracted to contain further features --laziness has been
important, as well as a directional node for un-rooted trees in selecting the
best downpass assignments to perform an uppass on (each internal node of a
binary tree has three of these corresponding to each combination of 2 children).
You can see these implementations in the Node module.

Example code to diagnose a tree with known data taxon data and tree file,

    module Root = Node.Make1D (Tree.NodeComparator) (SequenceData)
    module Node = Node.Make3D (Tree.NodeComparator) (SequenceData)
    module PTree= Ptopology.Make (Tree)
    module Diag = Diagnosis.Make (PTree) (Root) (Node)
    let diagnose_tree taxa_data edges =
        let nodes = List.map (fun x -> Node.of_data (SequenceData.of_string x)) taxa_data in
        let tree = PTree.of_parsed edges nodes in
        let tree = Diag.diagnose tree in
        Printf.printf "%s:[%d]" (PTree.to_string tree) (Diag.total_cost tree)


Overview of Search Modules
--------------------------

The search framework will require thoughtfulness to be able to encompass a wide
range of known search heuristics, as well as unknown situations in regard to
networks and other topologies. We would also like to spin this part off into a
general library if possible. Our problem, dealing with two NP-Hard problems
requires some extra thought that other packages do not often address. We can
look to current meta-heuristic literature to design a general library. Most of
these algorithms are dependent on a local search. At minimum this requires,

+ Solution - A topology.
+ Neighborhood- A way to generate solutions using local modifications
+ Choose - A method to choose a single member of a neighborhood for
  successive neighborhood searches.

Full search procedures, like branch and bound will also have to be employed as
well to round out an exhaustive approach to search on small data-sets.

    type local_search =
      choose : (t -> t -> bool) ->
        (module Neighborhood.S with type t = Diagnosis.t) ->
          (module LocalTabu.S with type t = Diagnosis.t) -> t -> t

    Figure 2. type definition for a local search

It is currently under consideration that the neighborhood be generated from a
lazy-list. In this way, we believe a wide range of options and strategies can be
employed. The local search modules Neighborhood and LocalTabu are specialized to
the type of the topology. This allows multiple diagnosis modules to use the same
Neighborhood for the same topology. This is important in Variable Neighborhood
Search especially and in general.

This local-search procedure can be used to build up a more global search
procedure that includes perturbations, a more robust tabu-search, and other
functionalities for global optimization and meta-heuristics. These procedures
can be defined separately instead of a single all encompassing search function,
and be parameterized about the specific requirements of the topology. In general
meta-heuristics can be separated into two categories:

Iterative Methods
+ Simulated Annealing (with restart)
+ Tabu-Search
+ Greedy Randomized Adaptive Search Procedure (GRASP)
+ Variable Neighborhood Search
+ Guided Local Search
+ Iterated Local Search

Population Based Methods
+ Scatter Search / Path Relinking
+ Evolutionary Computation (eg, GA)
+ Ant Colony Optimization
+ Firefly Optimization

The potential to compose these methods into hyper-heuristics is still a
question, but at the very least these methods encompass a wide range of ways
to vary the degrees of Intensification and Diversification. Some questions
remain regarding the search procedures,

+ Will this design be robust enough for parallel computation and other types
  of neighborhoods?
+ Does the first-class module cause speed issues?
+ Can neighborhoods be partitioned effectively with the scheme we have?
+ Can the frame-work be generalized into its own library?


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
+ Ward Wheeler(wheeler at amnh dot org)
+ [Lavanya Kannan] (lkannan at amnh dot org)
+ [Support Forum](https://groups.google.com/forum/?fromgroups#!forum/poy4)
+ [Site on AMNH](http://research.amnh.org/scicomp/research/projects/invertebrate-zoology/poy?q=projects/poy.php)


-------------------------------------------------------------------------------


