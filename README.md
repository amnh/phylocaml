phylocaml
=========

A Phylogenetic Library based on a refactoring of POY (code.google.com/p/poy).

We've thought a lot about how to develop a library from what we have and the
experience we have gathered from our work on POY3/4/5. The portion of the
code-base that can be easily translated over is the diagnosis subset --the
section of code that can analyze a topology and assign optimal ancestral states
to internal nodes under different optimality criterion. Anyone can develop a
search heuristic mechanism around this. Of course, designing one that is
expandable and encompasses 90% of users demands is another story. This document
talks about the design decisions to diagnose a topology, obtaining a score,
recovering states, modifying the topology (break, join, re-root, fuse, and
dealing with a forest) and optimizing those states on the topology to reduce the
overall score.

Table of Contents
-----------------

  + Overview of Diagnosis
  + Overview of Search
  + Overview of Utility Modules
  + Design Choices and Rationale
  + Current Open Issues


Overview of Diagnosis
------------------

Currently, in POY5, we encapsulate this pretty well. There is some leaking with
modules that do things to very specific data, but they are few and far between.
This refactoring will also be an effort to eliminate this leakage. We believe a
functor approach of a diagnosis module will be effective. In this way we can
implement a parallel diagnosis module that can plug into whatever the diagnosis
module uses (the Search module for example) as well as deal with unforeseen
issues with networks or more complex topologies in optimizing states.

          +------------+----------+-------------+----------+-------+
          | Likelihood | Sequence | NonAdditive | Additive |  ...  |<--+
          +------------+----------+-------------+----------+-------+   |
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


In Figure 1, the diagnosis module is a functor about a node root, and ptopo.
The topology designates a way to traverse the topology, and ptopology attaches
data to the nodes and roots. This allows separation between structure and gives
a common and basic interface to attach data to a topology. Topology additionally
contains a compare module for traversing a topology in a consistent way[^1].

The Node/Root module is a functor itself to apply a previously mentioned compare
module and a data module that contains all the functions in the optimality
criteria: Likelihood, Additive, Chromosome, Genome, Sankoff, ... all share a
common module interface (NodeData). Very close to what we have now with some
added components.

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


Overview of Search
------------------

The search framework will require thoughtfulness to be able to encompass a wide
range of known search heuristics, as well as unknown situations in regard to
networks and other topologies.

A robust framework can be made from building onto some basic functionality. In
essence we need a a few primary objects for a search,
    + Solution - A topology.
    + Comparator - A method to choose between two or more topologies.
    + Strategy - Adaptive plan to determine successive structural modifications.
    + Convergence - A method to stop the search strategy (optional).

This defines a local search. For an SPR search the strategy is the SPR
neighborhood; a comparator is a utility or cost function of the diagnosis
module; and convergence, if we want to be exhaustive in the neighborhood is
unnecessary.

    let local_search ?convergence ~compare (module N : Neighborhood.S) x = ...

This local-search procedure can be used to build up a more global search
procedure that includes perturbations, tabu search, and other functionalities
for global optimization and as a meta-heuristic. These procedures can be
defined separately instead of a single all encompassing search function,
although that generality would be nice at some point in developing the framework.

                                      +--------------+       +--------------+
    +---------+------------+    +-----| Convergence  |   +---| Feedback     |
    | Search  | Diagnosis  |<---+     +--------------+   |   +--------------+
    +---------+------------+    |     +--------------+   |   +--------------+
                     |          +-----| Comparator   |   +---| Perturb      |
    +-----------+    |          |     +--------------+   |   +--------------+
    | Diagnosis |<---+          |     +--------------+   |   +--------------+
    +-----------+               +-----| Neighborhood |   +---| ...          |
                                |     +--------------+   |   +--------------+
                                +------------------------+

    Figure 2. A simple and preliminary diagram of the Search framework


Overview of Utility Modules
---------------------------

These are a list of modules that can be composed and used to handle a wide range
of data for different optimality criterion.

    + cost\_matrix - handles median and cost assignments
    + sequence - handles an ordered continuous set of characters in an alphabet
    + align - algorithms to align sequences 


Design Choices and Rationale
----------------------------

  + In nodes containing multiple directions and because of heuristics in
    diagnsing unrooted topologies, we need to know which directional node is
    being queried. This can be done two different ways: by the parent or by the
    children. This variant type is located in the node module and can be used in
    any type of node for any topology.
  + Neighborhoods are defined by a first-class module passed to an enumeration
    function. This neighborhood module will define how breaks and deltas are
    generated. Deltas are a series of steps to transform a tree. Break, Join,
    and Reroot currently seem to be the only ones neccassy for a full featured
    neighborhood search, but this can be expanded if other options are needed
    (for example, optimization of the tree if this doesn't have a place
    elsewhere in the design of the application).
  + The NodeComparator is built to generally satisfy the left/right or ordering
    of children for medians[^1]. The design is abstract and depends on proper
    functorizing of the node module with the topology. Currently it is not
    known if this issue will arise with multiple parents in a topology
    (reticulate nodes of networks), or if there is a better way to allow
    consistency between functorized modules.


Current Open Issues
-------------------

  + Likelihood branch lengths. How should data be attached to edges for
    calculating a median? The current solution is for the another implentation
    of ptopology that contains a third parameter for a floating-point table, or
    that it takes the nodedata to define what type of data to attach to the
    nodes.
  + Does the creation of multiple diagnosis modules cause issues with Search?
  + General discussion of the Search module.

-------------------------------------------------------------------------------

[^1] - Traversing a topology in a consistent way is important for
reproducibility but this particular issue has to do the ordering in which
nodes enter a function to produce the same results. For example, although
cost(a,b) = cost(b,a), it does not hold that median(a,b) = median(b,a). This is
because the traceback in the alignment matrix when faced with two equally
parsimonious alignments, must choose a direction. As these different traversals
build up the tree to the root, vastly different assignments and costs can
result. The different assignments/costs are correct none-the-less. The final
differences are taken care of in the uppass and the assignment functions that
optimize the internal states of the topology.
