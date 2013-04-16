phylocaml
=========

A Phylogenetic Library based on a refactoring of POY (code.google.com/p/poy).

We've thought a lot about how to develop a library from what we have and the
experience we have gathered from our work on POY3/4/5. The portion of the
code-base that can be easily translated over is the diagnosis subset. Anyone can
develop a search heuristic mechanism around it. Of course, designing
one that is expandable and encompasses 90% of users demands is another story.
This document talks about the design decisions to diagnose a topology, obtaining a
score, recovering states, modifying the topology (break, join, re-root, fuse,
and dealing with a forest) and optimizing those states on the topology to reduce
the overall score.

Currently, in POY5, we encapsulate this pretty well. There is some leaking with
modules that do things to very specific data, but they are few and far between.
I think a functor approach of a diagnosis module will be effective. In this way
we can implement a parallel diagnosis module that can plug into whatever the
diagnosis module uses (the Search module for example) as well as deal with
unforseen issues with networks or more complex topologies.

          +------------+----------+-------------+----------+-------+
          | Likelihood | Sequence | NonAdditive | Additive |  ...  |<--+
          +------------+----------+-------------+----------+-------+   |
                                                                       |
          +----------------+---------+         +--------+----------+   |
          |                | Node    |-------->|        | NodeData |---+
          |                +---------+         | Node   +----------+
          |  Diagnosis     | Root    |-------->|        | Compare  |
          |                +---------+         +--------+----------+
          |                | PTopo   |<---+                  |
          +----------------+---------+    +------------------+

           Figure 1. Basic Module dependency diagram of a Diagnosis


In Figure 1, you can see that Diagnosis takes a node, root, and topology.The
topology designates a way to traverse the topology, and ptopology attaches data
to the nodes and roots, this allows separation between structure and a common
and basic interface to attach data to a topology. Topology additionally contains
a compare module for traversing a topology in a consistent way[1].

The Node/Root module is a functor itself to apply a previously mentioned compare
module and a data module that contains all the functions in the optimality
criteria: Likelihood, Additive, Chromosome, Genome, Sankoff, ... all share a
common module interface (NodeData). Very close to what we have now with some
added components.

Nodes can also be abstracted to contain further features --lazyness has been
important, as well as a directional node for un-rooted trees in selecting the
best downpass assignments to perform an uppass on (each internal node of a
binary tree has three of these corresponding to each combination of 2 children).
You can see these implementations in the Node module example.

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

-------------------------------------------------------------------------------

[1] - Naturally, Traversing a topology in a consistent way is important for
reproducibility but this particular issue has to do the ordering in which
nodes enter a function to produce the same results. For example, although
cost(a,b) = cost(b,a), it does not hold that median(a,b) = median(b,a). This is
because the traceback in the alignment matrix when faced with two equally
parsimonious alignments, must choose a direction. As these different traversals
build up the tree to the root, vastly different assignments and costs can
result. The different assignments/costs are correct none-the-less. The final
differences are taken care of in the uppass and the assignment functions that
optimize the internal states of the topology.

