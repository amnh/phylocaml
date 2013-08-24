(** Interface for Diagnosis of Topologies
 
    Define an interface for the diagnosis of a topology. This composes the
    topology with data-types to generate a diagnosis for specific data. *)

module type S =
  sig
  
    (** {2 Types} *)

    type a  (** Node Data type *)

    type b  (** Root Node Data type *)

    type r  (** Topology type *)

    type t  (* Ptopology type *)


    (** {2 Accessors} *)

    (** Return the basic topological structure of the data-attached topology *)
    val get_topology : t -> r


    (** {2 Topology Functions} *)

    (** Remove the edge from the topology; this may not produce two disjoint
        sets, this is the case in network topologies. This can be determined
        from the break-delta also returned with the updated topology. *)
    val break_fn : Topology.edge -> t -> t * (a,b) Ptopology.break_delta

    (** Reroot the topology to the provided edge - used for printing and
        diagnosis of the topology. *)
    val reroot_fn : Topology.edge -> t -> t * (a,b) Ptopology.reroot_delta

    (** Find the added cost of joining the two jxn points *)
    val cost_fn : Topology.jxn -> Topology.jxn -> t -> float

    (** Join the two jxn points in the topology *)
    val join_fn : Topology.jxn -> Topology.jxn -> t -> t * (a,b) Ptopology.join_delta

    (** Optimize the assignment of states at nodes *)
    val assign_fn : t -> t

    (** Optimize the model that defines the evolutionary aspects of the nodes *)
    val model_fn: t -> t

    (** Perform a post-order traversal of the topology filling in initial
        unoptimized states to the components of the topology. *)
    val downpass: t -> t

    (** Perform a pre-order traversal of the topology filling in single
        assignment and parent-oriented information into nodes of the topology. *)
    val uppass  : t -> t

    (** Perform a full-analysis on the topology; this calls the downpass
        then uppass, then model and assignment functions until convergence *)
    val diagnose: t -> t


    (** {2 Delta Functions for Search} *)

    (** Apply a topology delta to the topology *)
    val heuristic_delta : Topology.delta -> t -> float

    (** Apply a topology delta to the topology *)
    val apply_delta : Topology.delta -> t -> t


    (** {2 Higher-Order Functions on Data} *)

    (** Perform a direct map of the elememts of the topology *)
    val map : (a -> a) -> (b -> b) -> t -> t

    (** Perform a map of the elements in a diagnosis fashion, via downpass,
        then an uppass, with functions passed for each node *)
    val map_diagnosis :
        (a option -> a -> a) -> (a -> a -> b -> b) -> t -> t

    (** Perform a post_order traversal of the topology folding over elements
        of the nodes and edges accumulating the result *)
    val fold_diagnosis :
        (a option -> b option -> a -> 'a -> 'a) -> Topology.edge -> t -> 'a


    (** {2 Cost Functions} *)

    (** Return the cost of the rooting at the position specified in the
        topology. This is used for heuristics in certain methods, in many
        data-types this could return the same cost at each rooting, it may also
        produce high-computational costs since lazy-nodes will be forced. *)
    val root_costs : t -> (Topology.edge * float) list

    (** Return the total cost of the topology *)
    val topology_cost : t -> float

    (** Compose root and topology costs; this is the costs of costs *)
    val total_cost : t -> float

    (** Return the cost of the model of the data. *)
    val model_cost : t -> float

    (** {2 Debugging and I/O} *)

    (** Debugging function for printing the topology and node states *)
    val print : t -> unit
  end
