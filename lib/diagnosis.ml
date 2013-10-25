(** Interface for Diagnosis of Topologies
 
    Define an interface for the diagnosis of a topology. This composes the
    topology with data-types to generate a diagnosis for specific data. *)

module type S =
  sig
  
    (** {2 Types} *)
    
    type t

    type n  (** Node Data type *)

    type r  (** Topology type *)

    type s  (** Minimal Specification of Data for Parallelism *)

    type d  (** Heuristic Cost Function Residiual Information *) 


    (** {2 Accessors} *)

    (** Return the basic topological structure of the data-attached topology *)
    val get_topology : t -> r


    (** {2 Topology Functions} *)

    (** Remove the edge from the topology; this may not produce two disjoint
        sets, this is the case in network topologies. This can be determined
        from the break-delta also returned with the updated topology. *)
    val break_fn : Topology.edge -> t -> t * Topology.break_delta

    (** Reroot the topology to the provided edge - used for printing and
        diagnosis of the topology. *)
    val reroot_fn : Topology.edge -> t -> t * Topology.reroot_delta

    (** Find the added cost of joining the two jxn points *)
    val cost_fn : Topology.jxn -> Topology.jxn -> t -> float

    (** Join the two jxn points in the topology *)
    val join_fn : Topology.jxn -> Topology.jxn -> t -> t * Topology.join_delta

    (** Optimize the assignment of states at nodes *)
    val assign_fn : t -> t

    (** Optimize the model that defines the evolutionary aspects of the nodes *)
    val model_fn: t -> t

    (** Perform a full-analysis on the topology; this calls the downpass
        then uppass, then model and assignment functions until convergence *)
    val diagnose: t -> t


    (** {2 Load Data / Creation Functions} *)

    val load_data : n list -> r -> t

    val replace_data : n list -> t -> t


    (** {2 Delta Functions for Search} *)

    (** Apply a topology delta to the topology *)
    val heuristic_delta : Topology.topology_delta -> t -> float * d

    (** Apply a topology delta to the topology *)
    val apply_delta : Topology.topology_delta -> d -> t -> t


    (** {2 Higher-Order Functions on Data} *)

    (** Perform a direct map of the elememts of the topology *)
    val map : (n -> n) -> (n -> n) -> t -> t

    (** Perform a map of the elements in a diagnosis fashion, via downpass,
        then an uppass, with functions passed for each node *)
    val map_diagnosis :
        (n option -> n -> n) -> (n -> n -> n -> n) -> t -> t

    (** Perform a post_order traversal of the topology folding over elements
        of the nodes and edges accumulating the result *)
    val fold_diagnosis :
        (n option -> n option -> n -> 'a -> 'a) -> Topology.edge -> t -> 'a


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


    (** {2 Encoding / Decoding} *)

    (** Encode a tree to a specification that can more easily be transfered over
        a channel. *)
    val encode : t -> s

    (** Decode a specification to a topology. *)
    val decode : n list -> s -> t


    (** {2 Debugging and I/O} *)

    (** Debugging function for printing the topology and node states *)
    val print : t -> unit
  end
