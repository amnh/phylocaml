
module type S =
    sig
        type a

        type b

        type r

        type t
        
        (** Return the basic topological structure of the data-attached topology *)
        val get_topology : t -> r
            
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
    
        (** Find an optimal assignment through exhaustive search *)
        val exhaustive : t -> t 


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

        (** Perform a direct map of the elememts of the topology *)
        val map : (a -> a) -> (b -> b) -> t -> t

        (** Apply a topology delta to the topology *)
        val apply_delta : Topology.delta -> t -> t

        (** Perform a map of the elements in a diagnosis fashion, via downpass,
            then an uppass, with functions passed for each node *)
        val map_diagnosis :
            (a option -> a -> a) -> (a -> a -> b -> b) -> t -> t

        (** Perform a post_order traversal of the topology folding over elements
            of the nodes and edges accumulating the result *)
        val fold_diagnosis :
            (a option -> b option -> a -> 'a -> 'a) -> Topology.edge -> t -> 'a

        (** Return the cost of the rooting at the position specified in the topology *)
        val root_costs : t -> (Topology.edge * float) list

        (** Return the total cost of the topology *)
        val topology_cost : t -> float

        (** Compose root and topology costs; this is the costs of costs *)
        val total_cost : t -> float

        (** Debugging function for printing the topology and node states *)
        val print : t -> unit

    end


module Make (PTopo : Ptopology.S) (Root: Node.R) (Node: Node.R) =
    struct

        type a = Node.n

        type b = Root.n

        type r = PTopo.topology

        type t = (a,b) PTopo.t

        let get_topology t = t.PTopo.topology
    
        let apply_delta _ _ = failwith "TODO"

        let break_fn _ _ = failwith "TODO"
        let reroot_fn _ _ = failwith "TODO"
        let cost_fn _ _ _ = failwith "TODO"
        let join_fn _ _ _ = failwith "TODO"
        let assign_fn _ = failwith "TODO"
        let model_fn _ = failwith "TODO"
        let downpass _ = failwith "TODO"
        let uppass _ = failwith "TODO"
        let diagnose _ = failwith "TODO"
        let exhaustive _ = failwith "TODO"
        let map _ _ _ = failwith "TODO"
        let map_diagnosis _ _ _ = failwith "TODO"
        let fold_diagnosis _ _ _ = failwith "TODO"
        let root_costs _ = failwith "TODO"
        let topology_cost _ = failwith "TODO"
        let total_cost _ = failwith "TODO"
        let print _ = failwith "TODO"

    end

