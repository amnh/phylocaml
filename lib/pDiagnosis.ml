open Internal

(** Defines a dianosis as a pre-order traversal of the space and a post-order
    traversal of the space. *)

module Make (Node : Node.S) (Topo : Topology.S) : Diagnosis.S = struct

  (** {2 Types} *)
  type n = Node.n
  type r = Topo.t
  type m = PModel.t

  type t =
    {
      data : (Node.n, Node.r) Ptopology.t;
      topo : Topo.t;
      model: PModel.t;
    }

  type s = Topo.t * PModel.t
  type d = unit


  (** {2 Accessors} *)

  (** Return the basic topological structure of the data-attached topology *)
  let get_topology t = t.topo


  (** {2 Helper functions} *)

  let update_data_to_delta_remove t delta =
    (* removal of data *)
    Topology.(t.data
      |> List.fold_right Ptopology.remove_node_data delta.removed.d_nodes
      |> List.fold_right Ptopology.remove_root_data delta.removed.d_edges
      |> List.fold_right Ptopology.remove_comp_root delta.removed.d_handles
      |> (fun x -> {t with data = x;}))

  let create_component_root _ = failwith "TODO" 

  (** Perform a post-order traversal of the topology filling in initial
      unoptimized states to the components of the topology. *)
  let downpass t = t

  (** Perform a pre-order traversal of the topology filling in single
      assignment and parent-oriented information into nodes of the topology. *)
  let uppass t = t



  (** {2 Topology Functions} *)

  (** Remove the edge from the topology; this may not produce two disjoint
      sets, this is the case in network topologies. This can be determined
      from the break-delta also returned with the updated topology. *)
  let break_fn ((a,b) as edge) t =
    let topo,delta = Topo.break edge t.topo in
    let t = update_data_to_delta_remove {t with topo} delta in



    t,delta
    
  (** Reroot the topology to the provided edge - used for printing and
      diagnosis of the topology. *)
  let reroot_fn _ _ = failwith "TODO"

  (** Find the added cost of joining the two jxn points *)
  let cost_fn _ _ _ = failwith "TODO"

  (** Join the two jxn points in the topology *)
  let join_fn _ _ = failwith "TODO"

  (** Optimize the assignment of states at nodes *)
  let assign_fn _ = failwith "TODO"

  (** Optimize the model that defines the evolutionary aspects of the nodes *)
  let model_fn _ = failwith "TODO"

  (** Perform a full-analysis on the topology; this calls the downpass
      then uppass, then model and assignment functions until convergence *)
  let diagnose t = t |> downpass |> uppass

  (** disjoin the tree, removing all interior nodes and roots. *)
  let disjoint t = failwith "TODO"


  (** {2 Load Data / Creation Functions} *)

  let load_data _ _ = failwith "TODO"

  let replace_data _ _ = failwith "TODO"

  let replace_topology _ _ = failwith "TODO"


  (** {2 Delta Functions for Search} *)

  (** Apply a topology delta to the topology *)
  let heuristic_delta _ _ = failwith "TODO"

  (** Apply a topology delta to the topology *)
  let apply_delta _ _ _ = failwith "TODO"


  (** {2 Higher-Order Functions on Data} *)

  (** Perform a direct map of the elememts of the topology *)
  let map _ _ _ = failwith "TODO"

  (** Perform a map of the elements in a diagnosis fashion, via downpass,
      then an uppass, with functions passed for each node *)
  let map_diagnosis _ _ _ = failwith "TODO"

  (** Perform a post_order traversal of the topology folding over elements
      of the nodes and edges accumulating the result *)
  let fold_diagnosis _ _ _ = failwith "TODO"


  (** {2 Cost Functions} *)

  (** Return the cost of the rooting at the position specified in the
      topology. This is used for heuristics in certain methods, in many
      data-types this could return the same cost at each rooting, it may also
      produce high-computational costs since lazy-nodes will be forced. *)
  let root_costs _ = failwith "TODO"

  (** Return the total cost of the topology *)
  let topology_cost _ = failwith "TODO"

  (** Compose root and topology costs; this is the costs of costs *)
  let total_cost _ = failwith "TODO"

  (** Return the cost of the model of the data. *)
  let model_cost _ = failwith "TODO"


  (** {2 Encoding / Decoding} *)

  (** Encode a tree to a specification that can more easily be transfered over
      a channel. *)
  let encode t = t.topo, t.model

  (** Decode a specification to a topology. *)
  let decode _ _ = failwith "TODO"


  (** {2 Debugging and I/O} *)

  (** Debugging function for printing the topology and node states *)
  let print _ = failwith "TODO"

end
