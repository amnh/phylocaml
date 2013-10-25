(** {1 Ptopology}
    Defines a store for data in a topology. This just associates data to nodes
    and roots, and stores optimal roots and costs for those edges. This should
    work in all topologies. A root is a node directed from two nodes and defined
    as an edge in the topology. It may be used as a heuristic for optimal
    assignment, or may provide a literal rooting of the topology. *)

(** Type to define how a root is defined. *)
type 'root root_data = {
  assignment : (Topology.jxn * 'root) Internal.IntMap.t;
  (** The assigned root for the topology, if it exists yet. *)
  component_cost : float;
  (** The component cost is the cost added by the root. *)
  adjusted_cost : float;
  (** The adjusted cost is the optimal cost of the assignments in topology. *)
}

(** The type of the data store. This is left abstract since all of its
    functional accessors and inserters are listed below.*)
type ('node, 'root) t

(** An empty topology data-structure. *)
val empty : ('node, 'root) t

(** {2 Node Functions} *)
val get_node_data   : Topology.id -> ('node, 'root) t -> 'node
val add_node_data   : Topology.id -> 'node -> ('node, 'root) t -> ('node, 'root) t
val remove_node_data: Topology.id -> ('node, 'root) t -> ('node, 'root) t

(** {2 Edge/Root Functions} *)
val get_root_data   : Topology.edge -> ('node, 'root) t -> 'root
val add_root_data   : Topology.edge -> 'root -> ('node, 'root) t -> ('node, 'root) t
val remove_root_data: Topology.edge -> ('node, 'root) t -> ('node, 'root) t

(** {2 Component Root Functions} *)
val get_comp_root   : Topology.handle -> ('node, 'root) t -> 'root root_data
val add_comp_root   : Topology.handle -> 'root root_data -> ('node, 'root) t -> ('node, 'root) t
val remove_comp_root: Topology.handle -> ('node, 'root) t -> ('node, 'root) t

(* {2 Manipulate the Store of Component Roots} *)
val get_comp_root_i : Topology.handle -> int -> ('node, 'root) t -> Topology.jxn * 'root
val add_comp_root_i : Topology.handle -> int -> Topology.jxn * 'root -> ('node, 'root) t -> ('node, 'root) t
val remove_comp_root_i : Topology.handle -> int -> ('node, 'root) t -> ('node, 'root) t
