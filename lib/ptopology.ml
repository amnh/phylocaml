open Phylocaml_pervasives

type ('a,'b) break_delta = {
  topo_break_delta : Topology.break_delta;
}

type ('a,'b) join_delta = {
  topo_join_delta : Topology.join_delta;
}

type ('a,'b) reroot_delta = {
  topo_reroot_delta : Topology.reroot_delta;
}

module type S =
  sig

    type topology

    type 'a root_node = (Topology.jxn * 'a) option

    type 'a root = {
      assignment : 'a root_node;
      component_cost : float;
      adjusted_cost : float;
    }

    type ('a,'b) t = {
      topology  : topology;
      node_data : 'a Topology.IDMap.t;
      root_data : 'b Topology.EdgeMap.t;
      comp_root : 'a root Topology.IDMap.t;
    }

    (** Basic tree operations for creation *)
    val empty : ('a,'b) t
    val random : 'a list -> ('a,'b) t
    val disjoint : 'a list -> ('a,'b) t

    (** Basic returns of nodes / ids / edges *)
    val get_node_data : Topology.id -> ('a,'b) t -> 'a
    val remove_node_data : Topology.id -> ('a,'b) t -> ('a,'b) t
    val set_node_data : Topology.id -> 'a -> ('a,'b) t -> ('a,'b) t

    val get_root_data : Topology.edge -> ('a,'b) t -> 'b
    val remove_root_data : Topology.edge -> ('a,'b) t -> ('a,'b) t
    val set_root_data : Topology.edge -> 'b -> ('a,'b) t -> ('a,'b) t

    val get_component_root : Topology.handle -> ('a,'b) t -> 'a root
    val set_component_root : Topology.handle -> 'a root -> ('a,'b) t -> ('a,'b) t
    val remove_component_root : Topology.handle -> ('a,'b) t -> ('a,'b) t

    (** Basic tree level operations *)
    val break : ('a,'b) t -> Topology.edge -> ('a,'b) t * ('a,'b) break_delta
    val join : ('a,'b) t -> Topology.jxn -> Topology.jxn -> ('a,'b) t * ('a,'b) join_delta
    val reroot : ('a,'b) t -> Topology.edge -> ('a,'b) t * ('a,'b) reroot_delta
    val disjoin : ('a,'b) t -> ('a,'b) t

    val exhaustive : 
      node_enum:('a option -> 'a -> 'a option) ->
        root_enum:('b option -> 'b -> 'b option) ->
          compare:(('a,'b) t -> ('a,'b) t -> bool) ->
            ('a,'b) t -> ('a, 'b) t

    (** I/O *)
    val to_string : ('a,'b) t -> string
    val print : ('a,'b) t -> unit
    val of_parsed : 'a list -> Topology.EdgeSet.t -> ('a,'b) t
    val to_parsed : ('a,'b) t -> Topology.EdgeSet.t

  end

module Make (Topo : Topology.S) : S =
  struct

    module IDMap = Topology.IDMap
    module EdgeMap = Topology.EdgeMap

    type topology = Topo.t

    type 'a root_node = (Topology.jxn * 'a) option

    type 'a root = {
      assignment : 'a root_node;
      component_cost : float;
      adjusted_cost : float;
    }

    type ('a,'b) t = {
      topology  : topology;
      node_data : 'a IDMap.t;
      root_data : 'b EdgeMap.t;
      comp_root : 'a root IDMap.t;
    }

    let empty = failwith "TODO"
    let random _ = failwith "TODO"
    let disjoint _ = failwith "TODO"

    let get_node_data _ _ = failwith "TODO"
    let remove_node_data _ _ = failwith "TODO"
    let set_node_data _ _ _ = failwith "TODO"

    let get_root_data _ _ = failwith "TODO"
    let remove_root_data _ _ = failwith "TODO"
    let set_root_data _ _ _ = failwith "TODO"

    let get_component_root _ _ = failwith "TODO"
    let set_component_root _ _ _ = failwith "TODO"
    let remove_component_root _ _ = failwith "TODO"

    let break _ _ = failwith "TODO"
    let join _ _ = failwith "TODO"
    let reroot _ _ = failwith "TODO"
    let disjoin _ = failwith "TODO"

    let exhaustive ~node_enum ~root_enum ~compare _ = failwith "TODO"

    let to_string _ = failwith "TODO"
    let print  _ = failwith "TODO"
    let to_parsed _ = failwith "TODO"
    let of_parsed _ _ = failwith "TODO"

  end
