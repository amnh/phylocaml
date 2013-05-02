module type S =
  sig
    type deferred

    type t

    val has_deferred : bool

    val neighborhood :
      t -> Topology.handle -> Topology.handle -> Topology.delta list

    val init :
      t -> Topology.handle -> Topology.handle -> deferred option
 
    val next_delta :
      t -> deferred -> Topology.delta * deferred option

    val init_partition :
      t -> Topology.handle -> Topology.handle -> int -> deferred option list
  end
