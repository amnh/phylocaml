module type S =
  sig

    type t

    type deferred

    val neighborhood : t -> Topology.delta list

    val deferred_init : t -> deferred

    val deferred_next : deferred -> deferred option * Topology.delta

    val deferred_partition : t -> int -> deferred list

    val lazy_neighborhood : t -> Topology.delta Llist.llist

    val lazy_of_deferred : t -> deferred -> Topology.delta Llist.llist

  end
