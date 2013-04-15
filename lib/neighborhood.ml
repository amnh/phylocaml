module S =
    sig
        type deffered

        val neighborhood :
            Topology.t -> Topology.handle -> Topology.handle -> Topology.delta list

        val deffered_neighborhood :
            Topology.t -> Topology.handle -> Topology.handle -> deferred option -> Topology.delta * deferred option
        
        val deferred_partition :
            Topology.t -> Topology.handle -> Topology.handle -> int -> deferred option list
    end
