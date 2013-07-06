
module type S =
  sig
    type t

    type fp

    type topo

    module FingerPrint : sig
      type t
      val fingerprint : topo -> t
      val compare : t -> t -> int
    end

    module Ordered : Map.OrderedType with type t = fp
    module CladeSet : Set.S with type elt = fp
    module CladeMap : Map.S with type key = fp

    val fpcompare : fp -> fp -> int

    val calc : topo -> t

    val sets : topo -> CladeSet.t

    val query : Topology.edge -> t -> fp

    val num_leaves : fp -> int

    val fold : (Topology.edge -> fp -> 'a -> 'a) -> t -> 'a -> 'a

end

