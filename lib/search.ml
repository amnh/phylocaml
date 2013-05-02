module type S = functor (Diagnosis : Diagnosis.S) ->
  sig

    type t = Diagnosis.t

    val local_search :
      compare:(t -> t -> int) ->
        (module Neighborhood.S with type t = Diagnosis.r) ->
          Topology.edge -> t -> Topology.edge option-> t option

  end

module Make (Diagnosis : Diagnosis.S) =
  struct

    type t = Diagnosis.t

    let local_search ~compare (module N : Neighborhood.S with type t = Diagnosis.r) (a,b) t =
      assert( N.has_deferred );
      let rec loop_ modified deff origt bestt =
        let delta,next_deff = N.next_delta (Diagnosis.get_topology t) deff in
        let newt = Diagnosis.apply_delta delta origt in
        let bestt,modified = match compare bestt newt with
          |  1 -> bestt,modified
          |  0 -> bestt,modified
          | -1 -> newt, true
          |  _ -> assert false
        in
        match next_deff with
        | None when modified -> Some bestt
        | None   -> None
        | Some x -> loop_ modified x origt bestt
      in
      match N.init (Diagnosis.get_topology t) a b with
      | None   -> None
      | Some x -> loop_ false x t t

  end

