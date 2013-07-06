open Phylocaml_pervasives

let gen_tbr ?(reroot_dist=None) ?(join_dist=None) cost_fn =
  let module TBR =
    struct
      type t          = Tree.t
      type tabu       = unit
      type deferred   = unit
      let neighborhood _ _ = failwith "TODO"
      let deferred_init _ _ = failwith "TODO"
      let deferred_next _ = failwith "TODO"
      let deferred_partition _ _ = failwith "TODO"
      let tabu_empty = ()
      let tabu_add _ _ = failwith "TODO"
      let next _ _ _ = failwith "TODO"
      let cost = cost_fn
    end
  in
  (module TBR : Neighborhood.S with type t = Tree.t)


let gen_spr ?(join_dist=None) cost_fn =
  let module SPR =
    struct
      type t          = Tree.t
      type tabu       = unit
      type deferred   = unit
      let neighborhood _ _ = failwith "TODO"
      let deferred_init _ _ = failwith "TODO"
      let deferred_next _ = failwith "TODO"
      let deferred_partition _ _ = failwith "TODO"
      let tabu_empty = ()
      let tabu_add _ _ = failwith "TODO"
      let next _ _ _ = failwith "TODO"
      let cost = cost_fn
    end
  in
  (module SPR : Neighborhood.S with type t = Tree.t)
