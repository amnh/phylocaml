open Phylocaml_pervasives

type deferred =
  { current_key : Topology.edge option;
    data_set : EdgeSet.t EdgeMap.t;
  }

let neighborhood t h1 h2 = failwith "TODO"
            
let generate_deferred ?reroot_distance ?join_distance t h1 h2 = failwith "TODO"

let get_next k data =
  if EdgeMap.is_empty data then
    None
  else
    begin match k with
      | None ->
        let k,s = EdgeMap.min_binding data in
        if EdgeSet.is_empty s then
          get_next None (EdgeMap.remove k data)
        else
          let e = EdgeSet.min_elt s in
    in
    if EdgeSet.
    let e = Edgeset.min_elt s in


let rec deferred_neighborhood t h1 h2 dopt = match dopt with
  | None   -> deferred_neighborhood t h1 h2 (Some (generate_deferred t h1 h2))
  | Some x ->

let deferred_partition _ _ _ _ = failwith "TODO"
