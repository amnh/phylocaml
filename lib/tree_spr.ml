

type t = Tree.t

type deferred = Topology.EdgeSet.t

let has_deferred = true

let neighborhood t h1 h2 = failwith "TODO"
 
let get_deferred ?distance t h1 h2 : deferred option = failwith "TODO"

let init _ _ _ = failwith "TODO"

let next_delta _ = failwith "TODO"

let init_partition _ _ _ _ = failwith "TODO"
