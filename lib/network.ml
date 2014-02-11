(* open Internal (* Unused currently *)*)

open Topology

module NodeComparator = 
  struct
    type t =
      | Leaf of id
      | Node of t list

    let create x = Leaf x

    let ancestor_1 _ x   = x
    let ancestor_2 _ x y = Node [x;y]
    let ancestor_n _ xs  = Node xs

    let order _ _ = failwith "TODO"
    let sort _ = failwith "TODO"
  end

type node =
  | Leaf of id * id
  | Interior of id * id * id * id
  | Reticulate of id * id * id * id

type t =
  {      name : string option;
        nodes : node IDMap.t;
        edges : EdgeSet.t;
      handles : HandleSet.t;
  avail_codes : IDManager.t;
  }

exception InvalidNodeID of id
exception InvalidEdge of edge
exception InvalidHandle of handle

let empty =
  { name = None;
    nodes = IDMap.empty;
    edges = EdgeSet.empty;
    handles = HandleSet.empty;
    avail_codes = IDManager.empty;
  }

let random _ = failwith "TODO"
let disjoint _ = failwith "TODO"
let create _ = failwith "TODO"

let is_edge _ _ = failwith "TODO"
let is_node _ _ = failwith "TODO"
let is_leaf _ _ = failwith "TODO"
let is_single _ _ = failwith "TODO"

let random_edge _ = failwith "TODO"
let random_node _ = failwith "TODO"
let random_leaf _ = failwith "TODO"
let random_single _ = failwith "TODO"

let get_edge _ _ _ = failwith "TODO"
let get_node _ _ = failwith "TODO"
let get_neighbors _ _ = failwith "TODO"
let get_leaves _ = failwith "TODO"
let get_single _ = failwith "TODO"
let handle_of _ _ = failwith "TODO"

let get_edges _ _ = failwith "TODO"

let partition_edge _ _ = failwith "TODO"
let path_of _ _ _ = failwith "TODO"
let disjoint_edge _ _ = failwith "TODO"
let traverse_path _ _ _ = failwith "TODO"

let pre_order_nodes _ _ _ _ = failwith "TODO"
let pre_order_edges _ _ _ _ = failwith "TODO"
let post_order_edges _ _ _ _ = failwith "TODO"
let post_order_nodes _ _ _ _ _ = failwith "TODO"

let to_string _ = failwith "TODO"
let print _ = failwith "TODO"
let to_channel _ _ = failwith "TODO"
let of_channel _ = failwith "TODO"
let of_parsed _ = failwith "TODO"
let to_parsed _ = failwith "TODO"

let break _ _ = failwith "TODO"
let join _ _ _ = failwith "TODO"
let reroot _ _ = failwith "TODO"
