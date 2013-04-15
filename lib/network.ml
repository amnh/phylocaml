open Phylocaml_pervasives

open Topology

module NodeComparator = 
    struct
        type t =
            | Leaf of id
            | Node of t * t

        let create x = Leaf x

        let ancestor_1 i x = x
        let ancestor_2 i x y = Node (x,y)
        let ancestor_n i xs = assert false

        let order x y = failwith "TODO"
        let sort xs = failwith "TODO"
    end

type node =
    | Leaf of id * id
    | Interior of id * id * id * id
    | Reticulate of id * id * id * id

type t =
    {   name  : string option;
        nodes : node IDMap.t;
        edges : EdgeSet.t;
      handles : HandleSet.t;
  avail_codes : int * int list;
    }

exception InvalidNodeID of id
exception InvalidEdge of edge
exception InvalidHandle of handle

let empty =
    {   name = None;
        nodes = IDMap.empty;
        edges = EdgeSet.empty;
        handles = HandleSet.empty;
        avail_codes = 0,[];
    }

let next_code t : int * t = match t.avail_codes with
    | y,x::xs -> x, {t with avail_codes =y,xs; }
    | y,[]    -> y, {t with avail_codes =y+1,[]; }

let replace_code i t : t = match t.avail_codes with
    | y,x when i = (y-1) -> {t with avail_codes = y-1,x; }
    | y,x -> {t with avail_codes = y,i::x; }

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
let handle_of _ _ = failwith "TODO"

let partition_edge _ _ = failwith "TODO"
let path_of _ _ _ = failwith "TODO"
let disjoint_edge _ _ = failwith "TODO"

let break _ _ = failwith "TODO"
let join _ _ _ = failwith "TODO"
let reroot _ _ = failwith "TODO"

type 'a fuse_location = id * t

type 'a fuse_locations = 'a fuse_location list

let fuse_locations _ _ = failwith "TODO"
let fuse_all_locations _ = failwith "TODO"
let fuse _ _ = failwith "TODO"

let pre_order_nodes _ _ _ _ = failwith "TODO"
let pre_order_edges _ _ _ _ = failwith "TODO"
let pre_order_edges_root _ _ _ _ = failwith "TODO"
let post_order_edges _ _ _ _ = failwith "TODO"
let post_order_nodes _ _ _ _ _ = failwith "TODO"

let to_string _ = failwith "TODO"
let print _ = failwith "TODO"
let to_channel _ _ = failwith "TODO"
let of_channel _ = failwith "TODO"
let of_parsed _ = failwith "TODO"
let to_parsed _ = failwith "TODO"

type side_delta =
    [ `Reroot of edge | `Join of jxn * jxn ]

type delta = {
    left_delta  : side_delta option * handle;
    right_delta : side_delta option * handle;
}

let nni _ _ _ = failwith "TODO"
let spr _ _ _ = failwith "TODO"
let tbr _ _ _ = failwith "TODO"
let diagnose_delta _ _ = failwith "TODO"

type deferred_prev = int

let deferred_nni _ _ _ _ = failwith "TODO"
let deferred_spr _ _ _ _ = failwith "TODO"
let deferred_tbr _ _ _ _ = failwith "TODO"

let deferred_partition _ _ _ _ _ = failwith "TODO"
