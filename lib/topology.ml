open Internal

type id = int
type handle = id
type edge = id * id

module IDSet = IntSet
module HandleSet = IntSet
module HandleMap = IntMap
module IDMap = IntMap
module EdgeSet = UnorderedTupleSet
module EdgeMap = UnorderedTupleMap

type break

type join

type jxn = [ `Single of id | `Edge of (id * id) ]

type side_delta =
  { d_nodes : id list; d_edges : edge list; d_handles : id list }

let empty_side =
  { d_nodes = []; d_edges = []; d_handles =[]; }

type _ tdelta =
  { removed : side_delta;
    created : side_delta;
     jxn_of : jxn list; }

type reroot_delta = id list

type topology_delta =
  [`Reroot of edge | `Join of jxn * jxn | `Break of edge] list

let random_edgeset = random_elt_pairset

let random_nodeset = random_elt_intset

let random_nodemap = random_elt_intmap

let random_handleset = random_elt_intset

module IDManager =
  struct

    type t = int * int list

    let empty = 0,[]

    let pop = function
      | y,x::xs -> x, (y, xs)
      | y,[]    -> y, (y+1, [])

    let push i = function
      | y,x when i = (y-1) -> (y-1, x)
      | y,x -> (y, i::x)

    let of_list ids : int * int list =
      let rec holesmax holes max xxs = match xxs with
        | [] -> max,holes
        | x::xs when x = max -> holesmax holes (max+1) xs
        | _ -> holesmax (max::holes) (max+1) xxs
      in
    holesmax [] 0 (List.sort Pervasives.compare ids)

  end

module type NodeComparator = 
  sig
    type t

    val create : id -> t
    val ancestor_1 : id -> t -> t
    val ancestor_2 : id -> t -> t -> t
    val ancestor_n : id -> t list -> t

    val sort : ('a -> t) -> 'a list -> 'a list
    val order : ('a -> t) -> 'a -> 'a -> 'a * 'a
  end

module type S =
  sig

    type node

    type t =
      {   name  : string option;
          nodes : node IDMap.t;
          edges : EdgeSet.t;
        handles : HandleSet.t;
    avail_codes : IDManager.t; }

    module Comparator : NodeComparator

    exception InvalidNodeID of id
    exception InvalidEdge of edge
    exception InvalidHandle of handle

    val empty : t
    val random : id list -> t
    val create : id list -> t
    val disjoint : t -> t

    val compare : t -> t -> int

    val is_edge : id -> id -> t -> bool
    val is_node : id -> t -> bool
    val is_leaf : id -> t -> bool
    val is_single : id -> t -> bool

    val set_name : string -> t -> t
    val get_name : t -> string option

    val random_edge : t -> edge
    val random_node : t -> id
    val random_leaf : t -> id
    val random_single : t -> id

    val get_edge : id -> id -> t -> edge
    val get_node : id -> t -> node
    val get_neighbors : id -> t -> id list
    val get_leaves : t -> id list
    val get_handles : t -> id list
    val get_singles : t -> id list

    val move_handle : id -> t -> t * id list
    val handle_of : id -> t -> handle

    val get_edges : handle -> t -> EdgeSet.t
    val get_all_edges : t -> EdgeSet.t

    val partition_edge : edge -> t -> IntSet.t * IntSet.t * bool
    val path_of : id -> id -> t -> id list
    val disjoint_edge : edge -> t -> bool

    val break : edge -> t -> t * break tdelta
    val join : jxn -> jxn -> t -> t * join tdelta
    val reroot : edge -> t -> t * reroot_delta
 
    val traverse_path :
      ('a -> id -> id -> 'a) -> id list -> t -> 'a -> 'a

    val pre_order_nodes :
      (id option -> id -> 'a -> 'a) -> id -> t -> 'a -> 'a

    val pre_order_edges :
      (edge -> 'a -> 'a) -> edge -> t -> 'a -> 'a

    val post_order_edges :
      (id -> id -> 'a -> 'a) -> (id -> id -> 'a -> 'a -> 'a) -> edge -> t -> 'a -> 'a * 'a

    val to_string : t -> string
    val print : t -> unit

    val of_parsed : EdgeSet.t -> t
    val to_parsed : t -> EdgeSet.t
  end

