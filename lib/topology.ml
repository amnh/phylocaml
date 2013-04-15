open Phylocaml_pervasives

type id = int
type handle = id
type edge = id * id

module IDSet = IntSet
module HandleSet = IntSet
module EdgeSet = PairSet
module IDMap = IntMap
module EdgeMap = PairMap

type jxn = [ `Single of id | `Edge of id * id ]

type general_delta =
  { reroot : id list;
    removed_nodes : id list;
    removed_edges : edge list;
    added_nodes : id list;
    added_edges : edge list; }

type reroot_delta = general_delta
type break_delta  = general_delta
type join_delta   = general_delta

type delta = unit (** TODO **)

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
    avail_codes : int * int list; }

    exception InvalidNodeID of id
    exception InvalidEdge of edge
    exception InvalidHandle of handle

    val empty : t
    val random : id list -> t
    val disjoint : id list -> t

    val is_edge : id -> id -> t -> bool
    val is_node : id -> t -> bool
    val is_leaf : id -> t -> bool
    val is_single : id -> t -> bool

    val random_edge : t -> edge
    val random_node : t -> id
    val random_leaf : t -> id
    val random_single : t -> id

    val get_edge : id -> id -> t -> edge
    val get_node : id -> t -> node
    val get_neighbors : id -> t -> id list
    val get_leaves : t -> IDSet.t
    val handle_of : id -> t -> handle

    val get_edges : handle -> t -> EdgeSet.t

    val partition_edge : t -> edge -> IntSet.t * IntSet.t * bool
    val path_of : t -> id -> id -> id list
    val disjoint_edge : t -> edge -> bool

    val break : edge -> t -> t * break_delta
    val join : jxn -> jxn -> t -> t * join_delta
    val reroot : edge -> t -> t * reroot_delta
 
    val pre_order_nodes :
      (id option -> id -> 'a -> 'a) -> t -> 'a -> id -> 'a
    val pre_order_edges :
      (edge -> 'a -> 'a) -> t -> 'a -> edge -> 'a
    val pre_order_edges_root :
      (edge -> 'a -> 'a) -> (edge -> 'a -> 'a) -> edge -> t -> 'a -> 'a
    val post_order_edges :
      (id -> id -> 'a -> 'a) -> (id -> id -> 'a -> 'a -> 'a) -> edge -> t -> 'a -> 'a * 'a

    val to_string : t -> string
    val print : t -> unit

    val of_parsed : EdgeSet.t -> t
    val to_parsed : t -> EdgeSet.t

    type 'a fuse_location
    type 'a fuse_locations = 'a fuse_location list
    val fuse_locations : ('a * t) list -> ('a * t) -> 'a fuse_locations
    val fuse_all_locations : ('a * t) list -> 'a fuse_locations
    val fuse : 'a fuse_location -> 'a fuse_location -> t
  end
