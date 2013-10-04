(** Tree

Defines the topology of a binary unrooted tree and the functions to manipulate
the topology. *)

(** A NodeComparator for a tree, for traversal ordering. *)
module NodeComparator : Topology.NodeComparator

(** {2 Types} *)

(** Type for a node can be a leaf, a singleton, or an interior vertex. *)
type node =
  | Leaf of Topology.id * Topology.id
  | Interior of Topology.id * Topology.id * Topology.id * Topology.id
  | Single of Topology.id

(** Complete definition of a tree. *)
type t = {
  name : string option;           (** Optional naming of the tree -for I/O. *)
  nodes : node Topology.IDMap.t;  (** Map of a node id to the node type. *)
  edges : Topology.EdgeSet.t;     (** Set of edges in the topology. *)
  handles : Topology.HandleSet.t; (** Set of handles in the Topology. *)
  avail_codes : int * int list;   (** Codes for the creation of new nodes. *)
}


(** {2 Exceptions} *)

(** Raised when a node is attempted to be accessed but not found. *)
exception InvalidNodeID of Topology.id

(** Raised if an edge is attempted to be accessed but not found. *)
exception InvalidEdge of Topology.edge

(** Raised if handle is attempted to the accessed but not found. *)
exception InvalidHandle of Topology.handle


(** {2 Tree Creation Functions} *)

(** Create an empty topology. *)
val empty : t

(** Create a topology of singletons from a list of node ids. *)
val create : Topology.id list -> t

(** Convert a tree to a topology of singleton nodes. *)
val disjoint : t -> t


(** {2 Accessors / Queries on Tree Topologies} *)

(** Determine if an edge is present within the topology. *)
val is_edge : Topology.id -> Topology.id -> t -> bool

(** Determine if a node id is present within the topology, *)
val is_node : Topology.id -> t -> bool

(** Return if the node id is a leaf. Includes single nodes. *)
val is_leaf : Topology.IDMap.key -> t -> bool

(** Return if the node id is a single node. *)
val is_single : Topology.IDMap.key -> t -> bool

(** Determine if a node id is a handle of the topology. *)
val is_handle : Topology.HandleSet.elt -> t -> bool

(** Return a Set of leaves of the topology. *)
val get_leaves : t -> Topology.IDSet.t

(** Return the single nodes of the topology. *)
val get_singles : t -> Topology.IDSet.t

(** Of a pair of node ids, return an edge. *)
val get_edge : Topology.id -> Topology.id -> t -> Topology.edge

(** set the name for the tree *)
val set_name : string -> t -> t

(** return the name of the tree *)
val get_name : t -> string option

(** Return a set of edges attached to the handle of a topology. *)
val get_edges : Topology.handle -> t -> Topology.EdgeSet.t

(** Return a set of all edges in the tree *)
val get_all_edges : t -> Topology.EdgeSet.t

(** Return the node type of the node id. *)
val get_node : Topology.id -> t -> node

(** Return a list of neighbors of a node. Empty if single. *)
val get_neighbors : Topology.id -> t -> Topology.id list

(** Compare two trees. *)
val compare : t -> t -> bool

(** Move the handle of a tree to a location, return the path between nodes. *)
val move_handle : Topology.id -> t -> t * Topology.id list

(** Determine the handle of a node. *)
val handle_of : Topology.id -> t -> Topology.handle

(** Return the path of one node to another as a list of states. *)
val path_of : Topology.id -> Topology.id -> t -> Topology.id list

(** Determine if two nodes are on separate handles of the topology. *)
val disjoint_edge : Topology.id -> Topology.id -> bool


(** {2 Random Accessors} *)

(** Return a random edge from the topology. *)
val random_edge : t -> Topology.edge

(** Return a random node from the topology. *)
val random_node : t -> Topology.id

(** Return a random leaf from the topology; includes single nodes. *)
val random_leaf : t -> Topology.id

(** Return a random single node from the topology. *)
val random_single : t -> Topology.id

(** Generate a random tree from a list of node ids. *)
val random : Topology.id list -> t


(** {2 Traversal Functions} *)

(** [pre_order_nodes f x t a] perform a pre-order node traversal on [t] starting
    at node [x]. [f] takes the previous node as an option, the current node and
    the accumulator from [a]. *)
val pre_order_nodes :
  (Topology.id option -> Topology.id -> 'a -> 'a) ->
    Topology.id -> t -> 'a -> 'a

(** [pre_order_edges f e t a] performa pre-order traversal on the topology
    starting on the edge [e] and accumulating [a]. *)
val pre_order_edges :
  (Topology.edge -> 'a -> 'a) -> Topology.edge -> t -> 'a -> 'a

val pre_order_edges_root : 'a -> 'b -> 'c -> 'd -> 'e -> 'f

(** [post_order_edges f g e t a] perform a post-order traversal applying [f] to
    leaf nodes with the first argument being the parent and next being the
    current node, and [g] to internal nodes where the two accumulators come from
    the traversal to each of the children. *)
val post_order_edges :
  (Topology.id -> Topology.id -> 'a -> 'a) ->
    (Topology.id -> Topology.id -> 'a -> 'a -> 'a) ->
      Topology.edge -> t -> 'a -> 'a * 'a

(** [partition_edge e t] Return the leaf partitions determined by the edge [e].
    The boolean return value is used if the sets are disjoint in elements. This
    is always the case in Tree topologies. *)
val partition_edge :
  Topology.edge -> t -> Topology.IDSet.t * Topology.IDSet.t * bool


(** {2 Topological Functions} *)

(** Break an edge of a topology and return a new tree and a delta based on
    the new/removed nodes in the construction of the break. The number of
    handles will increase by one, always. *)
val break : Topology.id * Topology.id -> t -> t * Topology.break_delta

(** Join two jxn points of a topology and return a new tree and a delta based on
    the new/removed nodes in the construction of the join. The number of handles
    will decrease by one, always. *)
val join : Topology.jxn -> Topology.jxn -> t -> t * Topology.join_delta

(** Reroot a topology. This is relavent in rooted trees, only returns exact
    topology in this case. *)
val reroot : Topology.id -> t -> t


(** {2 Fusing Functions} *)

type 'a fuse_location = 'a * Topology.id * t
type 'a fuse_locations = 'a fuse_location list
val fuse_locations : 'a -> 'b -> 'c
val fuse_all_locations : 'a -> 'b
val fuse : 'a -> 'b -> 'c


(** {2 I/O Functions} *)

type 'a parsed = [ `Node of 'a * 'a parsed list | `Leaf of 'a ]
type data = unit (* TODO: float? string? ...? *)

val to_string : t -> string
val of_parsed : data parsed -> t
val to_parsed : t -> data parsed

val debug_print : t -> out_channel -> unit


(** {2 Math Functions} 
    Useful mathematical and combinatorial functions on binary unrooted trees. We
  use floating point results where exponential explosions can result. *)

(** Calculate the number of edges of a binary unrooted tree with [n] leaves.
    The exact formula is, 2*n-3. Add an additional edge for rooted trees. *)
val num_edges : int -> int

(** Calculate the number of nodes in a binary unrooted tree with [n] leaves. The
    number of nodes are 2*n-2. *)
val num_nodes : int -> int

(** Calculate the number of binary unrooted trees that can be produced from with
    [n] leaves. (2n-5)!! *)
val num_trees : int -> float
