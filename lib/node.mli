
(** Define how directions in a topology can be passed around. They can point TO
    a list of parents or FROM a list of children. *)
type dir =
    [ `Children of Topology.id list | `Parent of Topology.id list ] option

(** Module that defines a node. To avoid restricting neighborhoods of nodes in a
    topology, the interface is geared toward the common unrooted binary tree,
    but higher topologies are allowed with _n functions. *)
module type R =
  sig
    (** {2 Types} *)

    (** The encapsulated data that the node contains. *)
    type nd

    (** The internal type for nodes. *)
    type n

    (** {2 Basic Queries on Data} *)

    (** [of_data i x] Create a node with id [i] and data presented in a
        [code]->[data] map [x]. *)
    val of_data : Topology.id -> nd Internal.IntMap.t -> n

    (** [height dir n] the height of the rooted-tree from position oriented at
        [dir]. This is accumulated internally from a diagnosis, (thus, O(1)). *)
    val height : dir -> n -> int

    (** [cardinal n] the sum of all elements in the contained data-sets. *)
    val cardinal : n -> int

    (** [filter_codes c n] removes codes contained in [c] from [n], the result
        may be empty, which can be checked from [is_empty n] function. *)
    val filter_codes : Internal.IntSet.t -> n -> n option

    (** [compare a b] compare [a] and [b] nodes data by comparing the data. *)
    val compare : n -> n -> int

    (** recode the character codes contained in the data-sets. *)
    val recode : (int -> int) -> n -> n

    (** Return the set of character codes of the data-sets. *)
    val get_codes : n -> Internal.IntSet.t

    (** {2 Phylogenetic Functions} *)
      
    (** [get_preliminary d x n] Return the preliminary assignments from
        direction [d] of data [x] in node [n]. This is the heuristic downpass
        initial assignments for the analysis. *)
    val get_preliminary : dir -> int -> n -> nd

    (** [get_adjusted x n] return the adjusted assignment of data [x] from node
        [n]. These may not single assignments. *)
    val get_adjusted : int -> n -> nd

    (** [to_single x a b c] choose a single assignment of [x] from neighbors [a].
        [b] and [c]. *)
    val to_single : n option -> n -> n -> n -> n

    (** [final_states x a b c] finalize the assignment of [x] from neighbors [a].
        [b] and [c]. *)
    val final_states : n -> n -> n -> n -> n


    (** {2 Distance Functions} *)
      
    (** [distance_1 a b] is the cost of joining [a] and [b]. *)
    val distance_1 : n -> n -> float

    (** [distance_2 a b c] The distance of joining [c] between [a] and [b]. *)
    val distance_2 : n -> n -> n -> float

    (** {2 Median Functions} *)

    (** Calculate the ancestor of a single element. *)
    val median_1 : Topology.id -> n option -> n -> n

    (** Calculate the ancestor of two node elements. *)
    val median_2 : Topology.id -> n option -> n -> n -> n
    
    (** Calculate the ancestor of [n] node elements. *)
    val median_n : Topology.id -> n option -> n list -> n

    (** {2 Readjust Functions} *)

    (** [readjust_3 ?prelim set a b c d] Optimize the assignment of a central
        node [a] with three neighbors [b], [c], [d]. Only the characters with
        the set [s] are optimized if provided else all of them are optimized.
        [?prelim] is used if the preliminary or final assignments are optimized.
        Returns a set of characters that modifications had been made *)
    val readjust_3 : ?prelim:bool -> Internal.IntSet.t option -> n -> n -> n -> n -> n * Internal.IntSet.t
    
    (** The same as above, but abstracted for [n] neighbors. *)
    val readjust_n : ?prelim:bool -> Internal.IntSet.t option -> n -> n list -> n * Internal.IntSet.t

    (** {2 Uppass Functions} *)

    val uppass_heuristic_internal_3 : n -> n -> n -> n -> n

    val uppass_heuristic_internal_n : n -> n list -> n

    val uppass_heuristic_leaf : n -> n -> n

    val uppass_heuristic_root : n -> n -> n -> n

    (** {2 Cost Functions} *)

    val root_cost : dir -> n -> float
    val node_cost : dir -> n -> float

    (** {2 I/O and Reporting Functions} *)

    val to_string : n -> string
    val to_xml : out_channel -> n -> unit
  end


(** {2 Node Implementations} *)


(** Type for implementation of [R] that restricts the data to [NodeData.S]. *)
module type S = functor (NodeData : NodeData.S) -> R with type nd = NodeData.t

(** Implementation of NodeData with one direction in the assignment of nodes.
    This is used for rooted trees, or when diagnosis in different directions does
    not result in variation of costs. *)
module Make1D   : functor (Ordering : Topology.NodeComparator) -> S

(** An encapsulation of Make1D where nodes are lazy. *)
module MakeLazy : functor (Ordering : Topology.NodeComparator) -> S

(** A abstraction of NodeData where each internal node has two children and a
    parent and each combination of such is represented as a [MakeLazy.n]. *)
module Make3D   : functor (Ordering : Topology.NodeComparator) -> S

(** Further abstraction of [Make3D.n] with no limit to number of children or
    parents. This allows unrestricted analysis for general topologies, but the
    implementation of the functions behind the use are slow. *)
module MakeND   : functor (Ordering : Topology.NodeComparator) -> S
