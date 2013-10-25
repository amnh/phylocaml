(** {1 Node}
    A definition of a node in a topology. This is not the end-all definition,
    but provides the functions necessary to perform analysis and diagnosis. [S]
    is a general signature for other modules to interface with, and [R] is a
    type restricted definition with a [Model] and [NodeData] with the same model
    attached. This allows for models to be passed to the medians directly
    allowing for extra data to be applied -- branch lengths, complexity
    machines, et cetera. *)

(** Define how directions in a topology can be passed around. They can point TO
    a list of parents or FROM a list of children. No direction implies that
    there is one direction and the information is superfluous. *)
type dir =
    [ `Children of Topology.id list | `Parent of Topology.id list ] option


(** {2 Module Definition} *)

(** Module that defines a node. To avoid restricting neighborhoods of nodes in a
    topology, the interface is geared toward the common unrooted binary tree,
    but higher topologies are allowed with _n functions. *)
module type S =
  sig
    (** {3 Types} *)

    (** The encapsulated data that the node contains. *)
    type nd

    (** The internal type for nodes. *)
    type n

    (** The internal type for a root node. *)
    type r

    (** The type to define a model; or extra information/data to nodes. *)
    type m


    (** {3 Basic Queries / Conversions on NodeData} *)

    (** [of_data i x] Create a node with id [i] and data presented in a
        [code]->[data] map [x]. *)
    val of_data : Topology.id -> nd Internal.IntMap.t -> n

    (** [height dir n] the height of the rooted-tree from position oriented at
        [dir]. This is accumulated internally from a diagnosis, (thus, O(1)). *)
    val height : dir -> n -> int

    (** [cardinal n] the sum of all elements in the contained data-sets. *)
    val cardinal : n -> int

    (** [compare a b] compare [a] and [b] nodes data by comparing the data. *)
    val compare : n -> n -> int

    (** [filter_codes c n] removes codes contained in [c] from [n], the result
        may be empty, which can be checked from [is_empty n] function. *)
    val filter_codes : Internal.IntSet.t -> n -> n

    (** recode the character codes contained in the data-sets. *)
    val recode : (int -> int) -> n -> n

    (** Return the set of character codes of the data-sets. *)
    val get_codes : n -> Internal.IntSet.t


    (** {3 Optaining States} *)
      
    (** [get_preliminary d x n] Return the preliminary assignments from
        direction [d] of data [x] in node [n]. This is the heuristic downpass
        initial assignments for the analysis. *)
    val get_preliminary : dir -> int -> n -> nd

    (** [get_adjusted x n] return the adjusted assignment of data [x] from node
        [n]. These may not single assignments. *)
    val get_adjusted : int -> n -> nd


    (** {3 Distance Functions} *)
      
    (** [distance_1 a b] is the cost of joining [a] and [b]. *)
    val distance_1 : m -> n -> n -> float

    (** [distance_2 a b c] The distance of joining [c] between [a] and [b]. *)
    val distance_2 : m -> n -> n -> n -> float


    (** {3 Median Functions} *)

    (** Calculate the ancestor of a single element. *)
    val median_1 : m -> Topology.id -> n option -> n -> n

    (** Calculate the ancestor of two node elements. *)
    val median_2 : m -> Topology.id -> n option -> n -> n -> n
    
    (** Calculate the ancestor of three node elements. *)
    val median_3 : m -> Topology.id -> n option -> n -> n -> n -> n

    (** Calculate the ancestor of [n] node elements. *)
    val median_n : m -> Topology.id -> n option -> n list -> n

    (** [to_single x a b c] choose a single assignment of [x] from neighbors [a].
        [b] and [c]. *)
    val to_single : m -> n option -> n -> n -> n -> n

    (** [final_states x a b c] finalize the assignment of [x] from neighbors [a].
        [b] and [c]. *)
    val final_states : m -> n -> n -> n -> n -> n


    (** {3 Readjust Functions} *)

    (** [readjust_3 ?prelim set a b c d] Optimize the assignment of a central
        node [a] with three neighbors [b], [c], [d]. Only the characters with
        the set [s] are optimized if provided else all of them are optimized.
        [?prelim] is used if the preliminary or final assignments are optimized.
        Returns a set of characters that modifications had been made *)
    val readjust_3 : ?prelim:bool -> m -> Internal.IntSet.t option -> n -> n -> n -> n -> n * Internal.IntSet.t
    
    (** The same as above, but abstracted for [n] neighbors. *)
    val readjust_n : ?prelim:bool -> m -> Internal.IntSet.t option -> n -> n list -> n * Internal.IntSet.t


    (** {3 Uppass Functions} *)

    val uppass_heuristic_internal_3 : m -> n -> n -> n -> n -> n

    val uppass_heuristic_internal_n : m -> n -> n list -> n

    val uppass_heuristic_leaf : m -> n -> n -> n

    val uppass_heuristic_root : m -> n -> n -> n -> n


    (** {3 Cost Functions} *)

    val root_cost : dir -> n -> float
    val node_cost : dir -> n -> float

    (** {3 I/O and Reporting Functions} *)

    val to_string : n -> string
    val to_xml : out_channel -> n -> unit
  end


(** {2 Node Implementations} *)

(** Type for implementation of [S] that restricts the data to [NodeData.S]. *)
module type R =
  functor (Model    : Model.S) ->
  functor (NodeData : NodeData.S with type m = Model.t) ->
    S with type nd = NodeData.t and type m = Model.t
                              
(** Implementation of NodeData with one direction in the assignment of nodes.
    This is used for rooted trees, or when diagnosis in different directions does
    not result in variation of costs. *)
module Make1D   : functor (Ordering : Topology.NodeComparator) -> R

(** An encapsulation of Make1D where nodes are lazy. *)
module MakeLazy : functor (Ordering : Topology.NodeComparator) -> R

(** A abstraction of NodeData where each internal node has two children and a
    parent and each combination of such is represented as a [MakeLazy.n]. *)
module Make3D   : functor (Ordering : Topology.NodeComparator) -> R

(** Further abstraction of [Make3D.n] with no limit to number of children or
    parents. This allows unrestricted analysis for general topologies, but the
    implementation of the functions behind the use are slow. *)
module MakeND   : functor (Ordering : Topology.NodeComparator) -> R
