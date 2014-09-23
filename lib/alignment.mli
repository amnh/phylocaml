(** This module provides the mechanisms to perform data-alignments of types
    based on a minimal module of details. Included are a number of alignment
    algorithms, their properties are constrained by creating the [mem] type of
    each in particular ways. For example, setting minimum and maximum Ukkonen
    barriers is provided on a specific function of obtaining the [mem] type in
    the Ukkonen module.

    Due to the complexity of alignment, and the size of backtraces, we
    provide a number of functions that may have effects on [mem]. They are
    set-up with the [fill] function which returns unit to denote it's effectful
    nature. *)


(** {2 AssignCostMatrix}
    Provides the way that assignments and costs of elements are made *)
module type AssignCostMatrix =
  sig

    (** {2 Types} *)

    (** The type of costs for the alignment *)
    type cost

    (** The assignment that costs and alignments work from *)
    type elt

    (** The cost-model to define a matrix of costs. *)
    type model

    (** {2 Cost Values} *)

    (** empty/zero cost *)
    val zero : model -> cost

    (** inf or very large cost; never accepted if others are available *)
    val inf : model -> cost

    (** {2 Comparison Functions} *)

    (** less-than *)
    val lt : model -> cost -> cost -> bool

    (** equality *)
    val eq : model -> cost -> cost -> bool

    (** {2 Cost Functions} *)

    (** Add two costs together *)
    val add : model -> cost -> cost -> cost

    (** Determine the cost and the assignment of the alignment of two elements *)
    val cost : model -> elt -> elt -> cost

    (** {2 Accessing Assignments} *)

    (** Indel associated to the model *)
    val indel : model -> elt

    (** Determine the optimal assignment that minimizes cost between two elts *)
    val assign : model -> elt -> elt -> elt

    (** Determine the optimal cost and assignment for two elements *)
    val median : model -> elt -> elt -> cost * elt

  end


(** General alignment module *)
module type BaseAlignment =
  sig
    (** {2 Types} *)

    (** Type of an aligned data-type *)
    type t 

    (** Model for alignment; abstract from AssignCostMatrix *)
    type m

    (** Cost of alignment; abstract from AssignCostMatrix *)
    type c

    (** Memory storage for alignment; how is alignment stored to obtain
        backtraces, alignment of sequences, and cost. This is modified as a
        side-effect to all imperative alignment functions. *)
    type mem


    (** {2 Functional Interface} *)

    (** Return the pairwise cost and median of an alignment. *)
    val median_cost : m -> t -> t -> c * t

    (** Return all the entities and details of an alignment. *)
    val align : m -> t -> t -> c * t * t * t


    (** {2 Imperative Interface} *)

    (** Fill a memory structure with the alignment of the sequences. *)
    val fill : mem -> m -> t -> t -> unit

    (** Obtain the optimal cost of the alignment from a previous call to fill *)
    val cost : mem -> m -> t -> t -> c
    
    (** Obtain the aligned parents of a node from a previous call to fill *)
    val backtrace : mem -> m -> t -> t -> t * t

    (** Obtain the aligned data from a previous call to fill *)
    val alignments : mem -> m -> t -> t -> t * t * t

    (** Obtain the median of the alignment a previous call to fill *)
    val median : mem -> m -> t -> t -> t

    (** {2 Pretty Printing and IO} *)
  end

module type A =
  functor (C : AssignCostMatrix) ->
    BaseAlignment with type t = C.elt array and type m = C.model and type c = C.cost

(** Uses a full alignment matrix to align two sequences. *)
module FullAlignment : functor (C : AssignCostMatrix) ->
  sig
    include BaseAlignment
    val create_mem : m -> t -> t -> mem
  end with type t = C.elt array
       and type m = C.model
       and type c = C.cost

(** Uses the Ukkonen algorithm to limit the number of cells to compute. [k] is
   the initial width of the barrier in addition to the length difference of the
   sequences to initiate the memory. *)
module UkkAlignment : functor (C : AssignCostMatrix) ->
  sig
    include BaseAlignment
    val create_mem : k:int -> m -> t -> t -> mem
  end with type t = C.elt array
       and type m = C.model
       and type c = C.cost

