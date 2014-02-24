(** {1 Alignment}
    This module provides the mechanisms to perform data-alignments of any type
    based on a minimal module of details. Included are a number of alignment
    algorithms, their properties are constrained by creating the [mem] type of
    each in particular ways. For example, setting minimum and maximum Ukkonen
    barriers is provided on a specific function of obtaining the [mem] type in
    the Ukkonen module. *)


(** {2 AssignCost}
    Provides the way that assignments and costs of elements are made *)
module type AssignCost =
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

    (** {2 Cost Functions} *)

    (** Add two costs together *)
    val add : model -> cost -> cost -> cost

    (** minimum of two costs *)
    val min : model -> cost -> cost -> cost

    (* Determine the cost and the assignment of the alignment of two elements *)
    val cost : model -> elt -> elt -> cost

    (** Determine a minimal cost of three with added associated data *)
    val min3 : model -> cost * 'a -> cost * 'a -> cost * 'a -> cost * 'a list

    (** Determine a minimal cost of a list *)
    val minN : model -> (cost * 'a) list -> cost * 'a list

    (** {2 Accessing Assignments} *)

    (** Indel associated to the model *)
    val indel : model -> elt

    (** Determine the optimal assignment that minimizes cost between two elts *)
    val assign : model -> elt -> elt -> elt

    (** Determine the optimal cost and assignment for two elements *)
    val median : model -> elt -> elt -> cost * elt

    (** {2 Pretty Printing / IO} *)
    
    val pp_cost : Format.formatter -> cost -> unit
    
    val pp_elt : Format.formatter -> elt -> unit

  end


(** {2 DataVector}
    The storage mechanism for storing the data to be aligned. *)
module type DataVector =
  sig

    (** {2 Types} *) 

    (** The type of an element of the DataVector *)
    type elt

    (** The type of storage for the vector *)
    type t

    (** {2 Functions} *)

    (** [length] returns the length of a DataVector. *)
    val length : t -> int

    (** [get t i] obtain the value of [t] at [i] *)
    val get : t -> int -> elt

    (** [set t i e] sets the value at [i] in [t] to [e] *)
    val set : t -> int -> elt -> t

    val unsafe_get : int -> t -> elt
    val unsafe_set : int -> elt -> t -> t

    (** [of_list l] convert a list of [elt]s to [t] *)
    val of_list : elt list -> t

    (** {2 Pretty Printers} *)

    val pp_t : Format.formatter -> t -> unit

  end

module type Alignment =
  sig
    (** {2 Types} *)

    (** Type of an aligned data-type *)
    type t 

    (** Model for alignment; abstract from AssignCost *)
    type m

    (** Cost of alignment; abstract from AssignCost *)
    type c

    (** Memory storage for alignment; how is alignment stored to obtain
      backtraces, alignment of sequences, and cost. *)
    type mem

    (** {2 Functions} *)

    (** Obtain the aligned data with a model for minimization *)
    val alignments : mem -> m -> t -> t -> t * t

    (** Obtain the aligned data from a previous call to align *)
    val backtrace : mem -> m -> t -> t -> t

    (** Fill a memory structure with the alignment of the sequences. *)
    val align : mem -> m -> t -> t -> c

    (** {2 Pretty Printing and IO} *)

    val mem_to_latex : mem -> string
  end

(** Uses a full alignment matrix to align two sequences. *)
module FullAlignment :
  functor (V : DataVector) ->
    functor (C : AssignCost with type elt = V.elt) ->
  sig
    include Alignment
    val create_mem : m -> t -> t -> mem
  end

