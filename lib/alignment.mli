(** This module provides the mechanisms to perform data-alignments of any type
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

    (** {2 Comparison Functions} *)

    (** less-than *)
    val lt : model -> cost -> cost -> bool

    (** greater-than *)
    val gt : model -> cost -> cost -> bool

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

    (** {2 Pretty Printing / IO} *)
   
    val l_cost : cost Ppl.pp_l
    val l_elt : elt Ppl.pp_l

    val pp_cost : cost Ppf.pp_f
    val pp_elt : elt Ppf.pp_f
  end


(** General alignment module *)
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

    (** Return the pairwise cost and median of an alignment. *)
    val aligned : m -> t -> t -> c * t

    (** {2 Pretty Printing and IO} *)

    (** Prints the memory, and thus the internal states and alignment directions 
        in latex consumable format. *)
    val l_mem : mem Ppl.pp_l

    (** Print the memory, and thus the interal states and alignment directions
        using the pretty-printer formatter module in OCaml. *)
    val pp_mem : mem Ppf.pp_f
  end


(** Uses a full alignment matrix to align two sequences. *)
module FullAlignment : functor (C : AssignCost) ->
  sig
    include Alignment
    val create_mem : m -> t -> t -> mem
  end

(** Uses the Ukkonen algorithm to limit the number of cells to compute. [k] is
   the initial width of the barrier in addition to the length difference of the
   sequences to initiate the memory. *)
module UkkAlignment : functor (C : AssignCost) ->
  sig
    include Alignment
    val create_mem : k:int -> m -> t -> t -> mem
  end

(*
(** Limit the number of indel events in the alignment to [k]. *)
module MaxIndelAlignment : functor (C : AssignCost) ->
  sig
    include Alignment
    val create_mem : k:int -> t -> t -> mem
  end

(** Limit the maximum length of the number of indels to include in the
   respective sequences. *)
module MaxIndelLengthAlignment : functor (C : AssignCost) ->
  sig
    include Alignment
    val create_mem : g_indel:int -> f_indel:int -> g:t -> f:t -> mem
  end

(** Limit the maximum number of indel run events in the alignment. *)
module MaxIndelRunsAlignment : functor (C : AssignCost) ->
  sig
    include Alignment
    val create_mem : k:int -> t -> t -> mem
  end
*)
