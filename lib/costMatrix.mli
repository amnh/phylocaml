(** Provides a unifiying entry point for assignment and cost matrix generation
    for alignment and other procedures of minimization. *)

(** A Transformation Cost Matrix defines a set of functions for minimization of
    cost and assignment of character transformations. This is one-sided, in that
    it represents cost of a transformation not the two costs and a median
    assignment. *)
module type TCM =
  sig

    (** t defines transformation cost matrix *)
    type t

    (** type for cost; float, int, vector, .. *)
    type cost

    (** the type for assignments *)
    type elt

    (** The zero value for cost; this is parameterized by the model, but will
        more often than not be unnecessary. *)
    val zero : t -> cost

    (** The infinity or large value; this is parameterized by the model, but
        will more often than not be unnecessary. *)
    val inf : t -> cost

    (** {2 Ordering Functions} **)

    val lt : t -> cost -> cost -> bool
    val eq : t -> cost -> cost -> bool


    (** {2 Functions of Minimization} *)

    val add : t -> cost -> cost -> cost
    val assign : t -> elt -> elt -> elt list
    val cost : t -> elt -> elt -> cost
    val assign_cost : t -> elt -> elt -> cost * elt list

    (** {2 I/O} *)

    val l_cost : t -> cost Ppl.pp_l
    val l_elt : t -> elt Ppl.pp_l
  end

module type CM =
  sig
    include Alignment.AssignCostMatrix with type elt = Alphabet.code

    type spec

    val create : spec -> Alphabet.t -> model
  end

module Error :
  sig
    type t =
        [ `Alphabet_Does_Not_Contain_Gap of Alphabet.t
        | `Alphabet_Does_Not_Support_Cost_Matrix of Alphabet.t ]

    val to_string : t -> string
  end

exception Error of Error.t

(** [Make] produces cost matrices that are fully realized as they are created.
    This can be time consuming, but quicker if they are used often enough. *)
module Make : functor (M : TCM with type elt = Alphabet.code) -> CM

(** [MakeLazy] produces a memoized cost-matrix, as elements costs or assignments
    are queried they are added to the table. We also use Hashtbls to limit the
    overall space. This can be used when only a few transformations may be
    needed in the usage of the cost-matrix. *)
module MakeLazy : functor (M : TCM with type elt = Alphabet.code) -> CM

