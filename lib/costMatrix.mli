(** Provides a unifiying entry point for assignment and cost matrix generation
    for alignment and other procedures of minimization. *)

(** A Transformation Cost Matrix defines a set of functions for minimization of
    cost and assignment of character transformations. This is one-sided, in that
    it represents cost of a transformation not the two costs and a median
    assignment. *)
module type TCM =
  sig

    (** t defines transformation cost matrix; contains alphabet *)
    type spec

    (** type for cost; float, int, vector, .. *)
    type cost

    (** the type for assignments *)
    type elt

    (** A way to obtain an alphabet from the specification *)
    val get_alphabet : spec -> Alphabet.t

    (** The zero value for cost; this is parameterized by the model, but will
        more often than not be unnecessary. *)
    val zero : spec -> cost

    (** The infinity or large value; this is parameterized by the model, but
        will more often than not be unnecessary. *)
    val inf : spec -> cost

    (** {2 Ordering Functions} **)

    val lt : spec -> cost -> cost -> bool
    val eq : spec -> cost -> cost -> bool


    (** {2 Functions of Minimization / Assignment} *)

    val add : spec -> cost -> cost -> cost
    val assign : spec -> elt -> elt -> elt list
    val cost : spec -> elt -> elt -> cost
    val assign_cost : spec -> elt -> elt -> cost * elt list
    val compress : spec -> elt list -> elt

    (** {2 I/O} *)

    val l_cost : spec -> cost Ppl.pp_l
    val l_elt : spec -> elt Ppl.pp_l
  end

module type CM =
  sig
    include Alignment.AssignCostMatrix with type elt = Alphabet.code

    type spec

    val create : spec -> model
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
module Make (M : TCM with type elt = Alphabet.code)
          : CM with type spec = M.spec and type cost = M.cost and type elt  = M.elt

(** [MakeLazy] produces a memoized cost-matrix, as elements costs or assignments
    are queried they are added to the table. We also use Hashtbls to limit the
    overall space. This can be used when only a few transformations may be
    needed in the usage of the cost-matrix. *)
module MakeLazy (M : TCM with type elt = Alphabet.code)
          : CM with type spec = M.spec and type cost = M.cost and type elt  = M.elt

