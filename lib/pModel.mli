(** {1 Parsimony Model}
    This model contains the elements necessary for the modeling of character
    evolution under parsimony. It takes a general approach and does not concern
    itself with the type of parsimony --sankoff, fitch, dynamic, et cetera-- and
    is mostly then a store for the cost-regime. *)


(** {2 Types} *)

(** Define a transformation-cost-matrix, this is a cost between characters and
    would need to be expanded to a full cost-matrix with median assignments to
    be used. This specification allows a transformation to that state with a
    compressed format for data-transfer and IO. *)
type basic_tcm =
  [ `Const  of int               (** A single cost between differing states. *)
  | `Linear of int * int         (** A cost between states is equal, except indel. *)
  | `TCM    of int array array ] (** General TCM contains a full matrix. *)

(** Type-level enforcement to avoid the affine of affine state for tcm. *)
type tcm =
  [ `Affine of basic_tcm * int
  | basic_tcm ]

(** Define options on defining assignments and other details of the cost-matrix. *)
type options =
  { tie_breaker : [ `First | `Last | `Random ];
    (** The tie-breaker defines how assignments are choosen when the level is
        less than the total number of optimal assignments. *)
    alphabetsize : int;
    (** Define the number of individual states of the alphabet. *)
    level : int;
    (** Defines the level of combinations on large alphabets; 0 is unconstrained *)
  }

(** Define the specification of the model; this is a compressed cost regime that
    is a one to one relationship to a cost-matrix. *)
type s =
  { tcm : tcm;
      a : Alphabet.t;
    opt : options;
  }

(** The model here is a cost-matrix with it's specification. *)
type t =
  {
    spec : s;
    cm : int;
  }


(** {2 Creation Functions} *)

(** Produce a specification that may be cached in a table; we use t
    properties to determine equality, 1) the tcm specification and 2) the
    options, which include alphabet size, level and tie-breaker. Thus two
    alphabets with the same structure will collide and use the same cost-matrix.
    This can be a great savings in space. *)
val of_spec_cached : s -> t

(** Produce a fresh, un-cached [t] from a specification [s]. *)
val of_spec : s -> t
