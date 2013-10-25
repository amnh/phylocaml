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

(** Type-level enforcement of non affine of affine state for tcm. *)
type tcm =
  [ `Affine of basic_tcm * int
  | basic_tcm ]

(** Define options on defining assignments and other details of the cost-matrix. *)
type options =
  { tie_breaker : [ `First | `Last | `Random ];
    (** The tie-breaker defines how assignments are choosen when the level is
        less than the total number of optimal assignments. *)
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
