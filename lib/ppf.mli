(** {1 Pretty Printing Helper Functions}
    Defines a set of functions for helping with pretty-printing data-types.
    All types in Phylocaml have associated `pp_` functions for printing their
    values in the debugger, or generally. *)

(** {2 Types} *)

(** Definition for formatter functions for easily understandable type definitions *)
type 'a pp_f =
  Format.formatter -> 'a -> unit

(** {2 Generators}
    Composable PP functions for array based data-structures. *)

(** Format an array of data with separator; break hints between elements. *)
val pp_array :
  ?hsep : string -> 'a pp_f -> ('a array) pp_f

(** Format a matrix of values; right justified *)
val pp_matrix :
  ?hsep : string -> 'a pp_f -> ('a array array) pp_f

(** Format a matrix with a header row; right-justified *)
val pp_table :
  ?hsep : string -> 'a pp_f -> 'b pp_f -> ('a array * 'b array array) pp_f

