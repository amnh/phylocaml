(** {1 Pretty Printed Latex}
    Module for creating functions to generate LaTeX tables and matrices. *)

(** {2 Types} *)

(** type for abstracting printers of types. In the case of LaTeX, to strings. *)
type 'a pp_l = 'a -> string

(** basic alignment for columns, or entire box. *)
type align = L | C | R

(** {2 Functions generating Array commands} *)

(** Generate a matrix of values for LaTeX compilation. *)
val l_matrix :
  align array -> 'a pp_l -> ('a array array) pp_l

(** Generate a matrix of values with a header row for LaTeX compilation. *)
val l_table :
  align array -> 'a pp_l -> 'b pp_l -> ('a array * 'b array array) pp_l

(** Generate a matrix of values with a labeled column and row of LaTeX *)
val l_datatable :
  align array -> 'a pp_l -> 'b pp_l -> 'c pp_l -> ('a array * 'b array * 'c array array) pp_l
