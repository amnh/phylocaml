(** {2 Type Definitions} *)

(** Basic type to a sequence of data; abstracted by the interface. *)
type s

(** Parse error raised; returns entire sequence, invalid base, and location *)
exception Invalid_Sequence of (string * string * int)

val compare : s -> s -> int

(** {2 Basic Creation Functions} *)

val create : int -> s
(** [create i] create a sequence of capacity [i]. *)

val init : (int -> int) -> int -> s
(** [init f l] Create a sequence initialized by the function [f] about the
    indexes of the sequence up to [l]-1. capacity is equal to [l]. *)

val clone : s -> s
(** [clone s] duplicate the sequence [s]; return a new sequence. This would be
 * the same as [copy s (create (len s))], but more efficient. *)

val missing : Alphabet.t -> s
(** [missing a] is a special case. The sequence is of one element that is a
    [gap] as defined in the alphabet [a]. *)


(** {2 Information Queries} *)

val is_missing : s -> Alphabet.t -> bool
(** [is_missing x a] tests if the sequence [s] is a missing sequence based on
    the encoding of alphabet [a]. *)

val capacity : s -> int
(** [capacity] return the maximum size of the data-type *)

val length : s -> int
(** [length] return the length of the sequence *)

val count : int -> int -> s -> int
val contains_code : int -> s -> bool
val length_without_gap : int -> s -> int


(** {2 Accessors / Setters} *)

val get : s -> int -> int
(** [get s i] get the [i]th element of sequence [s]. *)

val set : s -> int -> int -> unit
(** [set s i v] set the [i]th element of the sequence [s] with value [v]. *)

val prepend : s -> int -> unit
(** [prepend s v] prepend the sequence [s] with value [v]. *)

val prepend_char : s -> int -> s


(** {2  Iterators} *)

val mapi : (int -> int -> int) -> s -> s
val map : (int -> int) -> s -> s
val fold : ('a -> int -> 'a) -> 'a -> s -> 'a
val foldi : ('a -> int -> int -> 'a) -> 'a -> s -> 'a
val foldi_2 : ('a -> int -> int -> int -> 'a) -> 'a -> s -> s -> 'a
val fold_right : ('a -> int -> 'a) -> 'a -> s -> 'a
val fold_righti : ('a -> int -> int -> 'a) -> 'a -> s -> 'a
val iter : (int -> 'a) -> s -> unit


(** {2 Usefule Modifiers.} *)

val remove_gaps : ?prependgap:bool -> s -> int -> s
val reverse_ip : s -> unit
val reverse : s -> s
val concat : s list -> s
val sub : s -> int -> int -> s
val sub_ignore_gap : int -> s -> int -> int -> s * int
val del_first_char : s -> s


(** {2 Higher-Order Creation Functions / Parsers.} *)

val copy : s -> s -> unit
(** [copy s t], copies the contents of [s] into [t]. *)

val split : (int * int) list -> s -> Alphabet.t -> s list

val to_array : s -> int array

val of_array : int array -> s

val of_string : string -> Alphabet.t -> s
(** convert a string of characters to a sequence by an alphabet. the alphabet
 * must be prefix free for the parsing of the string to be successful. *)

val of_list : string list -> Alphabet.t -> s
(** convert a list of characters to a sequence by an alphabet. *)

val to_string : s -> Alphabet.t -> string
(** transform the sequence to a string from the d string *)

val to_formater : s -> Alphabet.t -> string
(** transform the sequence to a formatter encoded string *)

val print : out_channel -> s -> Alphabet.t -> unit
(** sequence encoded by the alphabet to the specified channel. *)

val print_codes : s -> unit
(** print the encoded states, separated by commas. debug function *)


(** {2 Data Oriented Functions} *)

type single = [ `Choose | `Max | `Min | `Random ] 
(** define how to select elements for a single assignment *)

val select_one : ?how:single -> s -> Alphabet.t -> s
(** Select 1 element of each character; reduces a polymorphism to assignment *)

val complement : Alphabet.t -> s -> s
(** Take the complement of a sequence; not reverse. *)

val gap_saturation : s -> Alphabet.t -> float
(** proportion of characters without a gap-state set *)

val poly_saturation : s -> Alphabet.t -> int -> float
(** [poly_saturation s a n] proportion of characters with polymorphic states = [n] *)

val unique_elements : s -> Alphabet.t -> bool
(** [unique_elements] return true if the states in [s] are unique. *)

(*
??? val of_code_arr : int array -> int -> s
??? val cmp_num_all : s -> Alphabet.t -> int
??? val cmp_num_not_gap : s -> Alphabet.t -> int
*)

