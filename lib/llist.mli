(** Lazy List API
 
    This module provides access to lazy-lists. The first element of the list is
    not lazy, thus some computation is made in creating the type. *)


(** {2 Type Definition} *)

type 'a llist = Nil
              | Cons of 'a * 'a llist Lazy.t
(** An Empty (Nil) lazy list, and a cons cell containing some value and a lazy
    recursive list element. *)


(** {2 Basic List Comprehension} *)

val hd : 'a llist -> 'a option
(** Returns the head of the list, this value is already forced so no computation
    is being done. *)

val tl : 'a llist -> 'a llist option
(** Returns the tail of the list. The new head of the list is forced. *)

val cons : 'a -> 'a llist -> 'a llist
(** Append an element to the front of the list. *)

val is_nil : 'a llist -> bool
(** Returns true if the list is empty. It is stressed this function should not
   typically be used as list comprehension should take care of termination *)

val take : int -> 'a llist -> 'a list
(** Returns the next [int] elements of the lazt list. *)

val filter : ('a -> bool) -> 'a llist -> 'a llist
(** Filter a list by values that match the predicate. *)


val until : ('a -> bool) -> 'a llist -> 'a 
(** Continually accesses elements of the list and returns the first matching the
 * criteria defined in the predicate funciton. *)

(** {2 Functions of 'a -> 'b} *)

val map : ('a -> 'b) -> 'a llist -> 'b llist
(** Maps a function over the lazy list. *)

val thread : ('a -> 'b -> 'a * 'c) -> 'a -> 'b llist -> 'c llist
(** A function that manages a state ['a] through the computation of the map of
    the function over the list. *)


(** {2 Functions in generating lazy-lists} *)

val of_fn_i : (int -> 'a) -> int -> 'a llist
(** Create an integer indexed list *)

val iterate : ('a -> 'a) -> 'a -> 'a llist
(** Create a lazy list of an iterative function where the next element is a
   function of the previous. *)

val from : int -> int llist
(** Create an infinite list of integers from [int]. *)

val const : 'a -> 'a llist
(** Return an infinite list of a constant value *)

val singleton : 'a -> 'a llist
(** Return a list of a single element. *)


(** {2 Functions for Combining lazy-lists} *)

val weave : 'a llist -> 'a llist -> 'a llist
(** Combines two lists alternating elements of each. *)

val combine : 'a llist -> 'b llist -> ('a * 'b) llist
(** Function to combine to lists into a tuple *)

val append : 'a llist -> 'a llist -> 'a llist
(** Append one list onto the other; same as the infix [++]. *)

val ( ++ ) : 'a llist -> 'a llist -> 'a llist
(** alias. append *)

val concat : 'a llist llist -> 'a llist
(** Concatenate lazy-lists from a lazy-lists. *)


(** {2 Folding } *)
val fold : ('a -> 'b -> 'b) -> 'a llist -> 'b -> 'b


(** {2 Conversion Functions} *)

val of_stream : 'a Stream.t -> 'a llist
(** Produce a lazy-list from an OCaml stream *)

val of_list : 'a list -> 'a llist
(** Produce a lazy-list from a list *)


(** {2 Monadic Operators} *)

val return : 'a -> 'a llist
(** alias. singleton *)

val bind : 'a llist -> ('a -> 'b llist) -> 'b llist
(** Monadic bind operator for lazy-lists. *)
