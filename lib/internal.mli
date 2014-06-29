(** Internal "Standard" Library. This module is not meant to be exposed in the
   external API. This module is subject to change at any time and is not a
   replacement for standard library extensions like Core or Batteries. This
   module is also used to store modules that need a final home or waiting for a
   proper external library replacement during development. *)

(** {6 Module Inclusions} *)

(** The compatibility module allows backwards support to Stdlib changes. *)
include module type of Compatibility

(** {6 Shorthands} *)

(** [failwithf format] pass a format string to [failwith]. *)
val failwithf : ('a, unit, string, 'b) format4 -> 'a

(** [!$a] forces a lazy value [a]. *)
val ( !$ ) : 'a Lazy.t -> 'a

(** [!$$a] forces a lazy list of values [a]. *)
val ( !$$ ) : 'a Lazy.t list -> 'a list

(** [a--b] Generate a list of integers from [a] to [b] (inclusive). This
    function works on decreasing and increasing ranges of values [a] and [b]. *)
val ( -- ) : int -> int -> int list

(** [some x] A function to wrap [x] in an option type. The function usage allows
    better control with combinators above. *)
val some : 'a -> 'a option

(** [flip f a b] flips the arguments of a binary function *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** [id z] identity function *)
val id : 'a -> 'a

(** [take n l] take at most [n] elements from a list [l] *)
val take : int -> 'a list -> 'a list


(** {6 Floating Point Operations} *)

(** [a =. b] defines an approximately equal function for floating point numbers.
    The tolerance is [epsilon_float] in the standard library. *)
val ( =. ) : float -> float -> bool

(** [is_nan x] Determine if a floating point value [x] is a NAN. *)
val is_nan : float -> bool

(** [proportion a b] A fraction of [a]/[b] as floating point value. *)
val proportion : int -> int -> float


(** {6 Set/Map Implementations} *)

module OrderedInt : Set.OrderedType with type t = int
module OrderedTuple : Set.OrderedType with type t = int * int
module OrdString : Set.OrderedType with type t = string
module UnorderedTuple : Set.OrderedType with type t = int * int

module IntSet : Set.S with type elt = int
module IntMap : Map.S with type key = int

module PairSet : Set.S with type elt = int * int
module PairMap : Map.S with type key = int * int

module UnorderedTupleSet : Set.S with type elt = int * int
module UnorderedTupleMap : Map.S with type key = int * int

module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string

module IntSetSet : Set.S with type elt = IntSet.t
module IntSetMap : Map.S with type key = IntSet.t


(** {6 Random Selection Functions} *)

(** [rand_select n xs] Randomly select [n] elements from a list [xs] using
   resevoir sampling. *)
val random_select : int -> 'a list -> 'a list

(** [random_of_pair a b] Select a random value of two choices [a] and [b]. *)
val random_choice : 'a -> 'a -> 'a

val random_elt_intmap  : 'a IntMap.t -> int * 'a

val random_elt_intset  : IntSet.t -> int

val random_elt_pairset : UnorderedTupleSet.t -> int * int


(** {6 Array/List Functions} *)

val array_fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a


(** {6 BigArray(1,2) Functions on Floats} *)

(** [ba_of_array1] convert an array to bigarray1 for C data-processing. *)
val ba_of_array1 :
  float array -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [ba_of_array2] convert an array to bigarray2 for C data-processing. *)
val ba_of_array2 :
  float array array -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [create_ba1 n] create a bigarray of size [n]. *)
val create_ba1 :
  int -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [create_ba1 n m] create a bigarray of size [n]x[m]. *)
val create_ba2 :
  int -> int -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [ba_to_array1] convert a bigarray1 to an array. *)
val ba_to_array1 : ('a, 'b, 'c) Bigarray.Array1.t -> 'a array

(** [ba_to_array2] convert a bigarray1 to an array array. *)
val ba_to_array2 : ('a, 'b, 'c) Bigarray.Array2.t -> 'a array array


(** {6 Modules} *)

(** The bit-set module needs to be taken out and replaced by an appropriate
    module. Core and Batteries have appropriate replacements. Do they offer the
    speed and flexibility? *)
module BitSet :
  sig
    type t = [`List of int list | `Packed of int | `Set of IntSet.t]
    val empty : t
    val add : int -> t -> t
    val rem : int -> t -> t
    val to_packed : t -> int
    val to_list :  t -> IntSet.elt list
    val to_set : t -> IntSet.t
    val size : t -> int
  end

(** This module should be it's own file or found in an external library. *)
module FileStream :
  sig
    val read_string_matrix : string -> string array array
    val read_float_matrix : string -> float array array
    val read_integer_matrix : string -> int array array
    val read_char_matrix : string -> char array array
  end
