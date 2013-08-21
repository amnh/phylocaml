(** Bitvector is a module that holds a set of bit-sets that can represent any
 * type of discrete data. The operations are vectorized for speed. *)

module type BV = sig

  (** {2 Types} *)

  (** A set of bit-sets used to encode large amounts of data. *)
  type t

  (** Type of individual element that stores bits for the vector. *)
  type elt = int


  (** {2 Creation} *)

  (** [create w n] Create a new bitvector of all unset bits of length [n] and
      maximum width [w]. The [w] option is asserted in C code and used for
      [elt_int] function to determine if an OCaml int can be returned. *)
  val create : int -> int -> t 

  (** [copy t] copy vector [t] to a new vector with unique code. *)
  val copy : t -> t 

  (** [of_array w r] convert an array of elments [r] to a bit-vector. *)
  val of_array : int -> elt array -> t 


  (** {2 Accessors} *)

  (** [code t] The unique code generated internally for the vector. *)
  val code : t -> elt 

  (** [compare s t] Compare two vectors [s] and [t], first by size, then by the
      first difference between elements; ie the first vector with a set-bit is
      considered 'larger' and returns 1. *)
  val compare : t -> t -> elt 

  (** [cardinal t] Number of elements in the bit-vector [t]. Not to be confused
      with eltcount, which is the number of set bits in a bitvector element, or
      popcount which is the total number of set bits in the entire bitvector. *)
  val cardinal : t -> int


  (** {2 Element Manipulation} These functions may not (and probably not) are
      vectorized, they may also be unsafe. *)

  (** [set_elt t i n] Set element [i] of bitvector [t] to a given value [n]. *)
  val set_elt : t -> int -> elt -> unit 

  (** [set_bit t i n] Set bit [n] in element [i] of bitvector [t] to 1. *)
  val set_bit : t -> int -> int -> unit 

  (** [elt_int t i] return an element as an integer; if the data cannot fit in
      an OCaml value we return [None]. In which case, [elt_states] can be used
      to obtain a list of set bits. *)
  val elt_int : t -> int -> int option

  (** [elt_states t i] return element [i] of [t] as a list of set bits *)
  val elt_states : t -> int -> int list 

  
  (** {2 Logical Operations} *)

  (** [union s t] return the bitwise union of [s] and [t]. *)
  val union : t -> t -> t 

  (** [inter s t] return the bitwise intersection of [s] and [t]. *)
  val inter : t -> t -> t 

  (** [saturation t i] return the number of sites in [t] with bits of [i] set. *)
  val saturation : t -> elt -> int 

  (** [poly_saturation t n] return the number of sites with exactly [n] bits set *)
  val poly_saturation : t -> int -> int

  (** [distance s t] return the distance; the number of characters without
      overlapping intersection. *)
  val distance : t -> t -> int 

  (** [fitch_median_2 s t] calculate the median and the added cost of the
      alinged data of [s] and [t]. *)
  val fitch_median_2 : t -> t -> t * int


  (** {2 Tuning Parameters for GC} *)

  (** [gc_freq x] Define frequency of garbage collection; tuning parameter. *)
  val gc_freq : int -> unit
end

(** (possibly) Vectorized type that store at most 8-bits *)
module BV8   : BV

(** (possibly) Vectorized type that store at most 16-bits *)
module BV16  : BV

(** (possibly) Vectorized type that store at most 32-bits *)
module BV32  : BV

(** (possibly) Vectorized type that store at most 64-bits *)
module BV64  : BV

(** Unvectorized type can store an unlimited number of bits.*)
module BVGen : BV
