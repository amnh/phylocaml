open Internal

module type S =
sig

  type t
  type m

  type spec

  val filter_codes : IntSet.t -> t -> t option
  val filter_codes_comp : IntSet.t -> t -> t option
  val cardinal : t -> int
  val get_codes : t -> IntSet.t
  val mem : int list option -> t -> bool
  val union : t -> t -> t -> t
  val compare : t -> t -> int
  val recode : (int -> int) -> t -> t

  val median_1 : m -> t option -> t -> t
  val median_2 : m -> t option -> t -> t -> t
  val median_3 : m -> t option -> t -> t -> t -> t
  val median_n : m -> t option -> t -> t list -> t

  val adjust_3 : m -> IntSet.t option -> t -> t -> t -> t -> t * IntSet.t
  val adjust_n : m -> IntSet.t option -> t -> t list -> t * IntSet.t

  val cost : t -> float
  val root_cost : t -> float
  val leaf_cost : t -> float

  val distance_1 : m -> t -> t -> float
  val distance_2 : m -> t -> t -> t -> float
  val to_string : t -> string
end
