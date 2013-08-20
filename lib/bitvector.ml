module type BV = sig
  type t
  type elt = int

  val gc_freq : int -> unit

  val code : t -> elt 
  val compare : t -> t -> elt 
  val cardinal : t -> int

  val create : int -> int -> t 
  val copy : t -> t 
  val of_array : int -> elt array -> t 

  val set_elt : t -> int -> elt -> unit 
  val set_bit : t -> int -> int -> unit 
  val elt_int : t -> int -> int option 
  val elt_states : t -> int -> int list 

  val union : t -> t -> t 
  val inter : t -> t -> t 
  val saturation : t -> elt -> int 
  val poly_saturation : t -> int -> int 
  val distance : t -> t -> int 
  val fitch_median_2 : t -> t -> t * int

(*   val map : (elt -> elt) -> t -> t *)
(*   val fold_right : (elt -> 'a -> 'a) -> t -> 'a -> 'a *)
(*   val fold_left  : ('a -> elt -> 'a) -> 'a -> t -> 'a *)

end


module BV8 : BV = struct
  external register : unit -> unit = "bv8_CAML_register"
  external gc_freq : int -> unit = "bv8_CAML_custom_max"
  let () = register ()

  type t
  type elt = int

  external code : t -> elt = "bv8_CAML_code"
  external compare : t -> t -> elt = "bv8_CAML_compare"
  external cardinal : t -> int = "bv8_CAML_cardinal"

  external create : int -> int -> t = "bv8_CAML_create"
  external copy : t -> t = "bv8_CAML_copy"
  external of_array : int -> elt array -> t = "bv8_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv8_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv8_CAML_setbit"
  external elt_int : t -> int -> int option = "bv8_CAML_eltint"
  external elt_states : t -> int -> int list = "bv8_CAML_eltstates"

  external union : t -> t -> t = "bv8_CAML_union"
  external inter : t -> t -> t = "bv8_CAML_inter"
  external saturation : t -> elt -> int = "bv8_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv8_CAML_poly_saturation"
  external distance : t -> t -> int = "bv8_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int = "bv8_CAML_fitch_median2"
end


module BV16 : BV = struct
  external register : unit -> unit = "bv16_CAML_register"
  external gc_freq : int -> unit = "bv16_CAML_custom_max"
  let () = register ()
  type t
  type elt = int

  external code : t -> elt = "bv16_CAML_code"
  external compare : t -> t -> elt = "bv16_CAML_compare"
  external cardinal : t -> int = "bv16_CAML_cardinal"

  external create : int -> int -> t = "bv16_CAML_create"
  external copy : t -> t = "bv16_CAML_copy"
  external of_array : int -> elt array -> t = "bv16_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv16_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv16_CAML_setbit"
  external elt_int : t -> int -> int option = "bv16_CAML_eltint"
  external elt_states : t -> int -> int list = "bv16_CAML_eltstates"

  external union : t -> t -> t = "bv16_CAML_union"
  external inter : t -> t -> t = "bv16_CAML_inter"
  external saturation : t -> elt -> int = "bv16_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv16_CAML_poly_saturation"
  external distance : t -> t -> int = "bv16_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int = "bv16_CAML_fitch_median2"
end


module BV32 : BV = struct
  external register : unit -> unit = "bv32_CAML_register"
  external gc_freq : int -> unit = "bv32_CAML_custom_max"
  let () = register ()

  type t
  type elt = int

  external code : t -> elt = "bv32_CAML_code"
  external compare : t -> t -> elt = "bv32_CAML_compare"
  external cardinal : t -> int = "bv32_CAML_cardinal"

  external create : int -> int -> t = "bv32_CAML_create"
  external copy : t -> t = "bv32_CAML_copy"
  external of_array : int -> elt array -> t = "bv32_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv32_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv32_CAML_setbit"
  external elt_int : t -> int -> int option = "bv32_CAML_eltint"
  external elt_states : t -> int -> int list = "bv32_CAML_eltstates"

  external union : t -> t -> t = "bv32_CAML_union"
  external inter : t -> t -> t = "bv32_CAML_inter"
  external saturation : t -> elt -> int = "bv32_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv32_CAML_poly_saturation"
  external distance : t -> t -> int = "bv32_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int = "bv32_CAML_fitch_median2"
end

module BV64 : BV = struct
  external register : unit -> unit = "bv64_CAML_register"
  external gc_freq : int -> unit = "bv64_CAML_custom_max"
  let () = register ()

  type t
  type elt = int

  external code : t -> elt = "bv64_CAML_code"
  external compare : t -> t -> elt = "bv64_CAML_compare"
  external cardinal : t -> int = "bv64_CAML_cardinal"

  external create : int -> int -> t = "bv64_CAML_create"
  external copy : t -> t = "bv64_CAML_copy"
  external of_array : int -> elt array -> t = "bv64_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv64_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv64_CAML_setbit"
  external elt_int : t -> int -> int option = "bv64_CAML_eltint"
  external elt_states : t -> int -> int list = "bv64_CAML_eltstates"

  external union : t -> t -> t = "bv64_CAML_union"
  external inter : t -> t -> t = "bv64_CAML_inter"
  external saturation : t -> elt -> int = "bv64_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv64_CAML_poly_saturation"
  external distance : t -> t -> int = "bv64_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int = "bv64_CAML_fitch_median2"
end

(* module BVGen : BV = struct
  type t
  type elt = int

  val gc_freq : int -> unit

  val code : t -> elt 
  val compare : t -> t -> elt 
  val cardinal : t -> int

  val create : int -> t 
  val copy : t -> t 
  val of_array : elt array -> t 

  val set_elt : t -> int -> elt -> unit 
  val set_bit : t -> int -> int -> unit 
  val elt_int : t -> int -> int option 
  val elt_states : t -> int -> int list 

  val union : t -> t -> t 
  val inter : t -> t -> t 
  val saturation : t -> elt -> int 
  val poly_saturation : t -> int -> int 
  val distance : t -> t -> int 
  val fitch_median_2 : t -> t -> t 

end *)
