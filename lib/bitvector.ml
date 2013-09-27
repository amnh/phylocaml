open Internal

module type BV = sig
  type t
  type elt

  val create : int -> int -> t
  val copy : t -> t
  val of_array : int -> elt array -> t

  val code : t -> int
  val compare : t -> t -> int
  val cardinal : t -> int
  val width : t -> int
  val max_width : int

  val set_elt : t -> int -> elt -> unit
  val set_bit : t -> int -> int -> unit
  val elt_int : t -> int -> int option
  val get_elt : t -> int -> elt
  val elt_states : t -> int -> int list

  val union : t -> t -> t
  val inter : t -> t -> t
  val saturation : t -> elt -> int
  val poly_saturation : t -> int -> int
  val distance : t -> t -> int
  val fitch_median_2 : t -> t -> t * int

  val random_elt  : int -> elt
  val elt_of_ints : int list -> elt
  val ints_of_elt : elt -> int list
  val elt_of_int  : int -> elt

  val gc_freq : int -> unit
end

(*
let map (module BVN : BV) f t =
  let r = BVN.copy t in
  for i = 0 to (BVN.cardinal t)-1 do
    BVN.set_elt r i $ f (BVN.get_elt t i)
  done;
  r

let fold_left (module BVN : BV) f t acc =
  let acc = ref acc in
  for i = (BVN.cardinal t)-1 downto 0 do
    acc := f (BVN.get_elt t i) acc
  done;
  !acc

let fold_right (module BVN : BV) f acc t =
  let acc = ref acc in
  for i = 0 to (BVN.cardinal t)-1 do
    acc := f !acc $ BVN.get_elt t i
  done;
  !acc
*)

let random_elt_of max_width width =
  assert( max_width >= width );
  Random.int (1 lsl width)

module BV8 : BV with type elt = int = struct
  external register : unit -> unit = "bv8_CAML_register"
  let () = register ()

  type t
  type elt = int

  external code : t -> int                   = "bv8_CAML_code"
  external compare : t -> t -> int           = "bv8_CAML_compare"
  external cardinal : t -> int               = "bv8_CAML_cardinal"
  external width : t -> int                  = "bv8_CAML_size"
  let max_width = 8

  external create : int -> int -> t          = "bv8_CAML_create"
  external copy : t -> t                     = "bv8_CAML_copy"
  external of_array : int -> elt array -> t  = "bv8_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv8_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv8_CAML_setbit"
  external elt_int : t -> int -> int option  = "bv8_CAML_eltint"
  external get_elt : t -> int -> elt         = "bv8_CAML_getelt"
  external elt_states : t -> int -> int list = "bv8_CAML_eltstates"

  external union : t -> t -> t               = "bv8_CAML_union"
  external inter : t -> t -> t               = "bv8_CAML_inter"
  external saturation : t -> elt -> int      = "bv8_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv8_CAML_poly_saturation"
  external distance : t -> t -> int          = "bv8_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int= "bv8_CAML_fitch_median2"

  let random_elt w : elt = random_elt_of 8 w
  let elt_of_ints _ = failwith "TODO"
  let ints_of_elt _ = failwith "TODO"
  let elt_of_int _ = failwith "TODO"

  external gc_freq : int -> unit             = "bv8_CAML_custom_max"
end


module BV16 : BV with type elt = int = struct
  external register : unit -> unit           = "bv16_CAML_register"
  let () = register ()

  type t
  type elt = int

  external code : t -> int                   = "bv16_CAML_code"
  external compare : t -> t -> int           = "bv16_CAML_compare"
  external cardinal : t -> int               = "bv16_CAML_cardinal"
  external width : t -> int                  = "bv16_CAML_size"
  let max_width = 16

  external create : int -> int -> t          = "bv16_CAML_create"
  external copy : t -> t                     = "bv16_CAML_copy"
  external of_array : int -> elt array -> t  = "bv16_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv16_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv16_CAML_setbit"
  external elt_int : t -> int -> int option  = "bv16_CAML_eltint"
  external get_elt : t -> int -> elt         = "bv16_CAML_getelt"
  external elt_states : t -> int -> int list = "bv16_CAML_eltstates"

  external union : t -> t -> t               = "bv16_CAML_union"
  external inter : t -> t -> t               = "bv16_CAML_inter"
  external saturation : t -> elt -> int      = "bv16_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv16_CAML_poly_saturation"
  external distance : t -> t -> int          = "bv16_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int= "bv16_CAML_fitch_median2"
  
  let random_elt w : elt = random_elt_of 16 w
  let elt_of_ints _ = failwith "TODO"
  let ints_of_elt _ = failwith "TODO"
  let elt_of_int _ = failwith "TODO"

  external gc_freq : int -> unit             = "bv16_CAML_custom_max"
end


module BV32 : BV with type elt = Int32.t = struct
  external register : unit -> unit           = "bv32_CAML_register"
  let () = register ()

  type t
  type elt = Int32.t

  external code : t -> int                   = "bv32_CAML_code"
  external compare : t -> t -> int           = "bv32_CAML_compare"
  external cardinal : t -> int               = "bv32_CAML_cardinal"
  external width : t -> int                  = "bv32_CAML_size"
  let max_width = 32

  external create : int -> int -> t          = "bv32_CAML_create"
  external copy : t -> t                     = "bv32_CAML_copy"
  external of_array : int -> elt array -> t  = "bv32_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv32_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv32_CAML_setbit"
  external elt_int : t -> int -> int option  = "bv32_CAML_eltint"
  external get_elt : t -> int -> elt         = "bv32_CAML_getelt"
  external elt_states : t -> int -> int list = "bv32_CAML_eltstates"

  external union : t -> t -> t               = "bv32_CAML_union"
  external inter : t -> t -> t               = "bv32_CAML_inter"
  external saturation : t -> elt -> int      = "bv32_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv32_CAML_poly_saturation"
  external distance : t -> t -> int          = "bv32_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int= "bv32_CAML_fitch_median2"
  
  let random_elt _ = failwith "TODO"
  let elt_of_ints _ = failwith "TODO"
  let ints_of_elt _ = failwith "TODO"
  let elt_of_int _ = failwith "TODO"

  external gc_freq : int -> unit             = "bv32_CAML_custom_max"
end

module BV64 : BV with type elt = Int64.t = struct
  external register : unit -> unit           = "bv64_CAML_register"
  let () = register ()

  type t
  type elt = Int64.t

  external code : t -> int                   = "bv64_CAML_code"
  external compare : t -> t -> int           = "bv64_CAML_compare"
  external cardinal : t -> int               = "bv64_CAML_cardinal"
  external width : t -> int                  = "bv64_CAML_size"
  let max_width = 64

  external create : int -> int -> t          = "bv64_CAML_create"
  external copy : t -> t                     = "bv64_CAML_copy"
  external of_array : int -> elt array -> t  = "bv64_CAML_ofarray"

  external set_elt : t -> int -> elt -> unit = "bv64_CAML_setelt"
  external set_bit : t -> int -> int -> unit = "bv64_CAML_setbit"
  external elt_int : t -> int -> int option  = "bv64_CAML_eltint"
  external get_elt : t -> int -> elt         = "bv64_CAML_getelt"
  external elt_states : t -> int -> int list = "bv64_CAML_eltstates"

  external union : t -> t -> t               = "bv64_CAML_union"
  external inter : t -> t -> t               = "bv64_CAML_inter"
  external saturation : t -> elt -> int      = "bv64_CAML_saturation"
  external poly_saturation : t -> int -> int = "bv64_CAML_poly_saturation"
  external distance : t -> t -> int          = "bv64_CAML_distance2"
  external fitch_median_2 : t -> t -> t * int= "bv64_CAML_fitch_median2"
  
  let random_elt _ = failwith "TODO"
  let elt_of_ints _ = failwith "TODO"
  let ints_of_elt _ = failwith "TODO"
  let elt_of_int _ = failwith "TODO"

  external gc_freq : int -> unit             = "bv64_CAML_custom_max"
end

module BVGen : BV with type elt = int list = struct
  
  type elt = int list

  type t = 
    { bv : elt array; code: int; width : int; }
  
  let next_code = ref ~-1
  let get_next_code () = incr next_code; !next_code

  let code t = t.code
  let compare _ _ = failwith "TODO"
  let cardinal t = Array.length t.bv
  let width t = t.width
  let max_width = max_int

  let create width l = {bv = Array.make l []; code = get_next_code (); width;}
  let copy _ = failwith "TODO"
  let of_array _ _ = failwith "TODO"

  let set_elt _ _ _ = failwith "TODO"
  let set_bit _ _ _ = failwith "TODO"
  let elt_int _ _ = failwith "TODO"
  let get_elt t i = t.bv.(i)
  let elt_states _ _ = failwith "TODO"

  let union _ _ = failwith "TODO"
  let inter _ _ = failwith "TODO"
  let saturation _ _ = failwith "TODO"
  let poly_saturation _ _ = failwith "TODO"
  let distance _ _ = failwith "TODO"
  let fitch_median_2 _ _ = failwith "TODO"
 
  let random_elt _ = failwith "TODO"
  let elt_of_ints _ = failwith "TODO"
  let ints_of_elt _ = failwith "TODO"
  let elt_of_int _ = failwith "TODO"

  let gc_freq _ = failwith "TODO"
end

