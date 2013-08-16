
external register : unit -> unit = "bv_CAML_register"

let () = register ()

type t

type elt = int

external code : t -> elt = "bv_CAML_code"

external compare : t -> t -> elt = "bv_CAML_compare"

external cardinal : t -> elt = "bv_CAML_cardinal"


external create : int -> t = "bv_CAML_create"

external copy : t -> t = "bv_CAML_copy"

external of_array : elt array -> t = "bv_CAML_ofarray"


external set_elt : t -> int -> elt -> unit = "bv_CAML_setelt"

external set_bit : t -> int -> int -> unit = "bv_CAML_setbit"

external elt_int : t -> int -> int option = "bv_CAML_eltint"

external elt_states : t -> int -> int list = "bv_CAML_eltstates"


external union : t -> t -> t = "bv_CAML_union"

external inter : t -> t -> t = "bv_CAML_inter"

(* concentration of a particular state 'elt' *)
external saturation : t -> elt -> int = "bv_CAML_saturation"

(* concentration of poly-morphic states of = [n] bits set *)
external poly_saturation : t -> int -> int = "bv_CAML_poly_saturation"


external distance : t -> t -> int = "bv_CAML_distance"

external fitch_median_2 : t -> t -> t = "bv_CAML_fitch_median2"



(** TESTING FUNCTIONS **)

let of_dna str =
  let x = Array.init (String.length str) (fun i -> str.[i]) in
  let x = Array.map
            (fun x -> match x with
             | 'A' | 'a' -> 0b00001 | 'C' | 'c' -> 0b00010
             | 'T' | 't' -> 0b00100 | 'G' | 'g' -> 0b01000
             | '-'       -> 0b10000 | '?'       -> 0b11111
             |  _        -> assert false)
            x
 in
 of_array x

let print t =
  let pp_lst chan xs = List.iter (Printf.fprintf chan "%d") xs in
  let x = Array.init (cardinal t) (fun i -> elt_states t i) in
  Array.iter (fun xs -> Printf.printf "[%a] " pp_lst xs) x


let quick_test () =
  let st1 = "ACTGTATTTG---TG??" in
  let st2 = "AC----TTGGTTATGAA" in
  let t1  = of_dna st1 in
  let t2  = of_dna st2 in
  print t1; print_newline ();
  print t2; print_newline ();
  let t12 = union t1 t2 in
  print t12; print_newline ();
  let t12 = inter t1 t2 in
  print t12; print_newline ();
  let t12 = fitch_median_2 t1 t2 in
  print t12; print_newline ();
  ()

