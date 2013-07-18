(** Combinators **)

let failwithf format = Printf.ksprintf (failwith) format

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let ( $ ) a b = a b

let ( !$ ) a = Lazy.force a

let ( !$$ ) a = List.map (!$) a

let (=.) a b = (abs_float $ a -. b) < epsilon_float

let ( -- ) a b =
  let rec iter acc a bi =
     if a=bi then bi::acc
     else iter (bi::acc) a (bi-1)
  in
  if a <= b then iter [] a b
  else List.rev $ iter [] b a


(** Additional Option type functions *)

let some x = Some x


(** basic numerical functions *)

let is_nan x = match classify_float x with
  | FP_zero | FP_subnormal
  | FP_infinite | FP_normal -> false
  | FP_nan -> true


(** Additional List functions *)

let rand_select n list =
  let rec take i acc = function
    | lst when i = 0 -> rand_select (n+1) (Array.of_list acc) lst
    | [] -> acc
    | x::xs -> take (i-1) (x::acc) xs
  and rand_select i acc = function
    | [] -> Array.to_list acc
    | hd::tl ->
      let j = Random.int i in
      if j < n then acc.(j) <- hd;
      rand_select (i+1) acc tl
  in
  take n [] list

(** Additional Math functions *)

let proportion a b = (float_of_int a) /. (float_of_int b)


(** Additional Random functions *)

let random_of_pair x y =
  if Random.bool () then x else y


(** Modules for Sets/Maps *)

module OrderedInt =
  struct
    type t = int
    let compare a b = a - b
  end

module OrderedTuple =
  struct
    type t = (int * int)
    let compare (a, b) (c, d) = match a - c with
        | 0 -> b - d
        | x -> x
  end

module OrdString =
  struct
    type t = string
    let compare a b = Pervasives.compare a b
  end

module UnorderedTuple =
  struct
    type t = (int * int)
    let compare (a, b) (c, d) =
        let (a,b) = if a > b then a,b else b,a
        and (c,d) = if c > d then c,d else d,c in
        match a - c with
        | 0 -> b - d
        | x -> x
  end

module IntSet = Set.Make (OrderedInt)
module IntMap = Map.Make (OrderedInt)

module PairSet = Set.Make (OrderedTuple)
module PairMap = Map.Make (OrderedTuple)

module UnorderedTupleSet = Set.Make (UnorderedTuple)
module UnorderedTupleMap = Map.Make (UnorderedTuple)

module StringSet = Set.Make (OrdString)
module StringMap = Map.Make (OrdString)

module IntSetSet = Set.Make (IntSet)
module IntSetMap = Map.Make (IntSet)

(** int / bitset functions : TODO move to own module *)

module BitSet =
  struct
    type t = [ `List of int list | `Packed of int | `Set of IntSet.t ]

    let to_int _ = failwith "TODO"

    and to_list _ = failwith "TODO"

    and to_set i =
      let rec set_of_int acc j i =
        if i = 0 then
          acc
        else if (i land (1 lsl j)) > 0 then
          set_of_int (IntSet.add j acc) (j+1) i
        else
          set_of_int acc (j+1) i
      in
      set_of_int IntSet.empty 0 i

    and size _ = failwith "TODO"

end


(** File reading / Writing : TODO move to own module *)

module FileStream = struct

  let read_string_matrix file = failwith "TODO"

  let read_float_matrix file =
    Array.map $ Array.map float_of_string $ read_string_matrix file
  
  let read_integer_matrix file =
    Array.map $ Array.map int_of_string $ read_string_matrix file

  let read_char_matrix file =
    Array.map $ Array.map (fun x -> assert (1 = String.length x); x.[0]) $ read_string_matrix file

end

(** BigArray Functions *)


let ba_of_array1 x = Bigarray.Array1.of_array Bigarray.float64 Bigarray.c_layout x

and ba_of_array2 x = Bigarray.Array2.of_array Bigarray.float64 Bigarray.c_layout x

let create_ba1 x   = Bigarray.Array1.create Bigarray.float64 Bigarray.c_layout x

and create_ba2 x y = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout x y

let ba_to_array1 bray =
  Array.init (Bigarray.Array1.dim bray) (fun i -> bray.{i})

and ba_to_array2 bray =
    let a = Bigarray.Array2.dim1 bray and b = Bigarray.Array2.dim2 bray in
    let r = Array.make_matrix a b 0.0 in
    for i = 0 to a-1 do for j = 0 to b-1 do
        r.(i).(j) <- bray.{i,j};
    done; done; r

let print_barray1 chan a =
    for i = 0 to (Bigarray.Array1.dim a)-1 do
        Printf.fprintf chan "%2.10f\t" a.{i};
    done; Printf.fprintf chan "\n%!"; ()

and print_barray2 chan a =
    for i = 0 to (Bigarray.Array2.dim1 a)-1 do
        for j = 0 to (Bigarray.Array2.dim2 a)-1 do
            Printf.fprintf chan "%2.10f\t" a.{i,j};
        done; Printf.fprintf chan "\n";
    done; Printf.fprintf chan "\n%!"; ()
