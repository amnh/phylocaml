
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

(** int / bitset functions *)

let rec int_of_bitset _ = failwith "TODO"

and bitset_of_int acc j i =
  if i = 0 then
    acc
  else if (i land (1 lsl j)) > 0 then
    bitset_of_int (IntSet.add j acc) (j+1) i
  else
    bitset_of_int acc (j+1) i

and count_bits _ = failwith "TODO"



