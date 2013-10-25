include Compatibility

let failwithf format = Printf.ksprintf (failwith) format

let ( !$ ) a = Lazy.force a

let ( !$$ ) a = List.map (!$) a

let (=.) a b = (abs_float @@ a -. b) < epsilon_float

let ( -- ) a b =
  let rec iter acc a bi =
     if a=bi then bi::acc
     else iter (bi::acc) a (bi-1)
  in
  if a <= b then iter [] a b
  else List.rev @@ iter [] b a


(** Additional Option type functions *)

let some x = Some x


(** basic numerical functions *)

let is_nan x = match classify_float x with
  | FP_zero | FP_subnormal
  | FP_infinite | FP_normal -> false
  | FP_nan -> true


(** Additional List functions *)

let over_path f acc lst =
  let rec consume acc x = function
    | y::ys -> consume (f acc x y) y ys
    | []    -> acc
  in
  match lst with
  | x::y::xs -> consume (f acc x y) y xs
  | [_] | [] -> acc
  

let random_select n list =
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


(** Additional Array functions *)

let array_fold_left2 f acc aray bray =
  assert( (Array.length aray) = (Array.length bray) );
  let acc = ref acc in
  for i = 0 to (Array.length aray)-1 do
    acc := f !acc aray.(i) bray.(i)
  done;
  !acc


(** Additional Math functions *)

let proportion a b = (float_of_int a) /. (float_of_int b)


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


(** Additional Random functions *)

let random_choice x y =
  if Random.bool () then x else y

let random_elt_intmap (type t) (m: t IntMap.t) : IntMap.key * t =
  let n = IntMap.cardinal m in
  let i = Random.int n in
  let module M = struct exception E of IntMap.key * t end in
  try 
    IntMap.fold
      (fun k v j -> if i = j then raise (M.E (k,v)) else succ j) m 0
        |> ignore;
    assert false
  with M.E (k,v) -> k,v

let random_elt_intset (s: IntSet.t) =
  let n = IntSet.cardinal s in
  let i = Random.int n in
  let module M = struct exception E of IntSet.elt end in
  try 
    IntSet.fold
      (fun k j -> if i = j then raise (M.E k) else succ j) s 0
        |> ignore;
    assert false
  with M.E k -> k

let random_elt_pairset (s: UnorderedTupleSet.t) =
  let n = UnorderedTupleSet.cardinal s in
  let i = Random.int n in
  let module M = struct exception E of UnorderedTupleSet.elt end in
  try
    UnorderedTupleSet.fold
      (fun k j -> if i = j then raise (M.E k) else succ j) s 0
        |> ignore;
    assert false
  with M.E k -> k


(** int / bitset functions : TODO move to own module // find a nice bitset *)
module BitSet =
  struct
    type t = [ `List of int list | `Packed of int | `Set of IntSet.t ]

    let add _ _ = failwith "TODO"

    and rem _ _ = failwith "TODO"

    and empty = `Packed 0

    and of_list _ = failwith "TODO"

    and of_int _ = failwith "TODO"

    let to_packed t : int = match t with
      | `List _   -> failwith "TODO"
      | `Packed i -> i
      | `Set _    -> failwith "TODO"

    and to_list t = match t with
      | `List t   -> t
      | `Packed i ->
        let rec set_of_int acc j i =
          if i = 0 then
            acc
          else
            let s = 1 lsl j in
            if (i land s) > 0
              then set_of_int (j::acc) (j+1) (i lxor s)
              else set_of_int acc (j+1) i
        in
        set_of_int [] 0 i
      | `Set s    -> IntSet.elements s

    and to_set t = match t with
      | `Set s    -> s
      | `List t   ->
        List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty t
      | `Packed i ->
        let rec set_of_int acc j i =
          if i = 0 then
            acc
          else if (i land (1 lsl j)) > 0 then
            set_of_int (IntSet.add j acc) (j+1) i
          else
            set_of_int acc (j+1) i
        in
        set_of_int IntSet.empty 0 i

    and size t =  match t with
      | `List t   -> List.length t
      | `Set s    -> IntSet.cardinal s
      | `Packed i ->
        let rec count_bits acc x =
          if x = 0
            then acc
            else count_bits (acc+1) (x land (x-1))
        in
        count_bits 0 i
end


(** File reading / Writing : TODO move to own module *)

module FileStream = struct

  let read_string_matrix _ : string array array = failwith "TODO"

  let read_float_matrix file =
    Array.map (Array.map float_of_string) @@ read_string_matrix file
  
  let read_integer_matrix file =
    Array.map (Array.map int_of_string) @@ read_string_matrix file

  let read_char_matrix file =
    Array.map (Array.map (fun x -> assert (1 = String.length x); x.[0])) @@ read_string_matrix file

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
  let r = Array.make_matrix a b bray.{0,0} in
  for i = 0 to a-1 do for j = 0 to b-1 do
    r.(i).(j) <- bray.{i,j};
  done; done;
  r
