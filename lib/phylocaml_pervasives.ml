let failwithf format =
    Printf.ksprintf (failwith) format

let (|>) a b = b a

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

module PairSet = Set.Make (UnorderedTuple)
module PairMap = Map.Make (UnorderedTuple)

module StringSet = Set.Make (OrdString)
module StringMap = Map.Make (OrdString)

module IntSetSet = Set.Make (IntSet)
module IntSetMap = Map.Make (IntSet)

let random_of_pair x y =
  if Random.bool () then x else y
