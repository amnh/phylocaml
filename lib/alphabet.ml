
open Internal

(** {2 Definition of an Alphabet} *)

exception Illegal_Character of string
exception Illegal_Code of int
exception Illegal_Alphabet_Size of int

type kind =
  | Sequential
  | SimpleBitFlag
  | ExtendedBitFlag
  | Continuous
  | CombinationLevels of int

type t =
  { comb_set : IntSet.t IntMap.t;
    set_comb : int IntSetMap.t;
    name_code : int StringMap.t;
    code_name : string IntMap.t;
    comp_code : int IntMap.t;
    alphabet_type : kind;
    size : int;
    full_size : int;
    orientation : bool;
    gap : int option;
    missing: int option;
    all : int option;
  }


(** {6 Constants *)

let default_gap = "-"

let default_missing = "?"

let default_orientation = "~"


(** {6 Combination Functions} *)

let generate_combinational_elements ~level codes =
  let incr r = incr r; !r in
  let cross_product map1 map2 k =
    IntMap.fold
      (fun _ v1 ->
        IntMap.fold
          (fun _ v2 -> IntMap.add (incr k) (IntSet.union v1 v2))
          map2)
      map1
      IntMap.empty
  and inverse map =
    IntMap.fold (fun k v -> IntSetMap.add v k) map IntSetMap.empty
  and merge_maps =
    IntMap.merge
      (fun _ v1 v2 -> match v1,v2 with
        | None, None | Some _, Some _ -> assert false
        | None, ((Some _) as x)
        | ((Some _) as x), None -> x)
  in
  let l = Array.make level IntMap.empty in
  l.(0) <- IntMap.fold (fun i _ a -> IntMap.add i (IntSet.singleton i) a)
                       codes IntMap.empty;
  let code = ref (fst $ IntMap.max_binding l.(0)) in
  for i = 1 to level-1 do
    l.(i) <- cross_product l.(0) l.(i-1) code
  done;
  let comb_set = Array.fold_left merge_maps IntMap.empty l in
  comb_set, inverse comb_set

(** Tests any number of things to verify the integrity of an alphabet
    1 - that sequential alphabets are 0 indexed.
    2 - that the correct number of combinations exists.
    3 - the sequential alphabets don't skip numbers *)
let verify_alphabet a = match a.alphabet_type with
  | Sequential ->
    let rec verify i =
      if i = a.size then
        true
      else if IntMap.mem i a.code_name then
        (StringMap.mem (IntMap.find i a.code_name) a.name_code) && (verify (i+1))
      else
        false
    in
    ((IntMap.cardinal a.code_name) = a.size) && (verify 0)
  | CombinationLevels _ -> true
  | ExtendedBitFlag
  | SimpleBitFlag
  | Continuous -> true

let of_list lst gap all missing alphabet_type orientation =
  let add_one (cname,ncode,ccode) (name,code,comp) =
    let name = String.uppercase name in
    let ccode = match comp with
      | None -> ccode
      | Some x -> IntMap.add code x ccode
    in
    StringMap.add name code cname, IntMap.add code name ncode, ccode
  in
  let name_code,code_name,comp_code =
    List.fold_left add_one (StringMap.empty,IntMap.empty,IntMap.empty) lst
  in
  let comb_set,set_comb = match alphabet_type with
    | CombinationLevels level      -> generate_combinational_elements ~level code_name
    | Sequential | SimpleBitFlag
    | Continuous | ExtendedBitFlag -> IntMap.empty, IntSetMap.empty
  in
  let () = match gap with
    | None -> ()
    | Some x -> assert( IntMap.mem x code_name );
  in
  let a = {
    size = IntMap.cardinal code_name;
    full_size = IntMap.cardinal comb_set;
    alphabet_type; orientation;
    code_name; name_code;
    comb_set; set_comb;
    comp_code;
    all; gap; missing;
  } in
  assert ( verify_alphabet a );
  a


(** {6 Basic Alphabets} *)

let continuous =
  { alphabet_type = Continuous;
    comb_set = IntMap.empty;
    set_comb = IntSetMap.empty;
    name_code = StringMap.empty;
    code_name = IntMap.empty;
    comp_code = IntMap.empty;
    size      = max_int;
    full_size = ~-1;
    gap       = None;
    missing   = None;
    all       = None;
    orientation = false;
  }

let present_absent =
  let lst = [("present", 1, None); ("absent", 2, None) ] in
  of_list lst (Some 2) None None Sequential false

let dna =
  let lst = [
    ("A", 0b00001, Some 0b01000);
    ("C", 0b00010, Some 0b00100);
    ("G", 0b00100, Some 0b00010);
    ("T", 0b01000, Some 0b00001);
    ("-", 0b10000, None);
    ("X", 0b11111, Some 0b11111); ]
  in
  of_list lst (Some 0b10000) (Some 0b11111) (Some 0b10000) SimpleBitFlag false

let nucleotides =
  let lst = [ 
    ("A", 0b00001, Some 0b01000);
    ("C", 0b00010, Some 0b00100);
    ("G", 0b00100, Some 0b00010);
    ("T", 0b01000, Some 0b00001);
    ("M", 0b00011, Some 0b01100); 
    ("R", 0b00101, Some 0b01010); 
    ("W", 0b01001, Some 0b01001);
    ("S", 0b00110, Some 0b00110);
    ("Y", 0b01010, Some 0b00101);
    ("K", 0b01100, Some 0b00011);
    ("V", 0b00111, Some 0b01110);
    ("H", 0b01011, Some 0b01101);
    ("D", 0b01101, Some 0b01011);
    ("B", 0b01110, Some 0b00111);
    ("N", 0b01111, Some 0b01111);
    ("X", 0b01111, Some 0b01111);
    ("-", 0b10000, None);
    ("1", 0b10001, Some 0b11000);
    ("2", 0b10010, Some 0b10100);
    ("3", 0b10011, Some 0b11100);
    ("4", 0b10100, Some 0b10010);
    ("5", 0b10101, Some 0b11010);
    ("6", 0b10110, Some 0b10110);
    ("7", 0b10111, Some 0b11110);
    ("8", 0b11000, Some 0b10001);
    ("9", 0b11001, Some 0b11001);
    ("0", 0b11010, Some 0b10101);
    ("E", 0b11011, Some 0b11101);
    ("F", 0b11100, Some 0b10011);
    ("I", 0b11101, Some 0b11011);
    ("J", 0b11110, Some 0b10111);
    ("P", 0b11111, Some 0b11111);
    ("?", 0b11111, Some 0b11111);
  ] in
  of_list lst (Some 0b10000) (Some 0b11111) (Some 0b11111) ExtendedBitFlag false

let aminoacids =
  let lst = [
    ("A",  0, None); (* alanine *)
    ("R",  1, None); (* arginine *)
    ("N",  2, None); (* asparagine *)
    ("D",  3, None); (* aspartic *)
    ("C",  4, None); (* cysteine *)
    ("Q",  5, None); (* glutamine *)
    ("E",  6, None); (* glutamic *)
    ("G",  7, None); (* glycine *)
    ("H",  8, None); (* histidine *)
    ("I",  9, None); (* isoleucine *)
    ("L", 10, None); (* leucine *)
    ("K", 11, None); (* lysine *)
    ("M", 12, None); (* methionine *)
    ("F", 13, None); (* phenylalanine *)
    ("P", 14, None); (* proline *)
    ("S", 15, None); (* serine *)
    ("T", 16, None); (* threonine *)
    ("W", 17, None); (* tryptophan *)
    ("Y", 18, None); (* tyrosine *)
    ("V", 19, None); (* valine *)
    ("X", 20, None); (* all element *)
    ("-", 21, None); (* gap *)
  ] in
  of_list lst (Some 21) (Some 20) (Some 20) Sequential false



(** {6 Basic Functions for querying alphabets *)

let get_gap t = match t.gap with
  | Some x -> x
  | None   -> raise Not_found

let has_gap t = match t.gap with
  | Some _ -> true
  | None   -> false

let size t = t.size

let orientation t = t.orientation

let all_char t = t.all

let kind t = t.alphabet_type

let is_statebased t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> true
  | Continuous
  | ExtendedBitFlag
  | SimpleBitFlag -> false

let is_bitset t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential
  | Continuous -> false
  | ExtendedBitFlag
  | SimpleBitFlag -> true

let complement i t =
  if IntMap.mem i t.comp_code
    then Some (IntMap.find i t.comp_code)
    else None

let is_complement a b t =
  let is_complement a b t =
    if IntMap.mem a t.comp_code
      then ((IntMap.find a t.comp_code) = b)
      else false
  in
  let result = is_complement a b t in
  assert(result = is_complement b a t);
  result

let get_combination i t : IntSet.t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential    -> IntMap.find i t.comb_set
  | Continuous    -> IntSet.singleton i
  | ExtendedBitFlag
  | SimpleBitFlag -> BitSet.to_set (`Packed i)

let get_state_combination s t : int =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> IntSetMap.find s t.set_comb
  | Continuous ->
    assert( (IntSet.cardinal s) = 1 );
    IntSet.choose s
  | ExtendedBitFlag
  | SimpleBitFlag    -> BitSet.to_packed (`Set s)

let get_code n t =
  try match t.alphabet_type with
    | Continuous -> int_of_string n
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential ->
      StringMap.find (String.uppercase n) t.name_code
  with Not_found -> raise (Illegal_Character n)

let get_name c t =
  try match t.alphabet_type with
    | Continuous -> string_of_int c
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential -> IntMap.find c t.code_name
  with Not_found -> raise (Illegal_Code c)

let to_list t =
  StringMap.fold (fun k v lst -> (k,v,complement v t)::lst) t.name_code []


(** {6 Converting between types of alphabets *)

let rec to_sequential t =
  match t.alphabet_type with
  | Sequential
  | Continuous -> t
  | ExtendedBitFlag -> to_sequential (to_bitflag t)
  | SimpleBitFlag -> failwith "TODO"
  | CombinationLevels _ -> failwith "TODO"

and to_bitflag t =
  match t.alphabet_type with
  | Continuous
  | SimpleBitFlag -> t
  | CombinationLevels _ -> failwith "TODO"
  | ExtendedBitFlag ->
    let is_atomic x = 0 = (x land (x-1)) in
    let lst =
      IntMap.fold
        (fun code name lst ->
          if is_atomic code then (name,code,complement code t)::lst else lst)
        t.code_name
        []
    in
    assert( match t.gap with Some x -> is_atomic x | None -> false );
    assert( t.size = (IntMap.cardinal t.code_name) );
    of_list lst t.gap t.all t.missing SimpleBitFlag t.orientation
  | Sequential -> failwith "TODO"
    (* how to deal with large alphabets? *)
          
and simplify t =
  match t.alphabet_type with
  | Sequential
  | CombinationLevels _ -> to_sequential t
  | SimpleBitFlag 
  | ExtendedBitFlag -> to_bitflag t
  | Continuous -> t

and to_level level t =
  assert( level > 0 );
  match t.alphabet_type with
  | CombinationLevels _ when level = 1 -> to_sequential t
  | CombinationLevels l when level = l -> t
  | Sequential          when level = 1 -> t
  | CombinationLevels _                -> to_level level (to_sequential t)
  | Continuous
  | SimpleBitFlag
  | ExtendedBitFlag -> t
  | Sequential ->
    let combs,lsts = generate_combinational_elements ~level t.code_name in
    { t with
      comb_set = combs;
      set_comb = lsts;
      alphabet_type = CombinationLevels level; }

