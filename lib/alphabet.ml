
open Internal


(** {2 Definition of an Alphabet} *)

exception Illegal_Character of string
exception Illegal_Code of int

type kind =
  | Sequential (** Numbers define states. Should be 0 -- (n-1) *)
  | SimpleBitFlag (** Set bits in numbers define states. *)
  | ExtendedBitFlag (** Same as bitflag with additional character states for polymorphisms. *)
  | Continuous (** A continuous alphabet of values; the states define the alphabet. *)
  | CombinationLevels of int (** A sequential alphabet with additional states for polymorphisms. *)

(** An alphabet stores the character codes, states, their compliments, the type * of alphabet and some other basic information. comb_set/set_comb are used in
 * sequential alphabets only, to be used to associate a sequential state to
 * a set of sequential state that cannot be transformed easily (unlike bitsets). *)
type t =
  { comb_set : IntSet.t IntMap.t;  (* Combination Code -> States *)
    set_comb : int IntSetMap.t;    (* States -> Combination Code *)
    name_code : int StringMap.t;    (* Single Code -> Name *)
    code_name : string IntMap.t;    (* Name -> Single Code *)
    comp_code : int IntMap.t;       (* Code -> Compliment of Code *)
    alphabet_type : kind;           (* Type of the alphbet *)
    size : int;                     (* Size of the basic alphabet; excludes gap *)
    full_size : int;                (* Size of associated matrix *)
    orientation : bool;             (* If cost(~x,x) = cost(x,x) + O(n) *)
    gap : int option;               (* Code for the gap-character; if present *)
    missing: int option;            (* Code for the missing-character; if present *)
    all : int option;               (* Code for the all-character; if present *)
  }


(** {6 Constants *)

(** default gap representation *)
let default_gap = "-"

(** default missing represenation *)
let default_missing = "?"


(** {6 Combination Functions} *)

let generate_combinational_elements ~level codes =
  failwith "TODO"



(** {6 Basic Alphabets} *)

(** A Continuous alphabet does not have character states and an unbounded size.
 * It must be dealt with differently in most situations. *)
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

let count_states alphabet_type code_names =
  let succ_atomic _ x a = if 0 = (x land (x-1)) then a else succ a in
  match alphabet_type with
  | Sequential -> StringMap.cardinal code_names
  | SimpleBitFlag
  | ExtendedBitFlag -> StringMap.fold succ_atomic code_names 0
  | CombinationLevels _ -> failwith "TODO"
  | Continuous -> ~-1

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

(** Generate an alphabet from a list of states (NAME,CODE,COMPLIMENT). *)
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

(** A binary character for the absence and presence of a character *)
let present_absent =
  let lst = [("present", 1, None); ("absent", 2, None) ] in
  of_list lst (Some 2) None None Sequential false

(** Basic DNA alphabet; the simple bit flag annotation implies that
 * polymorphisms of characters are not defined in the alphabet *)
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

(** Define an extended-bit-flag representation of DNA that carries with it
 * associations of polymorphisms to single characters. This also includes
 * further characters from IUPAC that associates polymorphisms with gaps *)
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

(** Define a Sequential alphabet for the amino-acids *)
let aminoacids_char_list =
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

(** return the gap character *)
let get_gap t = match t.gap with
  | Some x -> x
  | None   -> raise Not_found

let has_gap t = match t.gap with
  | Some _ -> true
  | None   -> false

(** return the size of the alphabet *)
let size t = t.size

(** return if orientation is used *)
let orientation t = t.orientation

(** return the all element if it exists *)
let all_char t = t.all

(** return the type of the alphabet *)
let kind t = t.alphabet_type

(** return if the alphabet is state identified *)
let is_statebased t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> true
  | Continuous
  | ExtendedBitFlag
  | SimpleBitFlag -> false

(** return if the alphabet is bit identified *)
let is_bitset t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential
  | Continuous -> false
  | ExtendedBitFlag
  | SimpleBitFlag -> true

(** get the compliment of the character code *)
let complement i t =
  if IntMap.mem i t.comp_code
    then Some (IntMap.find i t.comp_code)
    else None

(** determines if two elements in the alphabet are complements *)
let is_complement a b t =
  let is_complement a b t =
    if IntMap.mem a t.comp_code
      then ((IntMap.find a t.comp_code) = b)
      else false
  in
  let result = is_complement a b t in
  assert(result = is_complement b a t);
  result

(** return the list of states that represent a code *)
let get_combination i t : IntSet.t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential    -> IntMap.find i t.comb_set
  | Continuous    -> IntSet.singleton i
  | ExtendedBitFlag
  | SimpleBitFlag -> BitSet.to_set (`Packed i)

(** Opposite of the above function *)
let get_state_combination s t : int =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> IntSetMap.find s t.set_comb
  | Continuous ->
    assert( (IntSet.cardinal s) = 1 );
    IntSet.choose s
  | ExtendedBitFlag
  | SimpleBitFlag    -> BitSet.to_packed (`List [s])

(** get the code associated with the name of the character *)
let get_code n t =
  try match t.alphabet_type with
    | Continuous -> int_of_string n
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential ->
      StringMap.find (String.uppercase n) t.name_code
  with Not_found -> raise (Illegal_Character n)

(** Return the name of the character code *)
let get_name c t =
  try match t.alphabet_type with
    | Continuous -> string_of_int c
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential -> IntMap.find c t.code_name
  with Not_found -> raise (Illegal_Code c)

(** Convert an alphabet to a list of the main properties *)
let to_list t =
  StringMap.fold (fun k v lst -> (k,v,complement v t)::lst) t.name_code []


(** {6 Converting between types of alphabets *)

(** convert alphabet to a sequentially ordered alphabet; remove combination if
    they exist in the alphabet, continuous alphabets are unchanged. *)
let rec to_sequential t =
  match t.alphabet_type with
  | Sequential
  | Continuous -> t
  | ExtendedBitFlag -> to_sequential (to_bitflag t)
  | SimpleBitFlag -> failwith "TODO"
  | CombinationLevels _ -> failwith "TODO"

(** Convert the alphabet to a simple bit encoding format. This removes extra
    polymorphic states; limited to transforming alphabets < 63bits. *)
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
          
(** Simplify turns the alphabet to one of the following based on it's current
 * state: simplebitflag, sequential, or continuous *)
and simplify t =
  match t.alphabet_type with
  | Sequential
  | CombinationLevels _ -> to_sequential t
  | SimpleBitFlag 
  | ExtendedBitFlag -> to_bitflag t
  | Continuous -> t

(** Convert the alphabet to one with levels; this generates polymorphisms and
    codes under non-bitset situations. *)
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

