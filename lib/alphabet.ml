
open Phylocaml_pervasives


(** {6 Definition of an Alphabet} *)

type kind =
  | Sequential (** Numbers define states. Should be 0 -- (n-1) *)
  | SimpleBitFlag (** Set bits in numbers define states. *)
  | ExtendedBitFlag (** Same as bitflag with additional character states for polymorphisms. *)
  | Continuous (** A continuous alphabet of values; the states define the alphabet. *)
  | CombinationLevels of int (** A sequential alphabet with additional states for polymorphisms. *)

(** An alphabet stores the character codes, states, their compliments, the type * of alphabet and some other basic information. comb_list/list_comb are used in
 * sequential alphabets only, to be used to associate a sequential state to
 * a set of sequential state that cannot be transformed easily (unlike bitsets). *)
type t =
  { comb_list : IntSet.t IntMap.t;
    list_comb : int IntSetMap.t;
    code_name : int StringMap.t;
    name_code : string IntMap.t;
    comp_code : int option IntMap.t;
    states    : IntSet.t;
    alphabet_type : kind;
    size : int;
    gap : int option;
    orientation : bool;
    all : int option;
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
    comb_list = IntMap.empty;
    list_comb = IntSetMap.empty;
    code_name = StringMap.Singleton "-" ~-1;
    name_code = IntMap.singleton ~-1 "-";
    comp_code = IntMap.empty;
    size      = max_int;
    gap       = None;
    orientation = false;
    all = None;
  }

let count_states alphabet_type code_names =
  let succ_atomic x = if 0 = (x land (x-1)) then succ else (fun x -> x) in
  match alphabet_type with
  | Sequential -> StringMap.cardinal code_names
  | SimpleBitFlag
  | ExtendedBitFlag -> StringMap.fold succ_atomic 0 code_names
  | CombinationLevels i ->
  | Continuous -> ~-1


(** Generate an alphabet from a list of states (NAME,CODE,COMPLIMENT). *)
let of_list lst gap all alphabet_type orientation =
  let add_one (cname,ncode,ccode) (name,code,comp) =
    let name = String.uppercase name in
    let ccode = match comp with
      | None -> ccode
      | Some x -> IntMap.add code comp ccode
    in
    StringMap.add name code cname, IntMap.add code name ncode, ccode
  in
  let code_name,name_code,comp_code =
    List.fold_left add_one (StringMap.empty,IntMap.empty,IntMap.empty) lst
  in
  let comb_list,list_comb = match alphabet_type with
    | CombinationLevels level      -> generate_combinational_elements ~level code_name
    | Sequential | SimpleBitFlag
    | Continuous | ExtendedBitFlag -> IntMap.empty, IntMap.empty
  in
  {
    size = IntMap.cardinal name_code;
    gap = match gap with | None -> None | Some gap -> Some (StringMap.find gap code_name);
    alphabet_type; orientation;
    code_name; name_code; comp_code; all; comb_list; list_comb;
  }

(** A binary character for the absence and presence of a character *)
let present_absent =
  let lst = [("present", 1, None); ("absent", 2, None) ] in
  a_of_list lst (Some "absent") None Sequential false

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
  of_list lst (Some "-") (Some 0b11111) SimpleBitFlag false

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
  of_list lst (Some "-") (Some 0b11111) ExtendedBitFlag false

(** Define a Sequential alphabet for the amino-acids *)
let aminoacids_char_list =
  let lst = [
    ("A",  1, None); (* alanine *)
    ("R",  2, None); (* arginine *)
    ("N",  3, None); (* asparagine *)
    ("D",  4, None); (* aspartic *)
    ("C",  5, None); (* cysteine *)
    ("Q",  6, None); (* glutamine *)
    ("E",  7, None); (* glutamic *)
    ("G",  8, None); (* glycine *)
    ("H",  9, None); (* histidine *)
    ("I", 10, None); (* isoleucine *)
    ("L", 11, None); (* leucine *)
    ("K", 12, None); (* lysine *)
    ("M", 13, None); (* methionine *)
    ("F", 14, None); (* phenylalanine *)
    ("P", 15, None); (* proline *)
    ("S", 16, None); (* serine *)
    ("T", 17, None); (* threonine *)
    ("W", 18, None); (* tryptophan *)
    ("Y", 19, None); (* tyrosine *)
    ("V", 20, None); (* Valine *)
    ("X", 21, None); (* all element *)
    ("-", 22, None); (* gap *)
  ] in
  of_list lst (Some "-") (Some 21) Sequential false


(** {6 Basic Functions for querying alphabets *)

(** return the gap character *)
let get_gap_char t = IntMap.find t.gap t.name_code

(** return the size of the alphabet *)
let get_size t = t.size

(** return if orientation is used *)
let get_orientation t = t.orientation

(** return the all element if it exists *)
let get_all_state t = t.all

(** return the type of the alphabet *)
let get_type t = t.alphabet_type

(** get the compliment of the character code *)
let get_comp i t =
  try IntMap.find i t.comp_code
  with Not_found -> None

(** return the list of states that represent a state *)
let get_combination_states i t =
  match t.alphabet_type with
  | CombinationLevels
  | Sequential -> IntMap.find i t.comb_list
  | Continuous -> IntSet.singleton i
  | ExtendedBitFlag
  | SimpleBitFlag    -> bitset_of_int i

(** Opposite of the above function *)
let get_state_combination s t =
  match t.alphabet_type with
  | CombinationLevels
  | Sequential -> IntSetMap.find s t.list_comb
  | Continuous ->
    assert( (IntSet.cardinal s) = 1 );
    IntSet.choose s
  | ExtendedBitFlag
  | SimpleBitFlag    -> int_of_bitset i

(** get the code associated with the name of the character *)
let get_code n t =
  match t.alphabet_type with
  | Continuous -> int_of_string c
  | SimpleBitFlag | ExtendedBitFlag
  | CombinationLevels _ | Sequential ->
    StringMap.find (String.uppercase n) t.code_name

(** Return the name of the character code *)
let get_name c t =
  match t.alphabet_type with
  | Continuous -> string_of_int c
  | SimpleBitFlag | ExtendedBitFlag
  | CombinationLevels _ | Sequential -> IntMap.find c t.name_code

(** Convert an alphabet to a list of the main properties *)
let to_list t =
  StringMap.fold
    (fun k v lst -> (k,v,get_comp v t)::lst)
    t.code_name
    []


(** {6 Converting between types of alphabets *)

(** convert alphabet to a sequentially ordered alphabet; remove combination if
    they exist in the alphabet, continuous alphabets are unchanged. *)
let rec to_sequential t =
  match t.alphabet with
  | Sequential
  | Continuous -> t
  | ExtendedBitFlag -> to_sequential (to_simple t)
  | SimpleBitFlag ->
  | CombinationLevels _ -> 

(** Convert the alphabet to a simple bit encoding format. This removes extra
    polymorphic states; limited to transforming alphabets < 63bits *)
and to_simple t =
  match t.alphabet with
  | Continuous
  | SimpleBitFlag -> t
  | CombinationLevels l ->
  | ExtendedBitFlag ->
    let is_atomic x = 0 = (x land (x-1)) in
    let lst =
      IntMap.fold
        (fun code name lst ->
          if is_atomic code then (name,code,get_comp)::lst else lst)
        (IntMap.empty,StringMap.empty)
    in
    assert( is_atomic gap );
    assert( t.size = (IntMap.cardinal names) );
    of_list lst t.gap t.all SimpleBitFlag t.orientation
  | Sequential ->
          
 
(** Convert the alphabet to one with levels; this generates polymorphisms and
    codes under non-bitset situations. *)
and to_level level t =
  match t.alphabet with
  | CombinationLevels l when l = level -> t
  | Sequential when level = 1 -> t
  | Continuous
  | SimpleBitFlag
  | ExtendedBitFlag -> t
  | CombinationLevels _ -> to_level level (to_sequential t)
  | Sequential ->
    let combs,lsts = generate_combinational_elements ~level t.code_name in
    { t with
      comb_list = combs;
      list_comb = lsts;
      alphabet_type = CombinationLevels level; }
    

(** {6 Parsers for reading alphabets *)
