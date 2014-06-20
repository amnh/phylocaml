(** This provides a point between the user and the software to translate data to
    the proper encoding. *)

(** Defines codes in the alphabet. This code can be either bitpacked, packed in
    some specific ordering, or literally define the state depending on the kind
    of alphabet. Using the interface to decrypt the states is recommended. *)
type code = int

module CodeMap    : Map.S with type key = code
module CodeSet    : Set.S with type elt = code
module CodeSetMap : Map.S with type key = CodeSet.t

(** combinations define a bijective map for a code and a set of atomic states *)
type combinations =
 {  comb_set : CodeSet.t CodeMap.t;
    set_comb : code CodeSetMap.t;
 }

(** Record of default characters for parsing. *)
(* type symbols = {
  gap : string;
  missing : string;
  orientation : string;
  separators : string list;
  containers : (string * string) list;
} *)

(** Defines the type of alphabet.*)
type kind =
  | Sequential
    (** Numbers define states. Indexed from, 0 -- (n-1) *)
  | BitFlag
    (** Set bits in numbers define states. Requires 2^s bits. *)
  | Continuous
    (** A continuous alphabet of values; the states define the alphabet. *)
  | CombinationLevels of int
    (** A sequential alphabet with additional states for polymorphisms. *)

(** An alphabet stores the character codes, states, their compliments, the type
    of alphabet and some other basic information. comb_set/set_comb are used in
    sequential alphabets only, to be used to associate a sequential state to
    a set of sequential states that cannot be transformed easily (unlike bitsets). *)
type t =
  { kind      : kind;         (** Type of the alphabet *)
    atomic    : CodeSet.t;    (** Return a set of the atomic elements *)
    name_code : code Internal.StringMap.t;  (** Single Code -> Name *)
    code_name : string CodeMap.t;           (** Name -> Single Code *)
    compliment: int CodeMap.t;(** Code -> Compliment of Code *)
    comb_data : combinations; (** Holds combination information. *)
    gap       : int option;   (** Code for the gap-character; if present *)
    missing   : int option;   (** Code for the missing-character; if present *)
    all       : int option;   (** Code for the all-character; if present *)
    case      : bool;         (** Does case matter in parsing/analysis? *)
    orientation : bool;       (** If ~ symbol should be used to parse *)
  }


(** {2 Basic Alphabets} *)

(** A Continuous alphabet does not have character states and an unbounded size.
    It must be dealt with differently in most situations. *)
val continuous : t

(** Basic DNA alphabet; the simple bit flag annotation implies that
    polymorphisms of characters are not defined in the alphabet. The default
    missing is over-loaded in this data-set to be "X". *)
val dna : t

(** Define an extended-bit-flag representation of DNA that carries with it
    associations of polymorphisms to single characters. This also includes
    further characters then IUPAC that associates polymorphisms with gap. *)
val nucleotides : t

(** Define a Sequential alphabet for the amino-acids. The default missing is
    over-loaded in this data-set to be "X". *)
val aminoacids : t
 
(** A binary character for the absence and presence of a character. 0 for absent
    and 1 for present.*)
val present_absent : t

(** Generate a sequential alphabet of [n] states. The names of the states are
    their prefix-free string representation of their code. Thus, "00" --> 0 for
    an alphabet of size <= 99. The default for gap is true, and will add an
    additional element to the data. A gap is enabled by default, while missing
    is not. All element can not be specified. *)
val generate_seq_alphabet : ?gap:bool -> ?missing:bool -> int -> t


(** {2 Basic Functions} *)

(** Get the code associated with the name of the character *)
val get_code : string -> t -> code

(** Return the name of the character code *)
val get_name : code -> t -> string

(** Return if the alphabet is state identified. Combination, Continuous, and
    Sequential alphabets are statebased. *)
val is_statebased : t -> bool

(** Return if the alphabet is bit identified. *)
val is_bitset : t -> bool

(** Get the compliment of the character code *)
val complement : code -> t -> code option

(** Determines if two elements in the alphabet are complements *)
val is_complement : code -> code -> t -> bool

(** Return the size of the alphabet as defined by the number of atomic states *)
val size : t -> int

(** {2 Functions on Polymorphisms} *)

(** Determines if a state is atomic *)
val is_atomic : code -> t -> bool

(** Return the list of states that represent a code; if the code is atomic
    return a singleton of the code. *)
val get_combination : code -> t -> CodeSet.t

(** Return a code that represents a set of codes *)
val get_state_combination : CodeSet.t -> t -> code option

(** Take a set of states and return a single state that represents their
    combination if they exist within the alphabet. *)
val compress_polymorphisms : code list -> t -> code option

(** Take a set of states and return a single state that represents their
    combination, if the combination extends beyond the number of combinations
    allowed within the polymorphism, we take the largest (in number of
    states represented) minimal (based on ordering of the states) set. *)
val choose_polymorphism : code list -> t -> code


(** {2 Creating alphabets} *)

(** [sequential_alphabet states equates gap all missing orientation case]
    Creates an alphabet encoded in sequential codes, that is 0,1,2,3...(n-1). No
    polymorphisms exist as a single code. *)
val sequential_alphabet :
  states : (string * string option) list -> equates : (string * string list) list ->
    gap : string option -> all : string option -> missing : string option ->
      orientation: bool -> case:bool -> t

(** [combination_alphabet equates gap all missing orientation case level] 
    Creates a combination sequential alphabet. It expands the sequential
    alphabet with polymorphisms up to the [level] argument. *)
val combination_alphabet :
  states : (string * string option) list -> equates : (string * string list) list ->
    gap : string option -> all : string option -> missing : string option ->
      orientation: bool -> case:bool -> level:int -> t

(** [bitflag_alphabet states equates gap all missing orientation case]
    Creates a bit-packed alphabet. The number of states are dependent on the OS,
    31 or 63 elements. *)
val bitflag_alphabet :
  states : (string * string option) list -> equates : (string * string list) list ->
    gap : string option -> all : string option -> missing : string option ->
      orientation: bool -> case:bool -> t


(** {2 Comparison Functions} *)

(** Compare two alphabets returning true if the atomic elements of the alphabets
    exist in each. The alphabets could have different encodings, but as long as
    the names of the atomic elements are the same along with the gap, missing,
    all and orientation properties this function returns true.  *)
val compare_elts : t -> t -> bool

(** Compare of all the properties and exact encodings of the elements *)
val compare : t -> t -> bool



(** {2 Converting between types of alphabets} *)

(** Convert alphabet to a sequentially ordered alphabet; remove combination if
    they exist in the alphabet, continuous alphabets are unchanged. *)
val to_sequential : t -> t

(** Convert the alphabet to a simple bit encoding format. Limited to
    transforming alphabets < 31/63bits, depending on OS. *)
val to_bitflag : t -> t

(** Simplify turns the alphabet to one of the following based on it's current
    state: bitflag, sequential, or continuous *)
val simplify : t -> t

(** Convert the alphabet to one with levels; this generates polymorphisms and
    codes under non-bitset situations. *)
val to_level : int -> t -> t


(** {2 Parsing Data} *)

(** default gap, missing, all, container and higher-level separator characters
    for parsing data. *)
(* val default_symbols : symbols *)

(* val parse_data_stream : t -> ('a -> int -> code -> 'a) -> in_channel -> 'a -> 'a *)


(** {2 Debugging} *)

(** Prints basic information of the alphabet. *)
val dump : t -> unit

val pp_alphabet : Format.formatter -> t -> unit


(** {2 Error Module} *)

module Error : sig

  (** Defines the types of errors that this module can raise. *)
  type t = [
    | `Missing_State_Sequential_Alphabet of int
      (** An element in a sequential alphabet is missing (indexed at 0) *)
    | `Missing_Name_Sequential_Alphabet of int
      (** A code was found, but the corresponding name is missing. *)
    | `Alphabet_Size_Expectation of int * int
      (** The calculated alphabet sized expected [a], but found [b] elements. *)
    | `Missing_Gap_Element of int
      (** The alphabet expected gap [a] to be in the set of codes. *)
    | `No_Gap_Character_Found of string
      (** The alphabet expected gap [a] to be in the set of names. *)
    | `No_All_Character_Found of string
      (** The alphabet expected all character [a] to be in the set of names. *)
    | `No_Missing_Character_Found of string
      (** The alphabet expected missing character [a] to be in the set of names. *)
      (** The alphabet expected [a] to be in the set of codes. *)
    | `Complement_Not_Bijective of int * int
      (** [a] = f([b]) === [b] = f([a]), where [f] is complement function *)
    | `Polymorphisms_In_Sequential_Alphabet
      (** Sequential alphabet does not have polymorphisms. *)
    | `Polymorphisms_In_Continuous_Alphabet
      (** Continuous alphabet does not have polymorphisms. *)
    | `Gap_Not_Atomic_BitFlag_Alphabet of int
      (** Gap character in BitFlag has multiple bits set. *)
    | `Unacceptable_Level_Argument of int
      (** When level is given <= 0 *)
    | `Illegal_Character of string
      (** Character name not found in alphabet *)
    | `Illegal_Code of int
      (** Character code not found in alphabet *)
    | `Alphabet_Size_Too_Large_For_BitFlag of int
      (** If the alphabet size is too large to convert to bitflags. *)
    | `Insufficient_Level_To_Represent_States of int * Internal.IntSet.t
      (** If the state is out of range of the level *)
  ]

  (** Convert the error messages to something more human readable. *)
  val to_string : t -> string

end

exception Error of Error.t
