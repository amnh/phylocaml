(** {2 Definition of an Alphabet} This provides point between the user and the
    software to translate data to the proper encoding. The input data to generate
    an alphabet using of_list is in a common format of (NAME,CODE,COMP). The CODE
    should be appropriate to the alphabet type. *)

(** How codes in the alphabet are represented; as an integer. This code an be
    either bitpacked, packed in some specific ordering, or literally define the
    state depending on the kind of alphabet. Using the interface to decrypt the
    states is recommended. *)
type code = int

module CodeMap    : Map.S with type key = int
module CodeSet    : Set.S with type elt = int
module CodeSetMap : Map.S with type key = CodeSet.t

(** Define additional properties for combinations. *)
type combinations =
 {  comb_set : CodeSet.t CodeMap.t;
    set_comb : code CodeSetMap.t;
    level    : int;
 }

(** Defines the type of alphabet that has been processed or will be processed.*)
type kind =
  | Sequential
    (** Numbers define states. Should be 0 -- (n-1) *)
  | BitFlag
    (** Set bits in numbers define states. *)
  | Continuous
    (** A continuous alphabet of values; the states define the alphabet. *)
  | CombinationLevels of int
    (** A sequential alphabet with additional states for polymorphisms. *)

(** An alphabet stores the character codes, states, their compliments, the type
    of alphabet and some other basic information. comb_set/set_comb are used in
    sequential alphabets only, to be used to associate a sequential state to
    a set of sequential state that cannot be transformed easily (unlike bitsets). *)
type t =
  { kind      : kind;         (** Type of the alphabet *)
    name_code : code Internal.StringMap.t;  (** Single Code -> Name *)
    code_name : string CodeMap.t;  (** Name -> Single Code *)
    comp_code : int CodeMap.t;(** Code -> Compliment of Code *)
    size      : int;          (** Size of the basic alphabet; excludes gap *)
    orientation : bool;       (** If ~ symbol should be used to parse *)
    gap       : int option;   (** Code for the gap-character; if present *)
    missing   : int option;   (** Code for the missing-character; if present *)
    all       : int option;   (** Code for the all-character; if present *)
    case      : bool;         (** Does case matter in parsing/analysis? *)
    comb_data : combinations; (** Holds combination information. *)
  }



(** {2 Constants} *)

(** default gap representation is '-'. *)
val default_gap : string

(** default missing represenation is '?'. *)
val default_missing : string

(** default prefix to denote orientation is '~'. *)
val default_orientation : string

(** default characters that define higher-level separations of data. *)
val default_separators : string list



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



(** {2 Functions for querying alphabets} *)

(** Get the code associated with the name of the character *)
val get_code : string -> t -> code

(** Return the name of the character code *)
val get_name : code -> t -> string

(** Return the gap character *)
val get_gap : t -> code

(** Return if the alphabet has a gap *)
val has_gap : t -> bool

(** Return the size of the alphabet *)
val size : t -> int

(** Return if orientation is used *)
val orientation : t -> bool

(** Return the all element if it exists *)
val get_all : t -> code

(** Return if the element has an all element *)
val has_all : t -> bool

(** Return the type of the alphabet *)
val kind : t -> kind

(** Return if the alphabet is state identified. Combination, Continuous, and
    Sequential alphabets are statebased. *)
val is_statebased : t -> bool

(** Return if the alphabet is bit identified. *)
val is_bitset : t -> bool

(** Get the compliment of the character code *)
val complement : int -> t -> int option

(** Determines if two elements in the alphabet are complements *)
val is_complement : code -> code -> t -> bool

(** Return the list of states that represent a code *)
val get_combination : code -> t -> CodeSet.t

(** Opposite of the above function *)
val get_state_combination : CodeSet.t -> t -> code

(** [of_list states equates gap all missing orientation case kind] Generate an
    alphabet from a list of [states] in the form [(NAME,COMPLIMENT OPTION)].
    Gap, all element, and missing are included if they exist as well as the type
    of alphabet to create, and if orientation and cse should be considered for
    parsing the data. Equates are stored in the name->code field, while states
    are stored in both. This allows coincident names to exist in parsing but not
    in the output stream of data. *)
val of_list :
  states : (string * string option) list -> equates : (string * string list) list ->
    gap : string option -> all : string option -> missing : string option ->
      orientation: bool -> case:bool -> kind:kind -> t

(** Convert an alphabet to a list of the main properties like [data] in the
    [of_list] function. This, once filtered of the middle element in the tuple,
    can be used to re-initialize the alphabet, although equates and indel
    details are missing. *)
val to_list : t -> (string * code * string option) list



(** {2 Converting between types of alphabets} *)

(** Convert alphabet to a sequentially ordered alphabet; remove combination if
    they exist in the alphabet, continuous alphabets are unchanged. *)
val to_sequential : t -> t

(** Convert the alphabet to a simple bit encoding format. This removes extra
    polymorphic states; limited to transforming alphabets < 63bits. *)
val to_bitflag : t -> t

(** Simplify turns the alphabet to one of the following based on it's current
    state: simplebitflag, sequential, or continuous *)
val simplify : t -> t

(** Convert the alphabet to one with levels; this generates polymorphisms and
    codes under non-bitset situations. *)
val to_level : int -> t -> t


(** {2 Parsing Data} *)

(* val parse_data_stream : t -> in_channel -> int array *)


(** {2 Debugging} *)

(** Prints basic information of the alphabet. *)
val dump : t -> unit

(** pretty-printer / debugger formatter *)
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
      (** The alphabet expected [a] to be in the set of codes. *)
    | `Complement_Not_Transitive of int * int
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
    | `Not_found
      (** If the alphabet size is too large to convert to bitflags. *)
    | `Alphabet_Size_Too_Large_For_BitFlag of int
  ]

  (** Convert the error messages to something human readable. *)
  val to_string : t -> string

end

exception Error of Error.t
