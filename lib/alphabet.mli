(** {2 Definition of an Alphabet} *)

type kind =
  | Sequential (** Numbers define states. Should be 0 -- (n-1) *)
  | SimpleBitFlag (** Set bits in numbers define states. *)
  | ExtendedBitFlag (** Same as bitflag with additional character states for polymorphisms. *)
  | Continuous (** A continuous alphabet of values; the states define the alphabet. *)
  | CombinationLevels of int (** A sequential alphabet with additional states for polymorphisms. *)

(** An alphabet stores the character codes, states, their compliments, the type
    of alphabet and some other basic information. comb_set/set_comb are used in
    sequential alphabets only, to be used to associate a sequential state to
    a set of sequential state that cannot be transformed easily (unlike bitsets). *)
type t =
  { comb_set : Internal.IntSet.t Internal.IntMap.t; (** Combination Code -> States *)
    set_comb : int Internal.IntSetMap.t;            (** States -> Combination Code *)
    name_code : int Internal.StringMap.t;           (** Single Code -> Name *)
    code_name : string Internal.IntMap.t;           (** Name -> Single Code *)
    comp_code : int Internal.IntMap.t;              (** Code -> Compliment of Code *)
    alphabet_type : kind;    (** Type of the alphbet *)
    size : int;              (** Size of the basic alphabet; excludes gap *)
    full_size : int;         (** Size of associated matrix *)
    orientation : bool;      (** If cost(~x,x) = cost(x,x) + O(n) *)
    gap : int option;        (** Code for the gap-character; if present *)
    missing: int option;     (** Code for the missing-character; if present *)
    all : int option;        (** Code for the all-character; if present *)
  }



(** {2 Constants} *)

(** default gap representation is '-'. *)
val default_gap : string

(** default missing represenation is '?'. *)
val default_missing : string

(** default prefix to denote orientation is '~'. *)
val default_orientation : string



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
    further characters from IUPAC that associates polymorphisms with gaps. *)
val nucleotides : t

(** Define a Sequential alphabet for the amino-acids. The default missing is
 * over-loaded inthis data-set to be "X". *)
val aminoacids : t
 
(** A binary character for the absence and presence of a character. 0 for absent
    and 1 for present.*)
val present_absent : t



(** {2 Functions for querying alphabets} *)

(** get the code associated with the name of the character *)
val get_code : string -> t -> int

(** Return the name of the character code *)
val get_name : Internal.IntMap.key -> t -> string

(** return the gap character *)
val get_gap : t -> int

(** return if the alphabet has a gap *)
val has_gap : t -> bool

(** return the size of the alphabet *)
val size : t -> int

(** return if orientation is used *)
val orientation : t -> bool

(** return the all element if it exists *)
val all_char : t -> int option

(** return the type of the alphabet *)
val kind : t -> kind

(** return if the alphabet is state identified *)
val is_statebased : t -> bool

(** return if the alphabet is bit identified *)
val is_bitset : t -> bool

(** get the compliment of the character code *)
val complement : Internal.IntMap.key -> t -> int option

(** determines if two elements in the alphabet are complements *)
val is_complement : Internal.IntMap.key -> Internal.IntMap.key -> t -> bool

(** return the list of states that represent a code *)
val get_combination : Internal.IntMap.key -> t -> Internal.IntSet.t

(** Opposite of the above function *)
val get_state_combination : Internal.IntSet.t -> t -> int

(** Generate an alphabet from a list of states (NAME,CODE,COMPLIMENT). *)
val of_list :
  (string * Internal.IntMap.key * int option) list ->
    int option -> int option -> int option -> kind -> bool -> t

(** Convert an alphabet to a list of the main properties *)
val to_list : t -> (Internal.StringMap.key * Internal.IntMap.key * int option) list



(** {2 Converting between types of alphabets} *)

(** convert alphabet to a sequentially ordered alphabet; remove combination if
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


(** {2 Debugging} *)

val print : t -> unit


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
  ]

  (** Conver the error messages to something human readable. *)
  val to_string : t -> string
end

exception Error of Error.t
