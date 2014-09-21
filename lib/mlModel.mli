(** Mlmodel - Represents a likelihood model of evolution containing all the
   standard parameters in tradiational applications with general variants. *)

(** Report General Error Messages for Model Creation through here *)
exception Error of string


(** {2 Types} *)

(** The distribution to define the rate-variability of sites in the model. *)
type site_var =
  (** Use [n] rate classes calculated from the means of the Gamma distribution. *)
  | DiscreteGamma of int * float
  (** Same as the DiscreteGamma model, with additional 0/invariable rate class. *)
  | DiscreteTheta of int * float * float
  (** Define a custom distribution from an array of [rate] and [probability] *)
  | DiscreteCustom of (float * float) array
  (** No site-variability. *)
  | Constant

(** Substitution Rate Models provided in the literature.
  
    {b References}
    + T. H. Jukes and C. R. Cantor. Evolution of protein molecules. In
      N. H. Munro, editor, Mammalian Protein Metabolism, pages 21–132.
      Academic Press, New York, 1969.
    + M. Kimura. A simple method for estimating evolutionary rate of base
      substitution through comparative studies of nucleotide sequences. J.
      Mol. Evol., 16:111–120, 1980.
    + J. Felsenstein. Evolutionary trees from dna sequences: a maximum
      likelihood approach. J. Mol. Evol., 17:368–376, 1981.
    + J. Felsenstein. Confidence limits on phylogenies: An approach using the
      bootstrap. Evolution, 39(4):783–791, 1985.
    + M. Hasegawa, H. Kishino, and T. Yano. A new molecular clock of
      mitochondrial dna and the evolution of hominoids. Proc. Japan Acad.,
      60:95–98, 1984.
    + H. Tamura and M. Nei. Estimation of the number of nucleotide sub-
      stitutions in the control region of mitochondrial dna in humans and
      chimpanzees. Mol. Biol. Evol., 10:512–526, 1993.
    + S. Tavaré. Some probabilistic and statistical problems on the analysis
      of dna sequences. Lec. Math. Life Sci., 17:57–86, 1986. *)
type subst_model =
  | JC69
  (** Has equal rate of site states with assumed equal priors. [1] *)
  | F81
  (** F81 has equal rate of site states with unequal priors. [3] *)
  | K2P of float
  (** Special case of TN93 wit additionally assumed equal priors. [2] *)
  | F84 of float
  (** Special case of TN93, implemented in DNAML. [4] *)
  | HKY85 of float
  (** Special case of TN93 where all transitions have the same rate. [5] *)
  | TN93 of (float * float)
  (** Transitions of pyrimadines and purines differ. [6] *)
  | GTR of float array
  (** Each transformation has it's own rate parameter. [7] *)
  | Const of float array array
  (** No optimization of parameters; matrix is normalized mean-rate=1 *)
  | Custom of (int Internal.IntMap.t * float array)
  (** The map is a pairing from element in a matrix->array index of floats *)


(** The prior probabilities of the model. *)
type priors =
  | Empirical of float array
  (** Priors are given by the user, by frequency of observation, or optimized
      (mle) as additional nuaces parameter through an optimization routine.*)
  | Equal
  (** Priors are equal to 1/[a], where [a] is the size of the alphabet. *)


(** An additional option for dealing with the possibility of indels as an
    additional state in the model. *)
type gap =
  | Missing
  (** Indels are defined the same as missing data *)
  | Coupled of float
  (** The gap is an additional character with common rate given. *)
  | Independent
  (** The gap is an addtional character with additional parameters given in the
      definition of the substitution model. *)


(** The specification of model; can be modified to re-create a new model and
    provides a one-to-one mapping from model to it's specification. *)
type s = {
  substitution : subst_model;
  site_variation : site_var;
  base_priors : priors;
  alphabet : Alphabet.t;
  gap : gap;
}

(** type alias for clarity in the interface *)
type matrix =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** type alias for clarity in the interface *)
type vector =
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Fully defined model of cost matrices decomposed for easy use and data-types
    presented in a fashion for C exposure. Left abstract, and should only be
    manipulated through the specification. *)
type t


(** {2 Matrix Diagonalization / Composition Functions} *)

(** [compose] compose a matrix from a model and a branch length. *)
val compose : t -> float -> matrix

(** [substitution_matrix m t?] return a substitution rate matrix optionally
    multiplied against a branch length [t]. This is the matrix Q in, [P=e^Q*t],
    or [Q*t] if [t] is optionally supplied. *)
val substitution_matrix : t -> float option -> matrix

(** [integerize_model \sigma m t] Generate a integerized matrix of precision
   [sigma]. This can be used to give speed-ups using alternate methods of
   diagnosis. The diagonal elements under likelihood have cost according to [t]. *)
val integerized_model : ?sigma:int -> t -> float -> int array array


(** {2 Creation and Enumeration functions of Models} *)

(** Create a model from a spec. *)
val create : s -> t

(** replace priors in a model with ones given and re-parameterize model if
    necessary (diagonalization, et cetera). *)
val replace_priors : t -> float array -> t

(** replace rate in a model and re-parameterize a model if necesary. *)
val replace_rates : t -> site_var -> t

(** replace the substitution rate matrix in the model and re-parameterize *)
val replace_subst : t -> subst_model -> t

(** Generate a function that enumerates the combinations of substitution models
    and site-rate-variation through lists of variants given and generate a new
    model from a previous model. *)
val enum_models :
  ?site_var:[`DiscreteGamma of int | `DiscreteTheta of int | `Constant
            | `DiscreteCustom of (float * float) array ]list ->
    ?subst_model:[`F81 | `F84 | `GTR | `HKY85 | `JC69 | `K2P | `TN93
                 | `Custom of int Internal.IntMap.t * float array] list ->
      ?priors:[`Empirical | `Equal] list ->
        ?gap:[`Missing | `Independent | `Coupled | `Indel] ->
          float array option -> Alphabet.t -> (unit -> s option)

(** [compute_priors (a,g) f (c,gc) ls] compute the priors of data from an array
    of base frequencies [f], but predicated on gap being an additional state we
    also include the indel prior from the number of indels [gc] and the minimum
    number of indels required to align all data from [ls]. *)
val compute_priors :
  Alphabet.t * bool -> float array -> int * int -> int list -> float array


(** {2 Query functions of models} *)

(** [get_alphabet] return the alphabet of the model *)
val get_alphabet : t -> Alphabet.t

(** [alphabet_size] return the width of the matrices in the model; this includes
   the gap-as-character situation if those options are set. *)
val alphabet_size : t -> int

(** [num_parameters] return the number of variables used to parameterize the model. *)
val num_parameters : t -> int

(** [gamma_rates] return the gamma rate classes generated from alpha, beta and
    number of categories; for testing, but can be used directly. *)
val gamma_rates : float -> float -> int -> vector

(** [get_spec] returns the specification of a model *)
val get_spec : t -> s

(** {2 Compare / Higher-Order Data-Types} *)

(** [compare a b] compare model [a] and [b] *)
val compare : t -> t -> bool

(** [MlModelMap] is a map of values implemented from compare function above. *)
module MlModelMap : Map.S with type key = s

(** [MlModelSet] is a set of values implemented from compare function above. *)
module MlModelSet : Set.S with type elt = s

(** [categorize_by_model] categorize a list of values into lists of differing
    types using the compare function and Map as a container. The function passed
    obtains the model from the passed values for inclusion. *)
val categorize_by_model : ('a -> t) -> 'a list -> 'a list list

(** [process_custom_matrix] process data to be used by the Custom model type.
    Create a custom model by a Map and array. The map is of the char code of the
    elements of the matrix to a index in an array. This can be used with the
    Custom tag for a model. *)
val process_custom_matrix :
  int -> char array array -> int Internal.IntMap.t * float array


(** {2 I/O Functions} *)

(** [short_name model] gives a short and sweet name of the models initials and
   it's rate information. Like JC69+G (for a jukes-cantor model with gamma). *)
val short_name : t -> string


(** {2 Substitution Rate Matrix Estimation} *)

(** We provide functions for counting and creating matrices from a
    classification of transformations in a data-set. *)

(** Create a specification from a classification, and parsed model details. *)
val process_classification :
  s -> float Internal.UnorderedTupleMap.t * float Internal.IntMap.t -> s


(* extra functions that do not need to be exposed
val diagonalize_gtr : matrix -> matrix -> matrix -> unit
val diagonalize_sym : matrix -> matrix -> unit
val compose_gtr : matrix -> matrix -> matrix -> float -> matrix
val compose_sym : matrix -> matrix -> float -> matrix
val diagonalize : bool -> matrix -> matrix * matrix * matrix option
val compose_matrix : matrix -> float -> matrix

type gap_repr = (int * float) option

val m_jc69 : int -> gap_repr -> matrix
val m_k2p : float -> int -> gap_repr -> matrix
val m_tn93 : vector -> float -> float -> int -> gap_repr -> matrix
val m_f81 : vector -> int -> gap_repr -> matrix
val m_hky85 : vector -> float -> int -> gap_repr -> matrix
val m_f84 : vector -> float -> int -> gap_repr -> matrix
val m_gtr : vector -> float array -> int -> gap_repr -> matrix
val m_file : vector -> float array array -> int -> matrix
val m_custom : vector -> int Internal.IntMap.t -> float array -> int -> matrix *)
