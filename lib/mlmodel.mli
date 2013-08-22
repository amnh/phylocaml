(** Mlmodel - Represents a likelihood model of evolution containing all the
   standard parameters in tradiational applications with general variants. *)

(** Report General Error Messages for Model Creation through here *)
exception ModelError of string


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
type spec = {
  substitution : subst_model;
  site_variation : site_var;
  base_priors : priors;
  alphabet : Alphabet.t * gap;
}


(** Fully defined model of cost matrices decomposed for easy use and data-types
    presented in a fashion for C exposure. *)
type model = {
  spec : spec;
  (** The specification that created this model. *)
  priors : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  (** The vector of prior probabilities. SUM=1 *)
  pinvar : float option;
  (** Percent invar for the Theta model of invariant sites in evolution. *)
  rates : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  (** The rates for each class in Discrete Rate distributions. *)
  probs : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  (** The probability of each rate-class for Discrete Rate Distributions. *)
  q : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  (** [q] The Substitution-Rate Matrix. *)
  u : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  (** [u] The right-eigenvectors of the decomposed [q] matrix. *)
  d : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  (** [d] the eigen-values as a diagonal matrix of the decomposed [q] matrix. *)
  ui : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t option;
  (** [ui] the inverse of the [u] matrix; optional in cases where [ui = ut]
      (transpose = inverse in symmetric matrices) *)
  opt : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  (** A vector of parameters to optimized; this is gathered from the model
      specification for easy optimization and update of model parameters. *)
}



(** {2 Matrix Diagonalization / Composition Functions} *)

(** [diagonalize_gtr U D Ui] diagonalize [U] into [U] [D] and [Ui], [U] is
    modified in this function call. [U] must be similar to a symmetric matrix,
    as this routine expects no imaginary eigen-values. GTR matrices with unequal
    priors are of this category.

    {b References}
    + Keilson J. Markov Chain Models–Rarity and Exponentiality.
      New York: Springer-Verlag; 1979.  *)
val diagonalize_gtr :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t -> unit

(** [diagonalize U D Ui] diagonalize [U] into [U] [D], [U] is
    modified in this function call. In this case, Ut = Ui (transpose = inverse). *)
val diagonalize_sym :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t -> unit

(** [compose_gtr U D Ui t] compose the construction of probability rate matrix
    [P] from a decomposed matrix [Q=U*D*Ui], where [P=e^Q*t]. *)
val compose_gtr :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  float -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [compose_sym U D Ui t] compose the construction of probability rate matrix
    [P] from a decomposed matrix [Q=U*D*Ui], where [P=e^Q*t]. In this case, [Ui]
    is unneccessary since [Ut = Ui]. *)
val compose_sym :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  float -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [diagonalize sym Q] diagonalize matrix [Q]; [sym] decides if the function
    should use symmetric or general routines for diagonalization. *)
val diagonalize :
  bool -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t *
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t *
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t option

(** [compose] compose a matrix from a model and a branch length. *)
val compose :
  model -> float ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [subst_matrix m t?] return a substitution rate matrix optionally multiplied
    against a branch length [t]. This is the matrix Q in, [P=e^Q*t], or [Q*t] if
        [t] is optionally supplied. *)
val substitution_matrix :
  model -> float option ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [compose_model Q t] compose the construction of a probability rate matrix
    directly from a substitution rate matrix and time period/branch length. *) 
val compose_model :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
    float -> (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

(** [integerize_model \sigma m t] Generate a integerized matrix of precision
   [sigma]. This can be used to give speed-ups using alternate methods of
   diagnosis. The diagonal elements under likelihood have cost according to [t]. *)
val integerized_model : ?sigma:int -> model -> float -> int array array



(** {2 Creation and Iteration functions of Models} *)

(** Create a model from a spec. *)
val create : spec -> model

(** replace priors in a model with ones given and re-parameterize model if
    necessary (diagonalization, et cetera). *)
val replace_priors : model -> float array -> model

(** replace rate in a model and re-parameterize a model if necesary. *)
val replace_rates : model -> site_var -> model

(** replace the substitution rate matrix in the model and re-parameterize *)
val replace_subst : model -> subst_model -> model

(** Generate a function that enumerates the combinations of substitution models
    and site-rate-variation through lists of variants given and generate a new
    model from a previous model. Empirical priors are required if the given model
    does not have them (under JC69, K80), else priors will always be equal. *)
val enum_models :
  ?site_var:[`DiscreteGamma | `DiscreteTheta | `Constant] list -> 
    ?subst_model:[`F81 | `F84 | `GTR | `HKY85 | `JC69 | `K2P | `TN93] list ->
      ?priors:float array -> (model -> model option)

(** [compuate_priors (a,g) f (c,gc) ls] compute the priors of data from an array
    of base frequencies [f], but predicated on gap being an additional state we
    also include the indel prior from the number of indels [gc] and the minimum
    number of indels required to align all data from [ls]. *)
val compute_priors :
  Alphabet.t * bool -> float array -> int * int -> int list -> float array


(** {2 Query functions of models} *)

(** [get_alphabet] return the alphabet of the specification *)
val get_alphabet : spec -> Alphabet.t

(** [alphabet_size] return the width of the matrices in the model; this includes
   the gap-as-character situation if those options are set. *)
val alphabet_size : spec -> int

(** [num_parameters] return the number of variables used to parameterize the model. *)
val num_parameters : model -> int

(** [gamma_rates a b i] return the different rates in each class of a gamma
    distribution with shape paramter [a], scale parameter [b] and [i] classes. *)
val gamma_rates :
  float -> float -> int ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t



(** {2 Compare / Higher-Order Data-Types} *)

(** [compare a b] compare model [a] and [b] *)
val compare : model -> model -> bool

(** [MlModelMap] is a map of values implemented from compare function above. *)
module MlModelMap : Map.S with type key = spec

(** [MlModelSet] is a set of values implemented from compare function above. *)
module MlModelSet : Set.S with type elt = spec

(** [categorize_by_model] categorize a list of values into lists of differing
   types using the compare function and Map as a container. The function passed
   obtains the model from the passed values for inclusion. *)
val categorize_by_model : ('a -> model) -> 'a list -> 'a list list



(** {2 Substitution Rate Matrix Constructions} *)

(** A type to represent how gaps should be treated when creating substitution
    rate matrices. *)
type gap_repr =
  (int * float) option

val m_jc69 :
  int -> gap_repr ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_k2p :
  float -> int -> gap_repr ->
    (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_tn93 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float -> float -> int -> gap_repr ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_f81 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    int -> gap_repr ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_hky85 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float -> int -> gap_repr ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_f84 :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float -> int -> gap_repr ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_gtr :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float array -> int -> gap_repr ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_file :
  (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    float array array -> int ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

val m_custom :
  (float, 'a, 'b) Bigarray.Array1.t ->
    int Internal.IntMap.t -> float array -> int ->
      (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t


(** {2 I/O Functions} *)

(** [output model str_chan table_chan nexus model char_sets]
    Output the model in a [POY Nexus] of [Phylip] like output format to
    [str_chan] and if available a table-formatted channel [table_chan].
    [char_sets] are associations of characters to this output block for [POY
    Nexus] formatting. *)
val output_model :
  (string -> unit) -> (string array array -> unit) option
    -> [`Nexus | `Phylip] -> model -> string list option -> unit

(** [short_name model] gives a short and sweet name of the models initials and
   it's rate information. Like JC69+G (for a jukes-cantor model with gamma). *)
val short_name : model -> string


(** {2 Substitution Rate Matrix Estimation} *)

(** We provide functions for counting and creating matrices from a
    classification of transformations in a data-set. *)

(** Create a specification from a classification, and parsed model details. *)
val process_classification :
  spec -> float Internal.UnorderedTupleMap.t * float Internal.IntMap.t -> spec

(** [classify_edges leaf1? leaf2? data1 data2 (acc1,acc2)]
    Create an accumulated classification of transformations between [data1] and
    [data2]. We also store site information for empirical priors in [acc2] when
    [leaf1] or [leaf2] are set to true. [data1] and [data2] are lists of pairs of
    the weight of the character and the states assigned to that data. This
    function can be used to estimate the initial rates from parsimony trees. *)
val classify_edges :
  bool -> bool -> (float * Internal.BitSet.t) list -> (float * Internal.BitSet.t) list ->
    float Internal.UnorderedTupleMap.t * float Internal.IntMap.t ->
      float Internal.UnorderedTupleMap.t * float Internal.IntMap.t

(** {2 Parser Friendly Functions} *)

(*
type string_spec =
  string * (string * string * string * string) * float list * (string * float option) * string option

val convert_string_spec : Alphabet.t -> string_spec -> spec

val convert_methods_spec :
  Alphabet.t * int ->
  (unit -> float array) ->
  [< `Int of int | `Max | `Min ] *
  [< `Custom of string
   | `F81
   | `F84 of float list
   | `File of string
   | `GTR of float list
   | `HKY85 of float list
   | `JC69
   | `K2P of float list
   | `TN93 of float list
   > `JC69 `K2P ] *
  [< `Gamma of int * float option | `Theta of int * (float * float) option ]
  option * [< `Consistent | `Equal | `Estimate | `Given of float list ] * 
  gap -> spec
*)


