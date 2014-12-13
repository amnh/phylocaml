open Internal

exception Error of string

let errorf format = Printf.ksprintf (fun x -> Error x) format

(* Minimum value when 0 or min_float are numerically unstable *)
let minimum = 1e-13

type site_var =
  | DiscreteGamma of int * float
  | DiscreteTheta of int * float * float
  | DiscreteCustom of (float * float) array
  | Constant

type subst_model =
  | JC69
  | F81
  | K2P    of float
  | F84    of float
  | HKY85  of float
  | TN93   of (float * float)
  | GTR    of float array
  | Const  of float array array
  | Custom of (int IntMap.t * float array)

let default_rate = 1.0

type priors = 
  | Empirical of float array
  | Equal

type gap =
  | Missing
  | Coupled of float
  | Independent

type s = {
  substitution : subst_model;
  site_variation : site_var;
  base_priors : priors;
  alphabet : Alphabet.t;
  gap : gap;
}

let default_alpha = 0.1
and default_invar = 0.1
and default_tstv  = 2.0

type vector = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t
type matrix = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t

type t = {
  spec  : s;
  priors: vector;
  pinvar: float option;
  rates : vector;
  probs : vector;
  q     : matrix;
  u     : matrix;
  d     : matrix;
  ui    : matrix option;
}

(** [diagonalize_gtr U D Ui] diagonalize [U] into [U] [D] and [Ui], [U] is
    modified in this function call. [U] must be similar to a symmetric matrix,
    as this routine expects no imaginary eigen-values. GTR matrices with unequal
    priors are of this category.

    {b References}
    + Keilson J. Markov Chain Models–Rarity and Exponentiality.
      New York: Springer-Verlag; 1979. *)
external diagonalize_gtr: (* U D Ui *)
  matrix -> matrix -> matrix -> unit = "likelihood_CAML_diagonalize_gtr"

(** [diagonalize U D Ui] diagonalize [U] into [U] [D], [U] is
    modified in this function call. In this case, Ut = Ui (transpose = inverse). *)
external diagonalize_sym: (* U D *)
  matrix -> matrix -> unit = "likelihood_CAML_diagonalize_sym"

(** [compose_gtr U D Ui t] compose the construction of probability rate matrix
    [P] from a decomposed matrix [Q=U*D*Ui], where [P=e^Q*t]. *)
external compose_gtr: (* U D Ui t -> P^e^t *)
  matrix -> matrix -> matrix -> float -> matrix = "likelihood_CAML_compose_gtr"

(** [compose_sym U D Ui t] compose the construction of probability rate matrix
    [P] from a decomposed matrix [Q=U*D*Ui], where [P=e^Q*t]. In this case, [Ui]
    is unneccessary since [Ut = Ui]. *)
external compose_sym: (* U D t -> P^e^t *)
  matrix -> matrix -> float -> matrix = "likelihood_CAML_compose_sym"

(** Generate the GAMMA RATE classes *)
let gamma_rates a b len =
  let dist = Pareto.Distributions.Gamma.create ~shape:a ~scale:b in
  Array.init len
    (fun i ->
      let p = (float_of_int i) /. (float_of_int len) in
      Pareto.Distributions.Gamma.quantile dist ~p)
    |> ba_of_array1

(** Return the alphabet for the characters *)
let get_alphabet t = t.spec.alphabet

(** Return the specificaiton of the model *)
let get_spec t = t.spec

(** Return the size of the alphabet under analysis  *)
let alphabet_size t =
  let g = match t.spec.gap with
    | Missing -> 0
    | Coupled _  | Independent -> 1
  in
  g + (Alphabet.size t.spec.alphabet)

(** Compare two models specification and the states of parameters *)
let compare a b =
  let a_asize = alphabet_size a
  and b_asize = alphabet_size b in
  let compare_array x y =
    if Array.length x != Array.length y then
      false
    else begin
      let results = ref true in
      for i = 0 to (Array.length x) - 1 do
        results := !results && (x.(i) =. y.(i));
      done;
      !results
    end
  in
  let compare_priors a b = match a.spec.base_priors,b.spec.base_priors with
    | _ when a_asize != b_asize-> false
    | Equal , Equal            -> true
    | Empirical x, Empirical y -> compare_array x y
    | (Equal | Empirical _ ), _-> false
  in
  let m_compare = match a.spec.substitution,b.spec.substitution with
    | JC69 , JC69 | F81 , F81 -> true
    | K2P x, K2P y | F84 x, F84 y | HKY85 x, HKY85 y when x = y -> true
    | Custom (a1,r1), Custom (a2,r2) ->
      (compare_array r1 r2) && 
        (IntMap.fold
          (fun k v acc ->
            if IntMap.mem k a2
              then acc && (v = IntMap.find k a2)
              else false)
          a1 true)
    | TN93 (x1,x2),  TN93 (y1,y2) when x1 = y1 && x2 = y2 -> true
    | GTR xs, GTR ys when compare_array xs ys -> true
    | Const x, Const y when x = y -> true
    | (JC69|F81|K2P _|F84 _|HKY85 _|Custom _|TN93 _|GTR _|Const _),_ -> false
  and v_compare = match a.spec.site_variation,b.spec.site_variation with
    | DiscreteGamma (ix,ax), DiscreteGamma (iy,ay) -> ix=iy && ax=ay
    | DiscreteTheta (ix,ax,bx), DiscreteTheta (iy,ay,by) -> ix=iy && ax=ay && bx=by
    | Constant, Constant -> true
    | DiscreteCustom a, DiscreteCustom b ->
      array_fold_left2 (fun acc (a,b) (c,d) -> (a =. c) && (b =. d) && acc) true a b
    | (DiscreteGamma _|DiscreteCustom _ |DiscreteTheta _|Constant), _ -> false
  and g_compare = match a.spec.gap,b.spec.gap with
    | Missing, Missing
    | Independent, Independent -> true
    | Coupled x, Coupled y when x=y-> true
    | (Missing | Independent | Coupled _), _ -> false
  and p_compare = compare_priors a b in
  m_compare && v_compare && g_compare && p_compare
 

module OrderedML = struct
  type t = s
  let compare a b = Pervasives.compare a b
end
module MlModelMap = Map.Make (OrderedML)
module MlModelSet = Set.Make (OrderedML)

(** Categorize a list of codes by model *)
let categorize_by_model get_fn codes =
  let set_codes,non_lk_codes =
    List.fold_left
      (fun (acc,oth) code ->
        try let spec = (get_fn code).spec in
          try let old = MlModelMap.find spec acc in
            MlModelMap.add spec (code::old) acc,oth
          with Not_found ->
            MlModelMap.add spec ([code]) acc,oth
        with _ -> (acc,code::oth))
      (MlModelMap.empty,[])
      codes
  in
  let init = match non_lk_codes with | [] -> [] | xs -> [xs] in
  MlModelMap.fold (fun _ e a -> e :: a) set_codes init


(** Count the number of parameters in the model; used for xIC functions **)
let num_parameters model : int = failwith "TODO"

(** Update a model from a specifiction with minimal work **)
let update model spec = failwith "TODO"

(** A type to represent how gaps should be treated when creating substitution
    rate matrices. *)
type gap_repr =
  (int * float) option

(** divide a matrix by the mean rate, to normalize to 1 *)
let m_meanrate srm pi_ =
  let mr = ref 0.0 and a_size = Bigarray.Array2.dim1 srm in
  for i = 0 to (a_size-1) do
    mr := !mr +. (~-.(srm.{i,i}) *. pi_.{i});
  done;
  for i = 0 to (a_size-1) do
    for j = 0 to (a_size-1) do
      srm.{i,j} <- srm.{i,j} /. !mr;
    done;
  done

(** val jc69 :: ANY ALPHABET size *)
let m_jc69 a_size (gap_r : gap_repr) =
  let mu = 1.0 in
  let srm = create_ba2 a_size a_size in
  Bigarray.Array2.fill srm mu;
  let () = match gap_r with
    | None ->
      let diag = -. mu *. float (a_size-1) in
      for i = 0 to (a_size-1) do
        srm.{i,i} <- diag
      done;
    | Some (i,r) ->
      let diag = -. mu *. (r +. float (a_size-2)) in
      for j = 0 to (a_size-1) do
        srm.{i,j} <- mu *. r;
        srm.{j,i} <- mu *. r;
        srm.{j,j} <- diag;
      done;
      srm.{i,i} <- -. mu *. r *. float (a_size-1)
  in
  let mr = ref 0.0 and wght = 1.0 /. float a_size in
  for i = 0 to (a_size-1) do
    mr := !mr +. (~-.(srm.{i,i}) *. wght );
  done;
  for i = 0 to (a_size-1) do
    for j = 0 to (a_size-1) do
      srm.{i,j} <- srm.{i,j} /. !mr;
    done;
  done;
  srm

(** val k2p :: only 4 or 5 characters *)
let m_k2p beta a_size (gap_r : gap_repr) =
  let alpha = 1.0 in
  if not ((a_size = 4) || (a_size = 5)) then
    raise (errorf "Alphabet does not support this model");
  let srm = create_ba2 a_size a_size in
  let beta = max beta minimum in
  Bigarray.Array2.fill srm beta;
  (* modify transition elements to alpha *)
  srm.{1, 3} <- alpha; srm.{3, 1} <- alpha;
  srm.{2, 0} <- alpha; srm.{0, 2} <- alpha;
  (* set up the diagonal elements *)
  let () = match gap_r with
    | None -> 
      let diag =
        if a_size = 4 then
          -. alpha -. beta -. beta
        else begin
          assert ( a_size = 5 );
          -. alpha -. beta *. 3.0
        end
      in
      for i = 0 to (a_size-1) do
        srm.{i,i} <- diag
      done;
    | Some (i,r) ->
      assert( a_size = 5 );
      let diag = -. alpha -. beta *. 3.0 -. beta *. r in
      for j = 0 to (a_size-1) do
        srm.{i,j} <- beta *. r;
        srm.{j,i} <- beta *. r;
        srm.{j,j} <- diag;
      done;
      srm.{i,i} <- -. beta *. r *. float (a_size-1)
  in
  let mr = ref 0.0 and wght = 1.0 /. float a_size in
  for i = 0 to (a_size-1) do
    mr := !mr +. (~-.(srm.{i,i}) *. wght );
  done;
  for i = 0 to (a_size-1) do
    for j = 0 to (a_size-1) do
      srm.{i,j} <- srm.{i,j} /. !mr;
    done;
  done;
  srm

(** val tn93 :: only 4 or 5 characters *)
let m_tn93 pi_ alpha beta a_size (gap_r : gap_repr) =
  if not ((a_size = 4) || (a_size = 5)) then
    raise (errorf "Alphabet size does not support this model");
  let srm = create_ba2 a_size a_size in
  let gamma = 1.0 in
  Bigarray.Array2.fill srm gamma;
  srm.{0,2} <- alpha; srm.{1,3} <- beta; (* ACGT -- R=AG -- Y=CT *)
  srm.{2,0} <- alpha; srm.{3,1} <- beta; (* 0123 -- R=02 -- Y=13 *)
  let () = match gap_r with
    | None -> 
      for i = 0 to (a_size-1) do
        for j = 0 to (a_size-1) do
          srm.{i,j} <- srm.{i,j} *. pi_.{j};
        done;
      done;
    | Some (k,r) -> 
      for i = 0 to (a_size-1) do
        for j = 0 to (a_size-1) do
          srm.{i,j} <- srm.{i,j} *. pi_.{j};
        done;
        srm.{i,k} <- gamma *. r *. pi_.{k};
        srm.{k,i} <- gamma *. r *. pi_.{i};
     done;
  in
  (* normalize diagonal so row sums to 0 *)
  for i = 0 to (a_size-1) do
    let diag = ref 0.0 in
    for j = 0 to (a_size-1) do
      if (i <> j) then diag := !diag +. srm.{i,j};
    done;
      srm.{i,i} <- -. !diag;
  done;
  m_meanrate srm pi_;
  srm

(** val f81 :: ANY ALPHABET size *)
let m_f81 pi_ a_size (gap_r : gap_repr) =
  let srm = create_ba2 a_size a_size in
  let lambda = 1.0 in
  let () = match gap_r with
    | None -> 
      for i = 0 to (a_size-1) do
        for j = 0 to (a_size-1) do
          if i = j then ()
                   else srm.{i,j} <- pi_.{j} *. lambda
        done;
      done;
    | Some (s,r) ->
      for i = 0 to (a_size-1) do
        for j = 0 to (a_size-1) do
               if i = j then ()
          else if j = s
                   then srm.{i,j} <- pi_.{j} *. r
                   else srm.{i,j} <- pi_.{j} *. lambda
        done;
      done;
  in
  for i = 0 to (a_size-1) do
    let diag = ref 0.0 in
    for j = 0 to (a_size-1) do
      if (i <> j) then diag := !diag +. srm.{i,j};
    done;
    srm.{i,i} <- -. !diag;
  done;
  m_meanrate srm pi_;
  srm

(** val hky85 :: only 4 or 5 characters *)
let m_hky85 pi_ kappa a_size (gap_r : gap_repr) =
  m_tn93 pi_ kappa kappa a_size gap_r

(** val f84 :: only 4 or 5 characters *)
let m_f84 pi_ kappa a_size (gap_r : gap_repr) =
  let y = pi_.{1} +. pi_.{3} in (* Y = C + T *)
  let r = pi_.{0} +. pi_.{2} in (* R = A + G *)
  let alpha = 1.0 +. kappa /. r in
  let beta = 1.0 +. kappa /. y in
  m_tn93 pi_ alpha beta a_size gap_r

(* normalize gap rate against rate between non-gap characters. *)
let normalize ?(m=minimum) gap_state vec = match gap_state with
  | Independent ->
    let normalize_factor = max m vec.((Array.length vec) - 1) in
    let vec = Array.map (fun i -> (max m i) /. normalize_factor) vec in
    vec, Independent
  | Coupled n ->
    let normalize_factor = max m vec.((Array.length vec) - 1) in
    let vec = Array.map (fun i -> (max m i) /. normalize_factor) vec in
    vec, Coupled (n/.normalize_factor)
  | Missing ->
    let normalize_factor = max m vec.((Array.length vec) - 1) in
    let vec = Array.map (fun i -> (max m i) /. normalize_factor) vec in
    vec,Missing

(* val gtr :: ANY ALPHABET size
   pi_ == n; co_ == ((n-1)*n)/2
   form of lower packed storage mode, excluding diagonal, *)
let m_gtr_independent pi_ co_ a_size =
  if (((a_size+1)*(a_size-2))/2) <> Array.length co_ then
    failwithf ("Length of GTR parameters (I) is incorrect for the alphabet."
               ^^ "They should be %d, but are %d.")
              (((a_size+1)*(a_size-2))/2) (Array.length co_);
  (* last element of GTR = 1.0 *)
  let co_ =
    let size = (((a_size-1)*a_size)/2) in
    Array.init (size)
               (fun i -> if i = (size-1) then 1.0 else co_.(i))
  in
  (* create matrix *)
  let n = ref 0 in (* array index *)
  let srm = create_ba2 a_size a_size in
  for i = 0 to (a_size-1) do
    for j = (i+1) to (a_size-1) do
      srm.{i,j} <- co_.(!n) *. pi_.{j};
      srm.{j,i} <- co_.(!n) *. pi_.{i};
      incr n;
    done;
  done;
  (* set diagonal so row sums to 0 *)
  for i = 0 to (a_size-1) do begin
    let diag = ref 0.0 in
    for j = 0 to (a_size-1) do
      if (i <> j) then diag := !diag +. srm.{i,j};
    done;
    srm.{i,i} <- -. !diag;
  end; done;
  (* divide through by mean-rate *)
  m_meanrate srm pi_;
  srm

let m_gtr_coupled pi_ co_ a_size i_gap r_gap =
  if (((a_size)*(a_size-3))/2) <> Array.length co_ then
    raise (errorf "GTR parameters %d, expected %d for alphabet %d."
                  (Array.length co_) (((a_size)*(a_size-3))/2) a_size);
  (* last element of GTR = 1.0; add back *)
  let co_ =
    let size = Array.length co_ in
    Array.init (size+1) (fun i -> if i = (size) then 1.0 else co_.(i))
  in
  (* create matrix *)
  let n = ref 0 in (* array index *)
  let srm = create_ba2 a_size a_size in
  for i = 0 to (a_size-1) do
    (* set the gap and gap coefficient col/row *)
    if i = i_gap then begin
      for j = 0 to (a_size-1) do
        srm.{j,i} <- r_gap *. pi_.{i};
        srm.{i,j} <- r_gap *. pi_.{j};
      done;
    end else begin
      for j = i+1 to (a_size-1) do
        if j = i_gap then ()
        else begin
          srm.{i,j} <- co_.(!n) *. pi_.{j};
          srm.{j,i} <- co_.(!n) *. pi_.{i};
          incr n;
        end;
      done;
    end;
  done;
  (* set diagonal so row sums to 0 *)
  for i = 0 to (a_size-1) do begin
    let diag = ref 0.0 in
    for j = 0 to (a_size-1) do
      if (i <> j) then diag := !diag +. srm.{i,j};
    done;
    srm.{i,i} <- -. !diag;
  end; done;
  (* divide through by mean-rate *)
  m_meanrate srm pi_;
  srm

let m_gtr pi_ co_ a_size (gap_r : gap_repr) =
  let srm = match gap_r with
    | Some (i,r) -> m_gtr_coupled pi_ co_ a_size i r
    | None       -> m_gtr_independent pi_ co_ a_size
  in
  srm

(* val m_file :: any alphabet size -- recomputes diagonal and divides by meanrate *)
let m_file pi_ f_rr a_size =
  assert(a_size = Array.length f_rr);
  let srm = create_ba2 a_size a_size in
  for r = 0 to (a_size-1) do
    assert(a_size = Array.length f_rr.(r));
    let diag = ref 0.0 in
    for c = 0 to (a_size-1) do
      if (c <> r) then begin
        diag := !diag +. f_rr.(r).(c);
        srm.{r,c} <- f_rr.(r).(c);
      end
    done;
    srm.{r,r} <- ~-. !diag;
  done;
  m_meanrate srm pi_;
  srm

let m_custom pi_ idxs ray a_size =
  let idx = ref 0 in
  let srm = create_ba2 a_size a_size in
  for i = 0 to (a_size-1) do
    for j=i+1 to (a_size-1) do
      let value = ray.(IntMap.find !idx idxs) in
      srm.{i,j} <- value *. pi_.{j};
      srm.{j,i} <- value *. pi_.{i};
      incr idx;
    done;
  done;
  for i = 0 to (a_size-1) do begin
    let diag = ref 0.0 in
    for j = 0 to (a_size-1) do
      if (i <> j) then diag := !diag +. srm.{i,j};
    done;
    srm.{i,i} <- -. !diag;
  end; done;
  m_meanrate srm pi_;
  srm


(* create a custom model by two Maps. One that maps an index to it's linked index
   and another that maps that to a value. The default value if the element does
   not exist is set to 1.0. The mean rate is normalized and the priors are
   multiplied through. The indexes (see below) ensure that we are dealing with a
   symmetric matrix for the parameters,

   ex,  - a a b  The index association would be, 1->1, 2->1, 3->2, 4->2,
        a - b a                                  5->1, 6->1 (upper triangular).
        a b - a  Diagonal elements are always ignored, a dash is recommended.
        b a a -  We ensure that the parameters are coupled symmetrically.  *)
let process_custom_matrix alph_size (f_aa: char array array) =
  let found = ref IntSet.empty in
  let assoc = ref IntMap.empty in
  let idx = ref 0 in
  let a_size = Array.length f_aa in
  if not (a_size = alph_size) then
    raise (errorf "Alphabet size does not match model");
  for i = 0 to a_size -1 do
    assert( Array.length f_aa.(i) = a_size );
    for j = i+1 to a_size-1 do
      let letter = Char.code f_aa.(i).(j) in
      if not ( letter = Char.code f_aa.(j).(i) ) then
        raise (errorf "Custom model should be symmetric");
      assoc := IntMap.add !idx letter !assoc;
      found := IntSet.add letter !found;
      incr idx;
    done;
  done;
  let length,map =
    IntSet.fold
      (fun v1 (i,map) ->
        let map =
          IntMap.map (fun v2 -> if v1 = v2 then i else v2) map
        in
        (i+1, map))
      (!found)
      (0,!assoc)
  in
  (map, Array.create length 1.0)

(** [diagonalize sym Q] diagonalize matrix [Q]; [sym] decides if the function
    should use symmetric or general routines for diagonalization. *)
let diagonalize (sym : bool) mat =
  (* A function to check for nan values; catch before a diagonalization, since
   * the lapack routines return illegal value instead of a backtrace *)
  let check_for_nan mat =
    try
      for i = 0 to (Bigarray.Array2.dim1 mat)-1 do
        for j = 0 to (Bigarray.Array2.dim1 mat)-1 do
            assert( not (is_nan mat.{i,j}));
        done;
      done;
      true
    with _ ->
      false
  in
  assert( check_for_nan mat );
  let alph = Bigarray.Array2.dim1 mat in
  let n_u  = create_ba2 alph alph in
  let () = Bigarray.Array2.blit mat n_u in
  let n_d  = create_ba2 alph alph in
  let () = Bigarray.Array2.fill n_d 0.0 in
  assert( alph = Bigarray.Array2.dim2 mat);
  match sym with
  | true  ->
    let () = diagonalize_sym n_u n_d in
    n_u, n_d, None
  | false ->
    let n_ui = create_ba2 alph alph in
    let () = Bigarray.Array2.fill n_ui 0.0 in
    let () = diagonalize_gtr n_u n_d n_ui in
    n_u, n_d, Some n_ui

let compose model t = match model.ui with
  | Some ui -> compose_gtr model.u model.d ui t
  | None    -> compose_sym model.u model.d t

let substitution_matrix model topt =
  let _gapr = match model.spec.gap with
    | Coupled x   ->
      begin match model.spec.alphabet.Alphabet.gap with
        | Some g  -> Some (g, x)
        | None    -> failwith "cannot couple with gap, w/out gap" (* todo *)
      end
    | Independent -> None
    | Missing     -> None
  and a_size = alphabet_size model
  and priors = model.priors in
  let m = match model.spec.substitution with
    | JC69         -> m_jc69 a_size _gapr
    | F81          -> m_f81 priors a_size _gapr
    | K2P t        -> m_k2p t a_size _gapr
    | F84 t        -> m_f84 priors t a_size _gapr
    | HKY85 t      -> m_hky85 priors t a_size _gapr
    | TN93 (ts,tv) -> m_tn93 priors ts tv a_size _gapr
    | GTR c        -> m_gtr priors c a_size _gapr
    | Const m      -> m_file priors m a_size
    | Custom (assoc,ray) -> m_custom priors assoc ray a_size
  in
  match topt with
  | Some t ->
    for i = 0 to (Bigarray.Array2.dim1 m)-1 do
      for j = 0 to (Bigarray.Array2.dim2 m)-1 do
        m.{i,j} <- m.{i,j} *. t;
      done;
    done;
    m
  | None ->
    m

(** [compose_matrix Q t] compose the construction of a probability rate matrix
    directly from a substitution rate matrix and time period/branch length.
let compose_matrix sub_mat t = 
  let a_size = Bigarray.Array2.dim1 sub_mat in
  let (u_,d_,ui_) = 
    let n_d = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout a_size a_size
    and n_ui = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout a_size a_size in
    Bigarray.Array2.fill n_d 0.0;
    Bigarray.Array2.fill n_ui 0.0;
    diagonalize_gtr sub_mat n_d n_ui;
    (sub_mat, n_d, n_ui)
  in
  compose_gtr u_ d_ ui_ t *)


(** [integerized_model] convert a model and branch length to a probability rate
    matrix and then scale by a factor of accuracy and convert to integers. *)
let integerized_model ?(sigma=4) model t =
  let sigma = 10.0 ** (float_of_int sigma)
  and size = alphabet_size model in
  let create = match model.spec.substitution with
    | JC69 | K2P _ ->
      (fun s m i j -> ~-(int_of_float (s *.(log m.(i).(j)))))
    | (F81 | F84 _ | HKY85 _ | TN93 _| GTR _ | Const _ | Custom _) ->
      (fun s m i j -> ~-(int_of_float (s *.(log (model.priors.{i} *. m.(i).(j))))))
  and matrix =
    ba_to_array2
      (match model.ui with
       | Some ui -> compose_gtr model.u model.d ui t
       | None    -> compose_sym model.u model.d t)
  and imatrix = Array.make_matrix size size 0 in
  assert( size = Array.length matrix);
  assert( size = Array.length matrix.(0));
  for i = 0 to size-1 do
    for j = 0 to size-1 do
      imatrix.(i).(j) <- create sigma matrix i j
    done;
  done;
  imatrix


(** create a model based on a specification and an alphabet *)
let create spec = 
  let a_size = 
    let g = match spec.gap with
      | Missing -> 0
      | Coupled _  | Independent -> 1
    in
    g + (Alphabet.size spec.alphabet)
  in
  (* set up all the probability and rates *)
  let rates, probs, pinvar =
    match spec.site_variation with
    | Constant -> ba_of_array1 [| 1.0 |], ba_of_array1 [| 1.0 |], None
    | DiscreteGamma (x,y) -> (* SITES,ALPHA *)
      let p = create_ba1 x in
      Bigarray.Array1.fill p (1.0 /. (float_of_int x));
      gamma_rates y y x,p,None
    | DiscreteCustom ray ->
      let rates = ba_of_array1 @@ Array.map fst ray
      and probs = ba_of_array1 @@ Array.map snd ray in
      rates,probs,None
    | DiscreteTheta (w,x,z) -> (* GAMMA_SITES,ALPHA,PERCENT_INVAR *)
      if w < 1 then
        failwith "Number of rate categories must be >= 1 (if invar w/out gamma)"
      else if w = 1 then begin
        ba_of_array1 [| 1.0 |], ba_of_array1 [| 1.0 |], Some z
      end else begin
        let p = create_ba1 w in
        Bigarray.Array1.fill p (1.0 /. (float_of_int w));
        let r = gamma_rates x x w in
        r,p,Some z
      end
  in
  (* extract the prior probability *)
  let priors =
    let p = match spec.base_priors with 
      | Equal -> Array.make a_size (1.0 /. (float_of_int a_size))
      | Empirical p ->
        let p = Array.map (fun i -> max minimum i) p in
        let sum = Array.fold_left (fun a b -> a +. b) 0.0 p in 
        if sum =. 1.0 
          then p
          else Array.map (fun i -> i /. sum) p
    in
    if not (a_size = Array.length p) then
      raise (errorf "Priors (length %d) don't match alphabet (length %d)"
                    (Array.length p) a_size)
    else
      ba_of_array1 p
  in
  let _gapr = match spec.gap with
    | Coupled x   ->
      begin match spec.alphabet.Alphabet.gap with
        | None    -> raise (errorf "No gap character found")
        | Some g  -> Some (g, x)
      end
    | Independent -> None
    | Missing     -> None
  in
  (*  get the substitution rate matrix and set sym variable and to_formatter vars *)
  let sym, q, substitution = match spec.substitution with
    | JC69  -> true,  m_jc69 a_size _gapr, JC69
    | F81   -> false, m_f81 priors a_size _gapr, F81
    | K2P t -> true,  m_k2p t a_size _gapr, spec.substitution
    | F84 t -> false, m_f84 priors t a_size _gapr, spec.substitution
    | HKY85 t -> false, m_hky85 priors t a_size _gapr, spec.substitution
    | TN93 (ts,tv) -> false, m_tn93 priors ts tv a_size _gapr, spec.substitution
    | GTR c ->
      let c = match spec.gap, (Array.length c) with
        | Coupled _, 0 -> Array.make ((a_size*(a_size-3))/2) 1.0
        | (Independent | Missing), 0 -> Array.make (((a_size-2)*(a_size+1))/2) 1.0
        | (Coupled _ | Independent | Missing), _ -> c
      in
      false, m_gtr priors c a_size _gapr, (GTR c)
    | Const m ->
      false, m_file priors m a_size, spec.substitution
    | Custom (assoc,xs) ->
      false, m_custom priors assoc xs a_size, spec.substitution
  in
  let (u,d,ui) = diagonalize sym q in
  let spec = {spec with substitution;} in
  {
    spec; rates; probs; pinvar; priors; q; u; d; ui;
  }

(** Enumerate models based on some set of criteria. for Multimodel Inference  *)
let enum_models ?(site_var=[`DiscreteGamma 4;`DiscreteTheta 4;`Constant])
                ?(subst_model=[`JC69;`F81;`K2P;`F84;`HKY85;`TN93;`GTR])
                ?(priors=[`Empirical;`Equal]) ?(gap=`Missing)
            empirical_priors alphabet : (unit -> s option) =
  let apply_model_delta gap substitution base_priors site_variation =
    let base_priors = match base_priors,empirical_priors with
      | `Empirical, Some x -> Empirical x
      | `Empirical, None   -> assert false (** todo: add better error message *)
      | `Equal, _          -> Equal
    in
    let substitution,base_priors = match substitution with
      | `JC69  -> JC69,Equal
      | `F81   -> F81,base_priors
      | `K2P   -> K2P default_tstv,Equal
      | `F84   -> F84 default_tstv,base_priors
      | `HKY85 -> HKY85 default_tstv,base_priors
      | `TN93  -> TN93 (default_tstv,default_tstv),base_priors
      | `GTR   -> GTR [||],base_priors
      | `Custom x -> Custom x,base_priors
    and site_variation = match site_variation with
      | `DiscreteGamma s      -> DiscreteGamma (s,default_alpha)
      | `DiscreteTheta s      -> DiscreteTheta (s,default_alpha,default_invar)
      | `Constant             -> Constant
      | `DiscreteCustom data  -> DiscreteCustom data
    in
    {substitution; base_priors; site_variation; gap; alphabet;}
  in
  let next_specs =
    let gaps = match gap with
      | `Missing     -> [Missing]
      | `Independent -> [Independent]
      | `Coupled     -> [Coupled default_rate]
      | `Indel       -> [Independent; Coupled default_rate]
    and priors = match empirical_priors with
      | None   -> [`Equal]
      | Some _ -> priors
    in
    let cp =
      List.fold_left (fun acc w ->
        List.fold_left (fun acc x ->
          List.fold_left (fun acc y ->
            List.fold_left (fun acc z -> (w,x,y,z)::acc) acc priors)
            acc site_var)
          acc subst_model)
        [] gaps
    in
    ref cp
  in
  (fun () -> match !next_specs with
    | (gap,subst,vari,prior)::ms ->
        next_specs := ms;
        Some (apply_model_delta gap subst prior vari)
    | [] -> None)

(** produce a short readable name for the model: like JC69+G+I. We ignore data
    dependent and optimality paramters (gap as missing/independent) and mpl/mal *)
let short_name model =
  let model_name = match model.spec.substitution with
    | JC69    -> "JC69" | F81     -> "F81"
    | K2P _   -> "K81"  | F84 _   -> "F84"
    | HKY85 _ -> "HKY"  | TN93 _  -> "TN93"
    | GTR _   -> "GTR"  | Const _ -> "GIVEN"
    | Custom _-> "CUSTOM"
  and variation_name = match model.spec.site_variation with
    | DiscreteTheta (i,_,_) when i > 1 -> "+G+I"
    | DiscreteGamma _ -> "+G"
    | DiscreteTheta _ -> "+I"
    | DiscreteCustom _ -> "+C"
    | Constant -> ""
  in
  model_name ^ variation_name

(** generate optimization functions and get current values for the model *)
let gen_subst_opt_func m : vector * (vector -> t) =
  failwith "todo"

let gen_rates_opt_func m : vector * (vector -> t) =
  failwith "todo"

let gen_prior_opt_func m : vector * (vector -> t) =
  failwith "todo"

