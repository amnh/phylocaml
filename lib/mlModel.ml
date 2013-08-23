open Internal

let debug = false

exception ModelError of string

let errorf format = Printf.ksprintf (fun x -> ModelError x) format

(* Minimum value for things when 0 is numerically unstable. *)
let minimum = 1e-13

type site_var =
  | DiscreteGamma of int * float
  | DiscreteTheta of int * float * float
  | DiscreteCustom of (float * float) array
  | Constant

let default_invar = 0.1

and default_alpha = 0.2

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

let default_tstv  = 2.0

type priors = 
  | Empirical of float array
  | Equal

type gap =
  | Missing
  | Coupled of float
  | Independent

let default_gap_ratio = 0.05

type spec = {
  substitution : subst_model;
  site_variation : site_var;
  base_priors : priors;
  alphabet : Alphabet.t * gap;
}

type model = {
  spec  : spec;
  priors: (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  pinvar: float option;
  rates : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  probs : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
  q     : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  u     : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  d     : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t;
  ui    : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t option; 
  opt   : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(** diagonalize a symmetric or gtr matrix, WARNING: modifies passed matrices *)
external diagonalize_gtr: (* U D Ui *)
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
    unit = "likelihood_CAML_diagonalize_gtr"

external diagonalize_sym: (* U D *)
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
    unit = "likelihood_CAML_diagonalize_sym"

(** compose matrices -- for testing purposes, as this composition is
    usually done on the C side exclusively. If the time is less then zero, the
    instantaneious rate matrix will be returned instead (which is just UDUi). *)
external compose_gtr: (* U D Ui t -> P^e^t *)
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t -> 
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t -> float ->
    (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t =
      "likelihood_CAML_compose_gtr"

external compose_sym: (* U D t -> P^e^t *)
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t ->
  (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t -> float ->
    (float,Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array2.t =
      "likelihood_CAML_compose_sym"

(** Generate the GAMMA RATE classes *)
let gamma_rates a b len =
  let dist = Pareto.Distributions.Gamma.create ~shape:a ~scale:b in
  Array.init len
    (fun i ->
      let p = (float_of_int i) /. (float_of_int len) in
      Pareto.Distributions.Gamma.quantile dist ~p)
    |> ba_of_array1

(** Return the alphabet for the characters *)
let get_alphabet s = fst s.alphabet

(** Return the size of the alphabet for analysis;  *)
let alphabet_size s =
  let g = match snd s.alphabet with
    | Missing -> 0
    | Coupled _  | Independent -> 1
  in
  g + (Alphabet.size $ fst s.alphabet)

(** Compare two models; not to be used for a total ordering (ret neg or zero) *)
let compare a b =
  let a_asize = alphabet_size a.spec
  and b_asize = alphabet_size b.spec in
  let compare_array x y =
    let results = ref true in
      for i = 0 to a_asize - 1 do
        results := !results && (x.(i) =. y.(i));
      done;
      !results
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
    | Custom _, Custom _ -> failwith "TODO"
    | TN93 (x1,x2),  TN93 (y1,y2) when x1 = y1 && x2 = y2 -> true
    | GTR xs, GTR ys when compare_array xs ys -> true
    | Const x, Const y when x = y -> true
    | (JC69|F81|K2P _|F84 _|HKY85 _|Custom _|TN93 _|GTR _|Const _),_ -> false
  and v_compare = match a.spec.site_variation,b.spec.site_variation with
    | DiscreteGamma (ix,ax), DiscreteGamma (iy,ay) -> ix=iy && ax=ay
    | DiscreteTheta (ix,ax,bx), DiscreteTheta (iy,ay,by) -> ix=iy && ax=ay && bx=by
    | Constant, Constant -> true
    | DiscreteCustom a, DiscreteCustom b ->
      fold_left2 (fun acc (a,b) (c,d) -> (a =. c) && (b =. d) && acc) true a b
    | (DiscreteGamma _|DiscreteCustom _ |DiscreteTheta _|Constant), _ -> false
  and g_compare = match snd a.spec.alphabet,snd b.spec.alphabet with
    | Missing, Missing
    | Independent, Independent -> true
    | Coupled x, Coupled y when x=y-> true
    | (Missing | Independent | Coupled _), _ -> false
  and p_compare = compare_priors a b in
  m_compare && v_compare && g_compare && p_compare
 

module OrderedML = struct
  type t = spec 
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


let generate_opt_vector spec = failwith "TODO"


(** Count the number of parameters in the model; used for xIC functions **)
let num_parameters model : int =
  Bigarray.Array1.dim model.opt

(** A type to represent how gaps should be treated when creating substitution
    rate matrices. *)
type gap_repr =
  (int * float) option

(** divide a matrix by the mean rate so it will equal 1 *)
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
let m_jc69 a_size gap_r =
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
  (* normalize by mean-rate TODO: remove, and replace above with exact. *)
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
let m_k2p beta a_size gap_r =
  let alpha = 1.0 in
  if not ((a_size = 4) || (a_size = 5)) then
    raise (raise (errorf "Alphabet does not support this model"));
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
  (* normalize by mean-rate; TODO: this should be calculated directly *)
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
let m_tn93 pi_ alpha beta a_size gap_r =
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
let m_f81 pi_ a_size gap_r =
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
let m_hky85 pi_ kappa a_size gap_r =
  m_tn93 pi_ kappa kappa a_size gap_r

(** val f84 :: only 4 or 5 characters *)
let m_f84 pi_ kappa a_size gap_r =
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

let m_gtr pi_ co_ a_size gap_r =
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
let process_custom_model alph_size (f_aa: char array array) =
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


(* functions to diagonalize the two types of substitution matrices *)
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
  let _gapr = match snd model.spec.alphabet with
    | Coupled x   -> Some (Alphabet.get_gap (fst model.spec.alphabet), x)
    | Independent -> None
    | Missing     -> None
  and a_size = alphabet_size model.spec
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


(* print output in our nexus format or Phyml output *)
let output_model output output_table nexus model set = 
  let printf format = Printf.ksprintf output format in
  let gtr_mod = ref false in
  if nexus = `Nexus then begin
    printf "@[Likelihood@.";
    let () = match model.spec.substitution with
      | JC69   -> printf "@[Model = JC69;@]@\n";
      | F81    -> printf "@[Model = F81;@]";
      | K2P x  -> printf "@[Model = K2P;@]";
                  printf "@[Parameters = %f;@]" x
      | F84 x  -> printf "@[Model = F84;@]";
                  printf "@[Parameters = %f;@]" x
      | HKY85 x-> printf "@[Model = HKY85;@]";
                  printf "@[Parameters = %f;@]" x
      | TN93 (a,b) -> printf "@[Model = TN93;@]";
                  printf "@[Parameters = %f %f;@]" a b
      | GTR xs -> printf "@[Model = GTR;@]";
                  printf "@[Parameters = ";
                  Array.iter (printf "%f ") xs;
                  printf ";@]";
                  gtr_mod := true
      | Const m ->
        printf "@[Model = @[";
        Array.iter
          (fun x ->
            printf "@[";
            Array.iter (fun y -> printf "%f; " y) x;
            printf "@]@\n")
          m
      | Custom (_,xs) ->
        printf "@[Parameters = ";
        Array.iter (printf "%f ") xs;
        printf ";@]";
    in
    let () = match model.spec.base_priors with
      | Equal -> printf "@[Priors = Equal;@]"
      | Empirical x ->
        let first = ref true in
        printf "@[Priors =";
        List.iter
            (fun (s,i,_) -> 
                try if !first then begin
                    printf "@[%s %.5f" s x.(i); first := false
                end else
                    printf ",@]@[%s %.5f" s x.(i) 
                with _ -> ())
            (Alphabet.to_list $ fst model.spec.alphabet);
        printf ";@]@]"
      in
      let () = match model.spec.site_variation with
        | Constant -> ()
        | DiscreteCustom ray ->
          printf "@[Variation = custom;@]";
          Array.iter (fun (a,b) -> printf "@[@[%.5f@] @[%f; @]@]" a b) ray
        | DiscreteGamma (c, p) ->
          printf "@[Variation = gamma;@]@[alpha = %.5f@]@[sites = %d;@]" p c
        | DiscreteTheta (c, p, i) ->
          printf ("@[Variation = theta;@]@[alpha = %.5f@]@[sites = %d;@]"
                 ^^"@[percent = %.5f;@]") p c i
      in
      let () = match snd model.spec.alphabet with
        (* the meaning of independent/coupled switch under gtr *)
        | Independent -> printf "@[gap = independent;@]"
        | Coupled x   -> printf "@[gap = coupled:%f;@]" x
        | Missing     -> printf "@[gap = missing;@]"
      in
      let () = match set with
        | Some namelist ->
          let first = ref true in
          printf "@[CharSet = ";
          List.iter
            (fun s -> 
              if !first then begin printf "@[%s" s; first := false end
                        else printf ",@]@[%s" s)
            namelist;
            printf ";@]@]";
        | None -> ()
      in
      printf ";@]@."
    (* ---------------------------- *)
    end else (* phylip *) begin
      assert( nexus = `Phylip );
      printf "@[<hov 0>Discrete gamma model: ";
      let () = match model.spec.site_variation with
        | Constant -> printf "No@]\n";
        | DiscreteGamma (cats,param) ->
          printf ("Yes@]@\n@[<hov 1>- Number of categories: %d@]\n"^^
                  "@[<hov 1>- DiscreteGamma Shape Parameter: %.4f@]\n") cats param
        | DiscreteTheta (cats,param,inv) ->
          printf ("Yes@]@\n@[<hov 1>- Number of categories: %d@]\n"^^
                  "@[<hov 1>- DiscreteGamma Shape Parameter: %.4f@]\n") cats param;
          printf ("@[<hov 1>- Proportion of invariant: %.4f@]\n") inv
        | DiscreteCustom ray ->
          let cats = Array.length ray in
          printf ("Yes@]@\n@[<hov 1>- Number of categories: %d@]\n") cats;
          Array.iter (fun (a,b) -> printf "@[<hov 1>@[%.5f@] @[%f; @]@]" a b) ray
      in
      printf "@[<hov 0>Priors / Base frequencies:@\n";
      let () = match model.spec.base_priors with
        | Equal -> printf "@[Equal@]@]@\n"
        | Empirical x ->
          List.iter
            (fun (s,i,_) ->
              (* this expection handling avoids gaps when they are not
               * enabled in the alphabet as an additional character *)
              try printf "@[<hov 1>- f(%s)= %.5f@]@\n" s x.(i) with _ -> ())
            (Alphabet.to_list $ fst model.spec.alphabet);
      in
      printf "@[<hov 0>Model Parameters: ";
      let a = alphabet_size model.spec in
      let () = match model.spec.substitution with
        | JC69  -> printf "JC69@]@\n"
        | F81   -> printf "F81@]@\n"
        | K2P x ->
          printf "K2P@]@\n@[<hov 1>- Transition/transversion ratio: %.5f@]@\n" x
        | F84 x ->
          printf "F84@]@\n@[<hov 1>- Transition/transversion ratio: %.5f@]@\n" x
        | HKY85 x    ->
          printf "HKY85@]@\n@[<hov 1>- Transition/transversion ratio:%.5f@]@\n" x
        | TN93 (a,b) -> 
          printf "tn93@]@\n@[<hov 1>- transition/transversion ratio:%.5f/%.5f@]@\n" a b
        | GTR ray -> gtr_mod := true;
          printf "GTR@]@\n@[<hov 1>- Rate Parameters: @]@\n";
          let get_str i = Alphabet.get_name i $ fst model.spec.alphabet
          and convert s r c = (c + (r*(s-1)) - ((r*(r+1))/2)) - 1 in
          begin match snd model.spec.alphabet with
            | Coupled x ->
              let ray =
                let size = (((a-3)*a)/2) in
                Array.init (size+1) (fun i -> if i = size then 1.0 else ray.(i))
              in
              for i = 0 to a - 2 do
                for j = i+1 to a - 2 do
                  printf "@[<hov 1>%s <-> %s - %.5f@]@\n"
                         (get_str i) (get_str j) ray.(convert (a-1) i j)
                done;
              done;
              printf "@[<hov 1>%s <-> N - %.5f@]@\n"
                (get_str (Alphabet.get_gap $ fst model.spec.alphabet)) x
            | Missing | Independent ->
              let ray =
                let size = (((a-1)*a)/2) in
                Array.init (size) (fun i -> if i = (size-1) then 1.0 else ray.(i))
              in
              for i = 0 to a - 1 do
                for j = i+1 to a - 1 do
                  printf "@[<hov 1>%s <-> %s - %.5f@]@\n" (get_str i)
                         (get_str j) ray.(convert a i j)
                done;
              done
          end
        | Const _ ->
          let mat = compose model 0.0 in
          printf "@[<hov 1>[";
          for i = 0 to a - 1 do
            printf "%s ------- " (Alphabet.get_name i $ fst model.spec.alphabet)
          done;
          printf "]@]@\n";
          for i = 0 to a - 1 do
            for j = 0 to a - 1 do
              printf "%.5f\t" mat.{i,j}
            done;
            printf "@\n";
          done;
        | Custom (_,xs) ->
          printf "@[Parameters : ";
          Array.iter (printf "%f ") xs;
          printf ";@]";
      in
      let () = match snd model.spec.alphabet with
        | Independent -> printf "@[<hov 0>Gap property: independent;@]@\n"
        | Coupled x   -> printf "@[<hov 0>Gap property: coupled, Ratio: %f;@]@\n" x
        | Missing     -> printf "@[<hov 0>Gap property: missing;@]@\n"
      in
      printf "@]";
      printf "@[@[<hov 0>Instantaneous rate matrix:@]@\n";
      match output_table with
      | Some output_table ->
        let table = Array.make_matrix (a+1) a "" in
        let () =
          let mat = compose model ~-.1.0 in
          for i = 0 to a - 1 do
            table.(0).(i) <- Alphabet.get_name i $ fst model.spec.alphabet;
          done;
          for i = 0 to a - 1 do 
            for j = 0 to a - 1 do
              table.(i+1).(j) <- string_of_float mat.{i,j}
            done;
          done;
        in
        let () = output_table table in
        printf "@\n@]"
      | None ->
        let () =
          let mat = compose model ~-.1.0 in
          printf "@[<hov 1>[";
          for i = 0 to a - 1 do
            printf "%s ------- " (Alphabet.get_name i $ fst model.spec.alphabet)
          done;
          printf "]";
          for i = 0 to a - 1 do
            printf "@]@\n@[<hov 1>";
            for j = 0 to a - 1 do
              printf "%8.5f  " mat.{i,j}
            done;
          done;
        in
        printf "@]@\n@]"
    end


(** [compose_model] compose a substitution probability matrix from branch length
    and substitution rate matrix. *)
let compose_model sub_mat t = 
  let a_size = Bigarray.Array2.dim1 sub_mat in
  let (u_,d_,ui_) = 
    let n_d = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout a_size a_size
    and n_ui = Bigarray.Array2.create Bigarray.float64 Bigarray.c_layout a_size a_size in
    Bigarray.Array2.fill n_d 0.0;
    Bigarray.Array2.fill n_ui 0.0;
    diagonalize_gtr sub_mat n_d n_ui;
    (sub_mat, n_d, n_ui)
  in
  compose_gtr u_ d_ ui_ t


(** [integerized_model] convert a model and branch length to a probability rate
    matrix and then scale by a factor of accuracy and convert to integers. *)
let integerized_model ?(sigma=4) model t =
  let sigma = 10.0 ** (float_of_int sigma)
  and size = alphabet_size model.spec in
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


(** create a cost matrix from a model
let model_to_cm model t =
  let input = let t = max minimum t in integerized_model model t in
  let llst = Array.to_list (Array.map Array.to_list input) in
  let res = Cost_matrix.Two_D.of_list ~suppress:true llst (snd model.spec.alphabet) in
  res *)


(* convert a string spec (from nexus, for example) to a specification
let convert_string_spec alph (name,(var,site,alpha,invar),param,priors,gap,file) =
  let gap_info = match String.uppercase (fst gap),snd gap with
    | "COUPLED", None   -> Coupled default_gap_ratio
    | "COUPLED", Some x -> Coupled x
    | "INDEPENDENT",_   -> Independent
    | "MISSING",_       -> Missing
    | "",_              -> Missing
    | x,_ -> raise (errorf "Invalid gap property, %s" x)
  in
  let submatrix = match String.uppercase name with
    | "JC69" -> begin match param with
      | [] -> JC69
      | _ -> failwith "Parameters don't match model" end
    | "F81" -> begin match param with
      | [] -> F81
      | _ -> failwith "Parameters don't match model" end
    | "K80" | "K2P" -> begin match param with
      | ratio::[] -> K2P ratio
      | []        -> K2P default_tstv
      | _ -> failwith "Parameters don't match model" end
    | "F84" -> begin match param with
      | ratio::[] -> F84 ratio
      | []        -> F84 default_tstv
      | _ -> failwith "Parameters don't match model" end
    | "HKY" | "HKY85" -> begin match param with
      | ratio::[] -> HKY85 ratio
      | []        -> HKY85 default_tstv
      | _ -> failwith "Parameters don't match model" end
    | "TN93" -> begin match param with
      | ts::tv::[] -> TN93 (ts,tv)
      | []         -> TN93 (default_tstv,default_tstv)
      | _ -> failwith "Parameters don't match model" end
    | "GTR" -> begin match param with
      | [] -> GTR [||]
      | ls -> GTR (Array.of_list ls) end
    | "GIVEN"->
      begin match file with
        | Some name ->
          name |> FileStream.read_float_matrix
               |> (fun x -> Const (x,name))
        | None -> raise (errorf "No File specified for model")
      end
    | "CUSTOM" ->
      begin match file with
        | Some name ->
          let matrix = FileStream.read_string_matrix name in
          let a_size = Array.length matrix in
          let matrix = Array.of_list (List.map (Array.of_list) matrix) in
          let assoc,ray = process_custom_model a_size matrix in
          Custom (assoc,ray,name)
        | None -> raise (errorf "No File specified for model")
      end
    (* ERROR *)
    | "" -> raise (errorf "No model specified")
    | xx -> raise (errorf "Unknown likelihood model %s" xx)
  in
  let variation = match String.uppercase var with
    | "GAMMA" ->
      let alpha = if alpha = "" then default_alpha else  float_of_string alpha in
      DiscreteGamma (int_of_string site, alpha)
    | "THETA" -> 
      let alpha = if alpha = "" then default_alpha else  float_of_string alpha in
      let invar = if invar = "" then default_invar else  float_of_string invar in
      DiscreteTheta (int_of_string site, alpha, invar)
    | "NONE" | "CONSTANT" | "" ->
      Constant
    | x -> raise (errorf "Unknown rate variation mode %s" x)
  and priors = match priors with
    | `Equal               -> Equal
    | `Estimate (Some x)   -> Empirical x
    | `Consistent pre_calc ->
      begin match submatrix, pre_calc with
        | JC69, _
        | K2P _, _   -> Equal
        | (F81|F84 _|HKY85 _|TN93 _|GTR _|Const _|Custom _), Some pi -> Empirical pi
        | (F81|F84 _|HKY85 _|TN93 _|GTR _|Const _|Custom _), None -> assert false
      end
    | `Estimate None -> assert false
  in
  { substitution = submatrix;
    site_variation = variation;
    base_priors = priors;
    alphabet = alph,gap_info; }


(** Convert Methods.ml specification to that of an MlModel spec *)
let convert_methods_spec (alph,alph_size) (compute_priors) 
                         (talph,subst,site_variation,base_priors,use_gap) =
  let u_gap = match use_gap with 
    | Independent | Coupled _ -> true | Missing -> false in
  let alph_size = 
    let w_gap = if u_gap then alph_size else alph_size - 1 in
    match talph with | `Min | `Max  -> w_gap | `Int x -> x
  in
  let base_priors = match base_priors with
    | `Estimate  -> Empirical (compute_priors ())
    | `Equal     -> Equal
    | `Given arr -> Empirical (Array.of_list arr)
    | `Consistent ->
      begin match subst with
        | `JC69 | `K2P _ -> Equal
        | _ -> Empirical (compute_priors ())
      end
  and site_variation = match site_variation with
    | None -> Constant 
    | Some (`DiscreteGamma (w,y)) -> 
      let y = match y with
        | Some x -> x 
        | None   -> default_alpha
      in
      DiscreteGamma (w,y)
    | Some (`DiscreteTheta (w,y)) ->
      let y,p = match y with 
        | Some x -> x 
        | None   -> default_alpha, default_invar
      in
      DiscreteTheta (w,y,p)
  and substitution = match subst with
    | `JC69    -> JC69
    | `F81     -> F81
    | `K2P [x] -> K2P x
    | `K2P []  -> K2P default_tstv
    | `K2P _   -> raise (ModelError "K2P requires 1 or 0 parameters")
    | `HKY85 [x] -> HKY85 x
    | `HKY85 []  -> HKY85 default_tstv
    | `HKY85 _   -> raise (ModelError "HKY85 requires 1 or 0 parameters")
    | `F84 [x]   -> F84 x
    | `F84 []    -> F84 default_tstv
    | `F84 _     -> raise (ModelError "F84 requires 1 or 0 parameters")
    | `TN93 [x;y]-> TN93 (x,y)
    | `TN93 []   -> TN93 (default_tstv,default_tstv)
    | `TN93 _    -> raise (ModelError "TN93 requires 2 or 0 parameters")
    | `GTR xs -> GTR (Array.of_list xs)
    | `File str ->
      (* this needs to be changed to allow remote files as well *)
      let matrix = FileStream.read_float_matrix str in
      Array.iter
        (fun x ->
          if not (Array.length x = alph_size) then
            raise (errorf "TN93 requires 2 or 0 parameters"))
        matrix;
      if Array.length matrix = alph_size
        then Const matrix
        else raise (errorf "Matrix in %s requires %d columns/rows" str alph_size)
    | `Custom str ->
      let matrix = FileStream.read_char_matrix str in
      let assoc,ray = process_custom_model alph_size matrix in
      Custom (assoc,ray,str)
  in
  { substitution = substitution;
  site_variation = site_variation;
     base_priors = base_priors;
        alphabet = (alph,use_gap); } *)

(** check the rates so SUM(r_k*p_k) == 1 and SUM(p_k) == 1 |p| == |r| *)
let verify_rates probs rates =
  let p1 = (Bigarray.Array1.dim probs) = (Bigarray.Array1.dim rates)
  and p2 =
    let rsps = ref 0.0 and ps = ref 0.0 in
    for i = 0 to (Bigarray.Array1.dim probs) - 1 do
      rsps := !rsps +. (probs.{i} *.  rates.{i});
      ps := !ps +. probs.{i};
    done;
    !rsps =. 1.0 && !ps =. 1.0
  in
  p1 && p2


(** create a model based on a specification and an alphabet *)
let create lk_spec = 
  let a_size = alphabet_size lk_spec in
  let (alph,gap) = lk_spec.alphabet in
  (* set up all the probability and rates *)
  let rates,probs,pinvar =
    match lk_spec.site_variation with
    | Constant -> ba_of_array1 [| 1.0 |], ba_of_array1 [| 1.0 |], None
    | DiscreteGamma (x,y) -> (* SITES,ALPHA *)
      let p = create_ba1 x in
      Bigarray.Array1.fill p (1.0 /. (float_of_int x));
      gamma_rates y y x,p,None
    | DiscreteCustom ray ->
      let rates = ba_of_array1 $ Array.map fst ray
      and probs = ba_of_array1 $ Array.map snd ray in
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
  assert( verify_rates probs rates );
  (* extract the prior probability *)
  let priors =
    let p = match lk_spec.base_priors with 
      | Equal -> Array.make a_size (1.0 /. (float_of_int a_size))
      | Empirical p ->
        let p = Array.map (fun i -> max minimum i) p in
        let sum = Array.fold_left (fun a b -> a +. b) 0.0 p in 
        if sum =. 1.0 
          then p
          else Array.map (fun i -> i /. sum) p
    in
    if not (a_size = Array.length p) then
      raise (errorf "Priors (length %d) don't match alphabet (%d)"
                    (Array.length p) a_size)
    else
      ba_of_array1 p
  in
  let _gapr = match gap with
    | Coupled x   -> Some (Alphabet.get_gap alph, x)
    | Independent -> None
    | Missing     -> None
  in
  (*  get the substitution rate matrix and set sym variable and to_formatter vars *)
  let sym, q, substitution = match lk_spec.substitution with
    | JC69  -> true,  m_jc69 a_size _gapr, JC69
    | F81   -> false, m_f81 priors a_size _gapr, F81
    | K2P t -> true,  m_k2p t a_size _gapr, lk_spec.substitution
    | F84 t -> false, m_f84 priors t a_size _gapr, lk_spec.substitution
    | HKY85 t -> false, m_hky85 priors t a_size _gapr, lk_spec.substitution
    | TN93 (ts,tv) -> false, m_tn93 priors ts tv a_size _gapr, lk_spec.substitution
    | GTR c ->
      let c = match gap, (Array.length c) with
        | Coupled _, 0 -> Array.make ((a_size*(a_size-3))/2) 1.0
        | (Independent | Missing), 0 -> Array.make (((a_size-2)*(a_size+1))/2) 1.0
        | (Coupled _ | Independent | Missing), _ -> c
      in
      false, m_gtr priors c a_size _gapr, (GTR c)
    | Const m ->
      false, m_file priors m a_size, lk_spec.substitution
    | Custom (assoc,xs) ->
      false, m_custom priors assoc xs a_size, lk_spec.substitution
  in
  let (u,d,ui) = diagonalize sym q in
  let spec =  {lk_spec with substitution; } in
  {
    spec; rates; probs; pinvar; priors; q; u; d; ui;
    opt = generate_opt_vector spec;
  }


(** Replace the priors in a model with that of an array *)
let replace_priors model array = 
  create {model.spec with base_priors = Empirical array}

let replace_subst model matrix = failwith "TODO"

let replace_rates model rates = failwith "TODO"

(** Enumerate models based on some set of criteria. *)
let enum_models ?(site_var=[`DiscreteGamma;`DiscreteTheta;`Constant])
                ?(subst_model=[`JC69;`F81;`K2P;`F84;`HKY85;`TN93;`GTR])
                ?(priors) : model -> model option =
  let apply_model_delta (substitution,base_priors,site_variation) model =
    create {model.spec with substitution; base_priors; site_variation;}
  in
  let next_models = ref subst_model and next_var = ref subst_model in
  (fun pmodel -> match !next_models,!next_var with
    | [],[] -> None
    | mo,xs -> None)


(** Compute the priors of a dataset by frequency and gap-counts *)
let compute_priors (alph,u_gap) freq_ (count,gcount) lengths : float array =
  let size = if u_gap then (Alphabet.size alph) else (Alphabet.size alph)-1 in
  let gap_contribution = (float_of_int gcount) /. (float_of_int size) in
  let gap_char = Alphabet.get_gap alph in
  let final_priors =
    if u_gap then begin
      let total_added_gaps =
        let longest = List.fold_left (fun a x-> max a x) 0 lengths in
        let add_gap = List.fold_left (fun acc x -> (longest - x) + acc) 0 lengths in
        float_of_int add_gap
      in
      freq_.(gap_char) <- freq_.(gap_char) +. total_added_gaps;
      let count = (float_of_int (count - gcount)) +. total_added_gaps;
      and weight  = (float_of_int gcount) /. (float_of_int size) in
      Array.map (fun x -> (x -. weight) /. count) freq_
    end else begin
      Array.map (fun x -> (x -. gap_contribution) /. (float_of_int count)) freq_
    end
  in
  let sum = Array.fold_left (fun a x -> a +. x) 0.0 final_priors in
  final_priors


(** estimate the model based on two sequences with attached weights *)
let classify_edges leaf1 leaf2 seq1 seq2 acc =
  let incr_map k v map =
    let v = match IntMap.mem k map with
      | true  -> v +. (IntMap.find k map)
      | false -> v 
    in
    IntMap.add k v map 
  and incr_tuple k v map =
    let v = match UnorderedTupleMap.mem k map with
      | true -> v +. (UnorderedTupleMap.find k map)
      | false -> v 
    in
    UnorderedTupleMap.add k v map
  in
  (* classify the mutation from a1 to a2 in a map *)
  let mk_transitions (tmap,fmap) (w1,s1) (w2,s2) =
    assert( w1 = w2 );
    let s1 = BitSet.to_list s1 and s2 = BitSet.to_list s2 in 
    let n1 = List.length s1   and n2 = List.length s2   in
    (* Count all the transitions A->B. Polymorphisms are counted as
     * 1/(na*nb) each, where na and nb are the number of polymorphisms
     * in and b respectively. *)
    let v = w1 /. (float_of_int (n1 * n2) ) in
    let tmap = (* cross product of states a and b *)
      List.fold_left 
        (fun map1 a ->
           List.fold_left
             (fun map2 b -> incr_tuple (a,b) v map2)
             map1 s2)
        tmap s1
    in
    (* Counts the base frequencies. Polymorphisms are counted as 1/n in
     * each base, where, as above, n is the number of polymorphisms. *)
    let n1 = w1 /. (float_of_int n1) and n2 = w2 /. (float_of_int n2) in
    let fmap = 
      if leaf1 then 
        List.fold_left (fun map a -> incr_map a n1 map) fmap s1 
      else fmap in
    let fmap = 
      if leaf2 then
        List.fold_left (fun map a -> incr_map a n2 map) fmap s2
      else fmap in
    tmap,fmap
  in
  List.fold_left2 mk_transitions acc seq1 seq2


let get_priors f_prior alph name = match f_prior with
  | Empirical x -> x.(Alphabet.get_code name alph)
  | Equal       -> 1.0 /. (float_of_int (Alphabet.size alph))


(* Develop a model from a classification --created above *)
let process_classification spec (comp_map,pis) =
  let ugap = match snd spec.alphabet with
    | Missing      -> false
    | Independent  -> true
    | Coupled _    -> true
  and alph = fst spec.alphabet in
  let f_priors,a_size = match spec.base_priors with
    | Equal when ugap -> Equal,1+Alphabet.size alph
    | Equal           -> Equal,  Alphabet.size alph
    | Empirical _     ->
      let sum = IntMap.fold (fun _ v x -> v +. x) pis 0.0
      and gap_size =
        try IntMap.find (Alphabet.get_gap alph) pis
        with Not_found -> 0.0
      in
      let l =
        let sum = if ugap then sum else sum -. gap_size in
        List.fold_left
          (fun acc (_,b,_) -> match b with
            | b when (not ugap) && (b = Alphabet.get_gap alph) -> acc
            | b ->
              let c =
                try (IntMap.find b pis) /. sum
                with Not_found -> 0.0 in
              c :: acc)
              []
          (Alphabet.to_list alph)
      in
      let ray = Array.of_list (List.rev l) in
      Empirical ray, Array.length ray
  and is_comp a b =
    (* these models assume nucleotides only; T<->C=1, A<->G=2, this is
       because this is used in DNA/nucleotide models only (k2p,hky85...) *)
    if (Alphabet.get_code "T" alph) = a then
      if (Alphabet.get_code "C" alph) = b then 1 else 0
    else if (Alphabet.get_code "C" alph) = a then
      if (Alphabet.get_code "T" alph) = b then 1 else 0
    else if (Alphabet.get_code "A" alph) = a then
      if (Alphabet.get_code "G" alph) = b then 2 else 0
    else if (Alphabet.get_code "G" alph) = a then
      if (Alphabet.get_code "A" alph) = b then 2 else 0
    else
      0
  in
  let get_prior (string:string):float = match f_priors with
    | Empirical x -> x.(Alphabet.get_code string alph)
    | Equal       -> 1.0 /. (float_of_int a_size)
  in
  let get_sva comp_map =
    let s1,s2,v,a =
      UnorderedTupleMap.fold
        (fun k v (sc1,sc2,vc,all) -> match k with
          | k1,k2 when 1 == is_comp k1 k2 -> (sc1+.v,sc2,vc,all+.v)
          | k1,k2 when 2 == is_comp k1 k2 -> (sc1,sc2+.v,vc,all+.v)
          | k1,k2 when k1 != k2           -> (sc1,sc2,vc+.v,all+.v)
          | _                             -> (sc1,sc2,vc,all+.v) )
        comp_map
        (0.0,0.0,0.0,0.0)
    in
    s1 /. a, s2 /. a, v /. a, a
  in
  (* build the model *)
  let gap = snd spec.alphabet in
  let m,gap = match spec.substitution with
    | JC69 -> JC69,gap
    | F81  -> F81,gap
    | K2P _ ->
      (* YANG: 1.12                                       *)
      (*  alpha*t = -0.5 * log(1-2S-V) + 0.25 * log(1-2V) *)
      (* 2*beta*t = -0.5 * log(1-2V)                      *)   
      (*        k = alpha / beta                          *)  
      (* k = [log(1-2S-V)-0.5*log(1-2V)] / log(1-2V)      *)
      let s1,s2,v,_ = get_sva comp_map in
      let s = s1 +. s2 in
      let numer = 2.0 *. (log (1.0-.(2.0*.s)-.v))
      and denom = log (1.0-.(2.0*.v)) in
      K2P ((numer /. denom)-.1.0),gap
    | GTR _ ->
      let tuple_sum a1 a2 map =
          let one = try UnorderedTupleMap.find (a1,a2) map 
                    with | Not_found -> 0.0
          and two = try UnorderedTupleMap.find (a2,a1) map
                    with | Not_found -> 0.0
          in
          one +. two
      in
      (* create list of transitions for GTR model creation *)
      (* 1 -> 2, 1 -> 3, 1 -> 4 ... 2 -> 3 ... *)
      begin match gap with
        | Independent | Missing ->
          let cgap = Alphabet.get_gap alph in
          let lst =
            List.fold_right
              (fun (_,alph1,_) acc1 ->
                if (not ugap) && (cgap = alph1) then
                  acc1
                else
                  List.fold_right
                    (fun (_,alph2,_) acc2 ->
                      if alph2 <= alph1 then acc2
                      else if (not ugap) && (cgap = alph2) then
                        acc2
                      else begin
                        let sum = tuple_sum alph1 alph2 comp_map in
                        sum :: acc2 
                      end)
                    (Alphabet.to_list alph) acc1)
                    (Alphabet.to_list alph) []
          in
          let sum = List.fold_left (fun a x -> x +. a) 0.0 lst in
          let lst = List.map (fun x -> x /. sum) lst in
          let arr,gap = normalize gap (Array.of_list lst) in
          let arr = Array.init ((List.length lst)-1) (fun i -> arr.(i)) in
          GTR arr,gap
        | Coupled _ ->
          let cgap = Alphabet.get_gap alph in
          let lst,gap_trans =
            List.fold_right
              (fun (_,alph1,_) acc1 ->
                if cgap = alph1 then acc1
                else
                  List.fold_right
                    (fun (_,alph2,_) ((chrt,gapt) as acc2) ->
                      if alph2 <= alph1 then acc2
                      else if cgap = alph2 then
                        let sum = tuple_sum alph1 alph2 comp_map in
                        (chrt, sum +. gapt)
                      else begin
                        let sum = tuple_sum alph1 alph2 comp_map in
                        (sum :: chrt,gapt)
                      end)
                    (Alphabet.to_list alph) acc1)
                    (Alphabet.to_list alph)
                    ([],0.0)
          in
          let sum = List.fold_left (fun a x -> x +. a) gap_trans lst in
          let lst = List.map (fun x -> x /. sum) lst in
          let gap = Coupled (gap_trans /. (sum *. (float_of_int a_size))) in
          let arr,gap = normalize gap (Array.of_list lst) in
          let arr = Array.init ((List.length lst)-1) (fun i -> arr.(i)) in
          GTR arr,gap
      end
    | F84 _ ->
      let pi_a = get_prior "A" and pi_c = get_prior "C"
      and pi_g = get_prior "G" and pi_t = get_prior "T" in
      let pi_y = pi_c +. pi_t and pi_r = pi_a +. pi_g in
      let s1,s2,v,_ = get_sva comp_map in let s = s1 +. s2 in
      let a = ~-. (log (1.0 -. (s /. (2.0 *. (pi_t*.pi_c/.pi_y +. pi_a*.pi_g/.pi_r)))
                           -. (v *.(pi_t*.pi_c *.pi_r/.pi_y +.
                                   (pi_a*.pi_g*.pi_y/.pi_r))) /.
                              (2.0 *. (pi_t*.pi_c *. pi_r +. pi_a *. pi_g *. pi_y))))
      and b = ~-. (log (1.0 -. (v/.(2.0 *. pi_y*.pi_r)))) in
      F84 (a/.b -. 1.0),gap
    | HKY85 _ ->
      let pi_a = get_prior "A" and pi_c = get_prior "C"
      and pi_g = get_prior "G" and pi_t = get_prior "T" in
      let pi_y = pi_c +. pi_t and pi_r = pi_a +. pi_g in
      let s1,s2,v,_ = get_sva comp_map in
      let s = s1 +. s2 in
      let a =
          ~-. (log (1.0 -. (s /. (2.0 *. (pi_t*.pi_c/.pi_y +. pi_a*.pi_g/.pi_r)))
                        -. (v *.(pi_t*.pi_c *.pi_r/.pi_y +.
                           (pi_a*.pi_g*.pi_y/.pi_r))) /.
                           (2.0 *. (pi_t*.pi_c *. pi_r +. pi_a *. pi_g *. pi_y))))
      and b = ~-. (log (1.0 -. (v/.(2.0 *. pi_y*.pi_r)))) in
      HKY85 (a/.b -. 1.0),gap
    | TN93 _ ->
      let pi_a = get_prior "A" and pi_c = get_prior "C"
      and pi_g = get_prior "G" and pi_t = get_prior "T" in
      let pi_y = pi_c +. pi_t and pi_r = pi_a +. pi_g
      and s1,s2,v,_ = get_sva comp_map in
      let a1 = ~-. (log (1.0 -. (pi_y*.s1/.(2.0*.pi_t*.pi_c)) -.  (v/.(2.0*.pi_y))))
      and a2 = ~-. (log (1.0 -. (pi_r*.s2/.(2.0*.pi_a*.pi_g)) -.  (v/.(2.0*.pi_r))))
      and b  = ~-. (log (1.0 -. (v /. (2.0 *. pi_y *. pi_r)))) in
      (* finally compute the ratios *)
      let k1 = (a1 -. (pi_r *. b)) /. (pi_y *. b)
      and k2 = (a2 -. (pi_y *. b)) /. (pi_r *. b) in 
      TN93 (k1,k2),gap
    | Custom _
    | Const _ -> failwith "I cannot estimate this type of model"
  and calc_invar all comp_map =
    let same = 
      List.fold_left
        (fun acc (_,ac,_) ->
          acc +. (try (UnorderedTupleMap.find (ac,ac) comp_map)
                  with | Not_found -> 0.0))
        0.0
        (Alphabet.to_list alph)
    in
    same /. all
  in
  let v = match spec.site_variation with
    | Constant -> Constant
    | DiscreteGamma (i,_) -> DiscreteGamma (i,default_alpha)
    | DiscreteTheta (i,_,_) ->
      let stuple = UnorderedTupleMap.fold (fun _ v a -> a +. v) comp_map 0.0 in
      DiscreteTheta (i,default_alpha,calc_invar stuple comp_map)
    | DiscreteCustom _ -> spec.site_variation
  in
  {
    substitution = m;
    site_variation = v;
    base_priors = f_priors;
    alphabet = alph, gap;
  }


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
