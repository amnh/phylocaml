open Internal

module type AssignCost =
  sig
    type cost
    type elt
    type model

    val zero : model -> cost
    val inf  : model -> cost

    val lt : model -> cost -> cost -> bool
    val gt : model -> cost -> cost -> bool
    val eq : model -> cost -> cost -> bool
    
    val add  : model -> cost -> cost -> cost

    val cost : model -> elt -> elt -> cost

    val indel  : model -> elt
    val assign : model -> elt -> elt -> elt
    val median : model -> elt -> elt -> cost * elt

    val pp_cost : Format.formatter -> cost -> unit
    val pp_elt : Format.formatter -> elt -> unit
  end

module type DataVector =
  sig
    type elt
    type t

    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> t
    val unsafe_get : int -> t -> elt
    val unsafe_set : int -> elt -> t -> t
    val of_list : elt list -> t

    val pp_t : Format.formatter -> t -> unit
  end

module type Alignment =
  sig

    type t
    type m
    type c
    type mem

    val alignments : mem -> m -> t -> t -> t * t
    val backtrace : mem -> m -> t -> t -> t
    val align : mem -> m -> t -> t -> c
    val aligned : m -> t -> t -> c * t

    val l_mem : mem Ppl.pp_l
  end

module Common (V:DataVector) (C:AssignCost with type elt = V.elt) =
  struct

    let min3 m (x,dx) (y,dy) (z,dz) =
      let xy,dxy =
        if C.lt m x y then (x,[dx])
        else if C.eq m x y then (y,[dy;dx])
        else (y,[dy])
      in
      if C.eq m xy z then (xy,dxy)
      else if C.eq m xy z then (xy,dz::dxy)
      else (z,[dz])

    let aligned m x y =
      let cst = ref @@ C.zero m and med = ref [] in
      assert ((V.length x) = (V.length y)); (* todo *)
      for i = (V.length x)-1 downto 0 do
        let c,md = C.median m (V.get x i) (V.get y i) in
        cst := C.add m c !cst;
        med := md :: !med;
      done;
      !cst, V.of_list !med

  end


module FullAlignment (V:DataVector) (C:AssignCost with type elt = V.elt) =
  struct

    include Common (V) (C)

    type e = V.elt
    type t = V.t
    type m = C.model
    type c = C.cost

    type dir =
      Root | Align  of e | Delete of e | Insert of e
    
    type mem =
      (c * dir list) array array

    let mem_zero m = (C.zero m,[])
    let mem_ref = ref None
    let rec create_mem m x y : mem = match !mem_ref with
      | None ->
        let mem = Array.make_matrix (V.length x) (V.length y) @@ mem_zero m in
        mem_ref := Some mem;
        mem
      | Some mem ->
        if (Array.length mem) <= (V.length x) &&
           (Array.length mem.(0)) <= (V.length y) then
          mem
        else begin
          mem_ref := None;
          create_mem m x y
        end

    let choose_dir dirs =
      let is_del = function
        | Delete _ -> true
        | (Align _ | Insert _ | Root) -> false
      and is_aln = function
        | Align  _ -> true
        | (Delete _ | Insert _ | Root) -> false
      and is_ins = function
        | Insert _ -> true
        | (Align _ | Delete _ | Root) -> false
      and is_root = function
        | Root     -> true 
        | (Align _ | Delete _ | Insert _) -> false
      in
           if List.exists is_del dirs then List.find is_del dirs
      else if List.exists is_aln dirs then List.find is_aln dirs
      else if List.exists is_ins dirs then List.find is_ins dirs
      else match dirs with
           | [x] when is_root x -> Root
           |  _ -> assert false
  
    let alignments mem m x y =
      let indel = C.indel m in
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_alignments one two i j = match get_direction i j with
        | Align  _ -> build_alignments ((V.get x i)::one) ((V.get y j)::two) (i-1) (j-1)
        | Insert _ -> build_alignments (indel ::one) ((V.get y j)::two) (i) (j-1)
        | Delete _ -> build_alignments ((V.get x i)::one) (indel::two) (i-1) (j)
        | Root     -> V.of_list @@ indel::one, V.of_list @@ indel::two
      in
      build_alignments [] [] ((V.length x)-1) ((V.length y)-1)

    let backtrace mem _m x y =
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_median acc i j = match get_direction i j with
        | Align  s -> build_median (s::acc) (i-1) (j-1)
        | Delete s -> build_median (s::acc) (i-1) j
        | Insert s -> build_median (s::acc) i (j-1)
        | Root     -> V.of_list ((V.get x 0)::acc)
      in
      build_median [] ((V.length x)-1) ((V.length y)-1)

    let align mem m x y =
      let xlen = V.length x and ylen = V.length y in
      let indel = C.indel m in
      let get_cost i j =
        if i = 0 && j = 0 then begin
          (C.zero m,[Root])
        end else if i = 0 then begin
          let cst,s = C.median m indel (V.get y j) in
          C.add m (fst mem.(i).(j-1)) cst,[Insert s]
        end else if j = 0 then begin
          let cst,s = C.median m (V.get x i) indel in
          C.add m (fst mem.(i-1).(j)) cst,[Delete s]
        end else begin
          let dcst,sd = C.median m (V.get x i) indel
          and icst,si = C.median m indel (V.get y j)
          and acst,sa = C.median m (V.get x i) (V.get y j) in
          let dcst = C.add m (fst mem.(i-1).(j)) dcst
          and icst = C.add m (fst mem.(i).(j-1)) icst
          and acst = C.add m (fst mem.(i-1).(j-1)) acst in
          min3 m (dcst,Delete sd) (icst,Insert si) (acst,Align sa)
        end
      in
      for i = 0 to xlen - 1 do
        for j = 0 to ylen - 1 do
          mem.(i).(j) <-get_cost i j
        done;
      done;
      fst mem.(xlen-1).(ylen-1)

    let l_mem (mat : mem) =
      let l_cell (x,tds) =
        (List.fold_left
          (fun acc -> function
            | Insert _-> acc ^ " \\leftarrow "
            | Delete _-> acc ^ " \\uparrow "
            | Align  _-> acc ^ " \\nwarrow "
            | Root    -> acc)
          "{"
          tds) ^ (pp_to_string C.pp_cost x) ^ "}"
      in
      failwith "TODO"
      (* Ppl.l_matrix ... *)

  end

module UkkAlignment  (V:DataVector) (C:AssignCost with type elt = V.elt) =
  struct

    include Common (V) (C)

    type e = V.elt
    type t = V.t
    type m = C.model
    type c = C.cost

    type dir =
      Root | Align  of e | Delete of e | Insert of e
    
    type mem =
      { k  : int;
        mat: (c * (int * dir list)) array array; }

    let mem_zero m = (C.zero m,(0,[]))
    let mem_ref = ref None
    let rec create_mem ~k m x y : mem = match !mem_ref with
      | None ->
        let mat = Array.make_matrix (V.length x) (V.length y) @@ mem_zero m in
        let mem = {k; mat} in
        mem_ref := Some mem;
        mem
      | Some mem ->
        if (Array.length mem.mat) <= (V.length x)
            && (Array.length mem.mat.(0)) <= (V.length y) then
          {mem with k = k}
        else begin
          mem_ref := None;
          create_mem k m x y
        end

    let choose_dir dirs =
      let is_del = function
        | Delete _ -> true
        | (Align _ | Insert _ | Root) -> false
      and is_aln = function
        | Align  _ -> true
        | (Delete _ | Insert _ | Root) -> false
      and is_ins = function
        | Insert _ -> true
        | (Align _ | Delete _ | Root) -> false
      and is_root = function
        | Root     -> true 
        | (Align _ | Delete _ | Insert _) -> false
      in
           if List.exists is_del dirs then List.find is_del dirs
      else if List.exists is_aln dirs then List.find is_aln dirs
      else if List.exists is_ins dirs then List.find is_ins dirs
      else match dirs with
           | [x] when is_root x -> Root
           |  _ -> assert false

    let alignments (mem:mem) m x y =
      let indel = C.indel m in
      let get_direction i j = mem.mat.(i).(j) |> snd |> snd |> choose_dir in
      let rec build_alignments one two i j = match get_direction i j with
        | Align  _ -> build_alignments ((V.get x i)::one) ((V.get y j)::two) (i-1) (j-1)
        | Insert _ -> build_alignments (indel ::one) ((V.get y j)::two) (i) (j-1)
        | Delete _ -> build_alignments ((V.get x i)::one) (indel::two) (i-1) (j)
        | Root     -> V.of_list @@ indel::one, V.of_list @@ indel::two
      in
      build_alignments [] [] ((V.length x)-1) ((V.length y)-1)

    let backtrace mem _m x y =
      let get_direction i j = mem.mat.(i).(j) |> snd |> snd |> choose_dir in
      let rec build_median acc i j = match get_direction i j with
        | Align  s -> build_median (s::acc) (i-1) (j-1)
        | Delete s -> build_median (s::acc) (i-1) j
        | Insert s -> build_median (s::acc) i (j-1)
        | Root     -> V.of_list ((V.get x 0)::acc)
      in
      build_median [] ((V.length x)-1) ((V.length y)-1)

    let align mem m x y =
        let indel = C.indel m in
        (* A general function to calculate the barrier of k; this is the length
           of the horizontal and vertical bands that build the diagonal strip. *)
        let barrier =
          let diff = (V.length y) - (V.length x) in
          (fun k -> (k - diff) / 2)
        and get_cost i j x_i y_j =
          let cst,s = C.median m (V.get x i) indel in
          C.add m cst @@ fst mem.mat.(i).(j),
            fst @@ snd mem.mat.(i).(j),
             s
        in
        (* update a cell in the matrix by ALL its neighbors; This should only be
           used to calculate the cost of a cell when all the neighbors exist. *)
        let update_all i j =
            let aln,at,sa = get_cost (i-1) (j-1) (V.get x i) (V.get y j)
            and del,dt,sd = get_cost (i-1) (j)   (V.get x i) indel
            and ins,it,si = get_cost (i)   (j-1) indel       (V.get y j) in
            (* modify the indel/edit count *)
            let at = if (V.get x i) = (V.get y j) then at else 1+at
            and it = it+1
            and dt = dt+1 in
            (* the minimum cost with minimum indel, adjusted with additional
               indel if necessary. *)
            let m =
                List.fold_left
                    (fun ((cmin,(imin,dmin)) as amin) (ccur,(icur,dcur)) ->
                        if C.lt m cmin ccur then amin
                        else if C.lt m ccur cmin then (ccur,(icur,[dcur]))
                        else begin (* equal cost; check indels/substitutions *)
                            if imin < icur then amin
                            else if icur < imin then (ccur,(icur,[dcur]))
                            else (ccur,(icur,dcur::dmin))
                        end)
                    (aln,(at,[Align sa]))
                    [(del,(dt,Delete sd)); (ins,(it,Insert si))]
            in
            mem.mat.(i).(j) <- m
        (* Same as above, but will not look at the node to the right (j-1) *)
        and update_row i j =
            let aln,at,sa = get_cost (i-1) (j-1) (V.get x i) (V.get y j)
            and del,dt,sd = get_cost (i-1) (j)   (V.get x i) indel in
            let at = if (V.get x i) = (V.get y j) then at else 1+at
            and dt = dt+1 in
            let m =
                if C.lt m del aln then del,(dt,[Delete sd])
                else if C.lt m aln del then aln,(at,[Align sa])
                else begin (* equal cost; check indels/substitutions *)
                    if dt < at then del,(dt,[Delete sd])
                    else if at < dt then aln,(at,[Align sa])
                    else aln,(at,[Delete sd;Align sa])
                end
            in
            mem.mat.(i).(j) <- m
        (* Same as above, but will not look at the node above (i-1) *)
        and update_col i j =
            let aln,at,sa = get_cost (i-1) (j-1) (V.get x i) (V.get y j)
            and ins,it,si = get_cost (i)   (j-1) indel       (V.get y j) in
            let at = if (V.get x i) = (V.get y j) then at else 1+at
            and it = it+1 in
            let m =
                if C.lt m ins aln then ins,(it,[Insert si])
                else if C.lt m aln ins then aln,(at,[Align sa])
                else begin
                    if it < at then ins,(it,[Insert si])
                    else if at < it then aln,(at,[Align sa])
                    else aln,(at,[Insert si;Align sa])
                end
            in
            mem.mat.(i).(j) <- m
        in
        let rec update_matrix (ok:int) (nk:int): unit =
            (* move across each row and update *)
            let run_row i j_min j_max =
                let rec run_row i j =
                    if j >= V.length y then ()
                    else if j = j_max then update_col i j
                    else begin update_all i j; run_row (i) (j+1) end
                in
                run_row i j_min
            in
            (* for each row, update strip with run_row *)
            let ob = barrier ok and nb = barrier nk in
            for i = 1 to (V.length x)-1 do
                let old_j_max = i+ob+((V.length x)-(V.length y))
                and new_j_max = i+nb+((V.length x)-(V.length y))
                and new_j_min = i - nb in
                let old_j_min = i - ob in
                if old_j_min <= 1 then
                  run_row i (old_j_max) (new_j_max)
                else if new_j_min < 1 then
                  run_row i 1 new_j_max
                else begin
                  update_row i new_j_min;
                  run_row i (new_j_min+1) new_j_max
                end
            done
        (* set root and first row and col *)
        and initial_matrix () =
            mem.mat.(0).(0) <- (C.zero m,(0,[Root]));
            for j = 1 to (V.length y) - 1 do
                let cost,_,s = get_cost 0 (j-1) indel (V.get y j) in
                mem.mat.(0).(j) <- cost,(j,[Insert s]);
            done;
            for i = 1 to (V.length x)-1 do
                let cost,_,s = get_cost (i-1) 0 (V.get x i) indel in
                mem.mat.(i).(0) <- cost,(i,[Delete s]);
            done;
            build_strip (max mem.k (((V.length x)-(V.length y))+1))
        (* build a single strip/band in matrix *)
        and build_strip k =
            let b = barrier k in
            let p_max = ref 0 in
            for i = 1 to (V.length x)-1 do
                let j_min = max 1 (i - b - 1)
                and j_max = min ((V.length y)-1) (i+b+((V.length y)-(V.length x))) in
                if j_min = 1
                    then update_all i 1
                    else update_row i j_min;
                for j = j_min+1 to j_max-1 do
                    update_all i j
                done;
                if !p_max = (V.length y)-1
                    then update_all i j_max
                    else update_col i j_max;
                p_max := j_max;
            done;
            update k
        (* this is to update k and matrix until ending condition *)
        and update k =
            let mat_k = fst (snd (mem.mat.((V.length x)-1).((V.length y)-1))) in
            if (k <= mat_k) then begin
                update_matrix k (k*2);
                update (k*2)
            end else begin
                ()
            end
        in
        initial_matrix ();
        fst (mem.mat.((V.length x)-1).((V.length y)-1))

    let l_mem (mat : mem) = failwith "TODO"
  end
