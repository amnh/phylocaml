open Internal

module type AssignCostMatrix =
  sig
    type cost
    type elt
    type model

    val zero : model -> cost
    val inf  : model -> cost

    val lt : model -> cost -> cost -> bool
    val eq : model -> cost -> cost -> bool
    
    val add  : model -> cost -> cost -> cost

    val cost : model -> elt -> elt -> cost

    val indel  : model -> elt
    val assign : model -> elt -> elt -> elt
    val median : model -> elt -> elt -> cost * elt
  end

module type BaseAlignment =
  sig
    type t
    type m
    type c
    type mem

    val median_cost : m -> t -> t -> c * t
    val align : m -> t -> t -> c * t * t * t

    val fill : mem -> m -> t -> t -> unit
    val cost : mem -> m -> t -> t -> c
    val backtrace : mem -> m -> t -> t -> t * t
    val alignments : mem -> m -> t -> t -> t * t * t
    val median : mem -> m -> t -> t -> t
  end

module type A =
  functor (C : AssignCostMatrix) ->
    BaseAlignment with type t = C.elt array and type m = C.model and type c = C.cost

module Common (C:AssignCostMatrix) =
  struct

    let min3 m (x,dx) (y,dy) (z,dz) =
      let xy,dxy =
        if C.lt m x y then (x,[dx])
        else if C.eq m x y then (y,[dy;dx])
        else (y,[dy])
      in
      if C.lt m xy z then (xy,dxy)
      else if C.eq m xy z then (xy,dz::dxy)
      else (z,[dz])

    let median_cost m x y =
      let cst = ref @@ C.zero m and med = ref [] in
      assert ((Array.length x) = (Array.length y)); (* todo *)
      for i = (Array.length x)-1 downto 0 do
        let c,md = C.median m x.(i) y.(i) in
        cst := C.add m c !cst;
        med := md :: !med;
      done;
      !cst, Array.of_list !med
  end


module FullAlignment (C:AssignCostMatrix) =
  struct

    include Common (C)

    type e = C.elt
    type t = e array
    type m = C.model
    type c = C.cost

    type dir =
      Root | Align  of e | Delete of e | Insert of e
    
    type mem =
      (c * dir list) array array

    let mem_zero m = (C.zero m,[])
    let mem_ref = ref None

    let init_mem m x y = 
      Array.make_matrix (Array.length x) (Array.length y) @@ mem_zero m

    let rec create_mem m x y : mem = match !mem_ref with
      | None ->
        let mem = init_mem m x y in
        mem_ref := Some mem;
        mem
      | Some mem ->
        if (Array.length mem) <= (Array.length x) &&
           (Array.length mem.(0)) <= (Array.length y) then
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
  
    let backtrace mem m x y =
      let indel = C.indel m in
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_alignments x' y' i j = match get_direction i j with
        | Align  _ -> build_alignments (x.(i)::x') (y.(j)::y') (i-1) (j-1)
        | Insert _ -> build_alignments (indel::x') (y.(j)::y') (i)   (j-1)
        | Delete _ -> build_alignments (x.(i)::x') (indel::y') (i-1) (j)
        | Root     -> Array.of_list @@ indel::x', Array.of_list @@ indel::y'
      in
      build_alignments [] [] ((Array.length x)-1) ((Array.length y)-1)

    let median mem _m x y =
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_median med i j = match get_direction i j with
        | Align  s -> build_median (s::med) (i-1) (j-1)
        | Delete s -> build_median (s::med) (i-1) (j)
        | Insert s -> build_median (s::med) (i)   (j-1)
        | Root     -> Array.of_list (x.(0)::med)
      in
      build_median [] ((Array.length x)-1) ((Array.length y)-1)

    let alignments mem m x y =
      let indel = C.indel m in
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_alignments one two med i j = match get_direction i j with
        | Align  s -> build_alignments (x.(i)::one) (y.(j)::two) (s::med) (i-1) (j-1)
        | Insert s -> build_alignments (indel::one) (y.(j)::two) (s::med) (i)   (j-1)
        | Delete s -> build_alignments (x.(i)::one) (indel::two) (s::med) (i-1) (j)
        | Root     ->
          Array.of_list @@ indel::one,
            Array.of_list @@ indel::two,
              Array.of_list @@ indel::med
      in
      build_alignments [] [] [] ((Array.length x)-1) ((Array.length y)-1)

    let cost mem _ x y =
      fst @@ mem.((Array.length x)-1).((Array.length y)-1)

    let fill mem m x y =
      let xlen = Array.length x and ylen = Array.length y in
      let indel = C.indel m in
      let get_cost i j =
        if i = 0 && j = 0 then begin
          (C.zero m,[Root])
        end else if i = 0 then begin
          let cst,s = C.median m indel y.(j) in
          C.add m (fst mem.(i).(j-1)) cst,[Insert s]
        end else if j = 0 then begin
          let cst,s = C.median m x.(i) indel in
          C.add m (fst mem.(i-1).(j)) cst,[Delete s]
        end else begin
          let dcst,sd = C.median m x.(i) indel
          and icst,si = C.median m indel y.(j)
          and acst,sa = C.median m x.(i) y.(j) in
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
      ()

    let align m x y =
      let mem = init_mem m x y in
      let () = fill mem m x y in
      let x',y',m = alignments mem m x y in
      cost mem m x y, x', y', m
  end

module UkkAlignment (C:AssignCostMatrix) =
  struct

    include Common (C)

    type e = C.elt
    type t = e array
    type m = C.model
    type c = C.cost

    type dir =
      Root | Align  of e | Delete of e | Insert of e
    
    type mem =
      { k  : int;
        mat: (c * (int * dir list)) array array; }

    let mem_zero m = (C.zero m,(0,[]))
    let mem_ref = ref None

    let init_mem ~k m x y =
      let mat = Array.make_matrix (Array.length x) (Array.length y) @@ mem_zero m in
      {k; mat}

    let rec create_mem ~k m x y : mem = match !mem_ref with
      | None ->
        let mem = init_mem ~k m x y in
        mem_ref := Some mem;
        mem
      | Some mem ->
        if (Array.length mem.mat) <= (Array.length x)
            && (Array.length mem.mat.(0)) <= (Array.length y) then
          {mem with k = k}
        else begin
          mem_ref := None;
          create_mem ~k m x y
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
      let get_direction i j = mem.mat.(i).(j) |> snd |> snd |> choose_dir in
      let rec build_alignments x' y' med i j = match get_direction i j with
        | Align  s -> build_alignments (x.(i)::x') (y.(j)::y') (s::med) (i-1) (j-1)
        | Insert s -> build_alignments (indel::x') (y.(j)::y') (s::med) (i) (j-1)
        | Delete s -> build_alignments (x.(i)::x') (indel::y') (s::med) (i-1) (j)
        | Root     -> Array.of_list @@ indel::x',
                        Array.of_list @@ indel::y',
                          Array.of_list @@ indel::med
      in
      build_alignments [] [] [] ((Array.length x)-1) ((Array.length y)-1)

    let backtrace mem m x y =
      let indel = C.indel m in
      let get_direction i j = mem.mat.(i).(j) |> snd |> snd |> choose_dir in
      let rec build_alignments one two i j = match get_direction i j with
        | Align  _ -> build_alignments (x.(i)::one) (y.(j)::two) (i-1) (j-1)
        | Insert _ -> build_alignments (indel::one) (y.(j)::two) (i) (j-1)
        | Delete _ -> build_alignments (x.(i)::one) (indel::two) (i-1) (j)
        | Root     -> Array.of_list @@ indel::one, Array.of_list @@ indel::two
      in
      build_alignments [] [] ((Array.length x)-1) ((Array.length y)-1)

    let median mem _m x y =
      let get_direction i j = mem.mat.(i).(j) |> snd |> snd |> choose_dir in
      let rec build_median acc i j = match get_direction i j with
        | Align  s -> build_median (s::acc) (i-1) (j-1)
        | Delete s -> build_median (s::acc) (i-1) j
        | Insert s -> build_median (s::acc) i (j-1)
        | Root     -> Array.of_list (x.(0)::acc)
      in
      build_median [] ((Array.length x)-1) ((Array.length y)-1)

    let fill mem m x y =
      let indel = C.indel m in
      (* A general function to calculate the barrier of k; this is the length
         of the horizontal and vertical bands that build the diagonal strip. *)
      let barrier =
        let diff = (Array.length y) - (Array.length x) in
        (fun k -> (k - diff) / 2)
      and get_cost i j x_i y_j =
        let cst,s = C.median m x_i y_j in
        C.add m cst @@ fst mem.mat.(i).(j), fst @@ snd mem.mat.(i).(j), s
      in
      (* update a cell in the matrix by ALL its neighbors; This should only be
         used to calculate the cost of a cell when all the neighbors exist. *)
      let update_all i j =
        let aln,at,sa = get_cost (i-1) (j-1) x.(i) y.(j)
        and del,dt,sd = get_cost (i-1) (j)   x.(i) indel
        and ins,it,si = get_cost (i)   (j-1) indel       y.(j) in
        (* modify the indel/edit count *)
        let at = if x.(i) = y.(j) then at else 1+at
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
        let aln,at,sa = get_cost (i-1) (j-1) x.(i) y.(j)
        and del,dt,sd = get_cost (i-1) (j)   x.(i) indel in
        let at = if x.(i) = y.(j) then at else 1+at
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
        let aln,at,sa = get_cost (i-1) (j-1) x.(i) y.(j)
        and ins,it,si = get_cost (i)   (j-1) indel       y.(j) in
        let at = if x.(i) = y.(j) then at else 1+at
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
            if j >= Array.length y then ()
            else if j = j_max then update_col i j
            else begin update_all i j; run_row (i) (j+1) end
          in
          run_row i j_min
        in
        (* for each row, update strip with run_row *)
        let ob = barrier ok and nb = barrier nk in
        for i = 1 to (Array.length x)-1 do
          let old_j_max = i+ob+((Array.length x)-(Array.length y))
          and new_j_max = i+nb+((Array.length x)-(Array.length y))
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
        for j = 1 to (Array.length y) - 1 do
          let cost,_,s = get_cost 0 (j-1) indel y.(j) in
          mem.mat.(0).(j) <- cost,(j,[Insert s]);
        done;
        for i = 1 to (Array.length x)-1 do
          let cost,_,s = get_cost (i-1) 0 x.(i) indel in
          mem.mat.(i).(0) <- cost,(i,[Delete s]);
        done;
        build_strip (max mem.k (((Array.length x)-(Array.length y))+1))
      (* build a single strip/band in matrix *)
      and build_strip k =
        let b = barrier k in
        let p_max = ref 0 in
        for i = 1 to (Array.length x)-1 do
          let j_min = max 1 (i - b - 1)
          and j_max = min ((Array.length y)-1) (i+b+((Array.length y)-(Array.length x))) in
          if j_min = 1
            then update_all i 1
            else update_row i j_min;
          for j = j_min+1 to j_max-1 do
            update_all i j
          done;
          if !p_max = (Array.length y)-1
            then update_all i j_max
            else update_col i j_max;
          p_max := j_max;
        done;
        update k
      (* this is to update k and matrix until ending condition *)
      and update k =
        let mat_k = fst (snd (mem.mat.((Array.length x)-1).((Array.length y)-1))) in
        if (k <= mat_k) then begin
          update_matrix k (k*1);
          update (k*2)
        end else begin
          ()
        end
      in
      initial_matrix ();
      () 

    let cost mem _ x y =
      fst @@ mem.mat.((Array.length x)-1).((Array.length y)-1)

    let default_k = 3

    let align m x y =
      let mem = init_mem ~k:default_k m x y in
      let () = fill mem m x y in
      let x',y',m = alignments mem m x y in
      cost mem m x y, x', y', m
  end
