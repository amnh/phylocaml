
module type TCM =
  sig
    type spec
    type cost
    type elt

    val get_alphabet : spec -> Alphabet.t

    val zero : spec -> cost
    val inf : spec -> cost
    val lt : spec -> cost -> cost -> bool
    val eq : spec -> cost -> cost -> bool
    val add : spec -> cost -> cost -> cost
    val cost : spec -> elt -> elt -> cost
    val compress : spec -> elt list -> elt

    val to_string_cost : spec -> cost -> string
    val to_string_elt : spec -> elt -> string
    val l_cost : spec -> cost Ppl.pp_l
    val l_elt  : spec -> elt Ppl.pp_l

    val is_symmetric : spec -> bool
    val is_equal : spec -> bool
    val is_metric : spec -> bool
  end

module type CM =
  sig
    include Alignment.AssignCostMatrix with type elt = Alphabet.code

    type spec

    val create : spec -> model
  end

module Error =
  struct

    type t =
      [ `Alphabet_Does_Not_Support_Cost_Matrix of Alphabet.t
      | `Alphabet_Does_Not_Contain_Gap of Alphabet.t ]

    let to_string = function
      | `Alphabet_Does_Not_Contain_Gap _ ->
        Printf.sprintf "Alphabet does not support gap character used as indel events"
      | `Alphabet_Does_Not_Support_Cost_Matrix _ ->
        Printf.sprintf "Alphabet does not support generation of a cost-matrix"
  end

exception Error of Error.t

module Make (M:TCM with type elt = Alphabet.code) =
  struct

    type spec = M.spec
    type cost = M.cost
    type elt  = M.elt

    type model =
      { cost_matrix   : cost array array;
        assign_matrix : elt array array;
        spec          : spec;
        indel         : elt;
      }

    let find_median_pair spec istate jstate candidates =
      Alphabet.CodeSet.fold
        (fun k ((cost,assign) as acc) ->
          let kcost = M.add spec (M.cost spec istate k) (M.cost spec jstate k) in
          if M.eq spec kcost cost
            then (cost,k::assign)
          else if M.lt spec kcost cost
            then (kcost,[k])
            else acc)
        candidates
        (M.inf spec, [])

    (* exhaustively look through each median assignment and collect optimal states *)
    let find_median_general spec i j =
      let cost,assign =
        Alphabet.CodeSet.fold (fun istate ->
          Alphabet.CodeSet.fold
            (fun jstate ((cost,assign) as acc) ->
              let ncost, nassign =
                find_median_pair spec istate jstate (M.get_alphabet spec).Alphabet.atomic in
              if M.eq spec ncost cost
                then (cost, nassign@assign)
              else if M.lt spec ncost cost
                then (ncost, nassign)
                else acc)
            (Alphabet.get_combination j (M.get_alphabet spec)))
          (Alphabet.get_combination i (M.get_alphabet spec))
          (M.inf spec, [])
      in
      cost, M.compress spec assign

    (* use state intersections/unions to determine optimal median *)
    let find_median_equal (spec:spec) =
      let unequal_cost, equal_cost =
        let set = (M.get_alphabet spec).Alphabet.atomic in
        let e1 = Alphabet.CodeSet.choose set in
        let e2 = Alphabet.CodeSet.choose @@ Alphabet.CodeSet.remove e1 set in
        M.cost spec e1 e2, M.cost spec e1 e1
      in
      (fun i j ->
        let is = Alphabet.get_combination i (M.get_alphabet spec) in
        let js = Alphabet.get_combination j (M.get_alphabet spec) in
        let assign, cost =
          if Alphabet.CodeSet.is_empty @@ Alphabet.CodeSet.inter is js
            then Alphabet.CodeSet.union is js, unequal_cost
            else Alphabet.CodeSet.inter is js, equal_cost
        in
        cost, M.compress spec @@ Alphabet.CodeSet.elements assign)

    (* median must be subset of the union of parent states *)
    let find_median_metric spec i j =
      let is = Alphabet.get_combination i (M.get_alphabet spec)
      and js = Alphabet.get_combination j (M.get_alphabet spec) in
      let ks = Alphabet.CodeSet.union is js in
      let cost,assign =
        Alphabet.CodeSet.fold (fun istate ->
          Alphabet.CodeSet.fold
            (fun jstate ((cost,assign) as acc) ->
              let ncost, nassign = find_median_pair spec istate jstate ks in
              if M.eq spec ncost cost
                then (cost, nassign@assign)
              else if M.lt spec ncost cost
                then (ncost, nassign)
                else acc)
              js)
            is
          (M.inf spec,[])
      in
      cost, M.compress spec assign

    (* fill a cost matrix *)
    let fill_cm bits size model =
      let () =
        let lo,hi = if bits then 1,size-1 else 0,size-1 in
        let find_median =
          match M.is_equal model.spec, M.is_metric model.spec with
          | true, _ -> find_median_equal
          | _,true  -> find_median_metric
          | _,_     -> find_median_general
        in
        let find_median_with_debug s i j =
          let pp_ilst chan = List.iter (Printf.fprintf chan "%d;") in
          let c,a as res = find_median s i j in
          let alph = M.get_alphabet s in
          Printf.printf "%d[%a] -> %d[%a] <- %d[%a] = %s\n%!" 
            i pp_ilst (Alphabet.explode_polymorphisms [i] alph)
            a pp_ilst (Alphabet.explode_polymorphisms [a] alph)
            j pp_ilst (Alphabet.explode_polymorphisms [j] alph)
            (M.to_string_cost s c);
          res
        in
        if M.is_symmetric model.spec then begin
          for i = lo to hi do for j = i to hi do
            let cost,assign = find_median model.spec i j in
            model.cost_matrix.(i).(j)   <- cost;
            model.assign_matrix.(i).(j) <- assign;
            model.cost_matrix.(j).(i)   <- cost;
            model.assign_matrix.(j).(i) <- assign;
          done done
        end else begin
          for i = lo to hi do for j = lo to hi do
            let cost,assign = find_median model.spec i j in
            model.cost_matrix.(i).(j)   <- cost;
            model.assign_matrix.(i).(j) <- assign;
          done done
        end
      in
      ()

    let create spec =
      let alphabet = M.get_alphabet spec in
      let bits,size = match alphabet.Alphabet.kind with
        | Alphabet.BitFlag             -> true, 1 lsl (Alphabet.size alphabet)
        | Alphabet.Sequential          -> false, Alphabet.size alphabet
        | Alphabet.CombinationLevels _ -> false, Alphabet.(CodeMap.cardinal alphabet.comb_data.comb_set)
        | Alphabet.Continuous          -> raise (Error (`Alphabet_Does_Not_Support_Cost_Matrix alphabet))
      in
      let cost_matrix = Array.make_matrix size size (M.inf spec)
      and assign_matrix = Array.make_matrix size size 0 in
      let indel = match alphabet.Alphabet.gap with
        | None   -> raise (Error (`Alphabet_Does_Not_Contain_Gap alphabet))
        | Some x -> x
      in
      let model = {cost_matrix; assign_matrix; spec; indel;} in
      let () = fill_cm bits size model in
      model

    (** extend to match [Alignment.AssignCostMatrix] *)
    let zero t = M.zero t.spec
    let inf t  = M.inf t.spec
    let add t  = M.add t.spec
    let eq t   = M.eq t.spec
    let lt t   = M.lt t.spec

    let l_cost t = M.l_cost t.spec
    let l_elt t = M.l_elt t.spec

    let assign t i j = t.assign_matrix.(i).(j)
    let cost   t i j = t.cost_matrix.(i).(j)
    let median t i j = (cost t i j), (assign t i j)

    let indel t = t.indel

  end


module MakeLazy (M:TCM with type elt = Alphabet.code) =
  struct

    type spec = M.spec
    type cost = M.cost
    type elt  = M.elt

    type model =
      { cost_matrix   : (elt * elt, cost) Hashtbl.t;
        assign_matrix : (elt * elt, elt) Hashtbl.t;
        spec          : spec;
        indel         : elt;
        median_fn     : spec -> elt -> elt -> cost * elt;
      }


    let find_median_pair spec istate jstate candidates =
      Alphabet.CodeSet.fold
        (fun k ((cost,assign) as acc) ->
          let kcost = M.add spec (M.cost spec istate k) (M.cost spec jstate k) in
          if M.eq spec kcost cost
            then (cost,k::assign)
          else if M.lt spec kcost cost
            then (kcost,[k])
            else acc)
        candidates
        (M.inf spec, [])

    (* exhaustively look through each median assignment and collect optimal states *)
    let find_median_general spec i j =
      let cost,assign =
        Alphabet.CodeSet.fold (fun istate ->
          Alphabet.CodeSet.fold
            (fun jstate ((cost,assign) as acc) ->
              let ncost, nassign =
                find_median_pair spec istate jstate (M.get_alphabet spec).Alphabet.atomic in
              if M.eq spec ncost cost
                then (cost, nassign@assign)
              else if M.lt spec ncost cost
                then (ncost, nassign)
                else acc)
            (Alphabet.get_combination j (M.get_alphabet spec)))
          (Alphabet.get_combination i (M.get_alphabet spec))
          (M.inf spec, [])
      in
      cost, M.compress spec assign

    (* use state intersections/unions to determine optimal median *)
    let find_median_equal (spec:spec) =
      let unequal_cost, equal_cost =
        let set = (M.get_alphabet spec).Alphabet.atomic in
        let e1 = Alphabet.CodeSet.choose set in
        let e2 = Alphabet.CodeSet.choose @@ Alphabet.CodeSet.remove e1 set in
        M.cost spec e1 e2, M.cost spec e1 e1
      in
      (fun i j ->
        let is = Alphabet.get_combination i (M.get_alphabet spec) in
        let js = Alphabet.get_combination j (M.get_alphabet spec) in
        let assign, cost =
          if Alphabet.CodeSet.is_empty @@ Alphabet.CodeSet.inter is js
            then Alphabet.CodeSet.union is js, unequal_cost
            else Alphabet.CodeSet.inter is js, equal_cost
        in
        cost, M.compress spec @@ Alphabet.CodeSet.elements assign)

    (* median must be subset of the union of parent states *)
    let find_median_metric spec i j =
      let is = Alphabet.get_combination i (M.get_alphabet spec)
      and js = Alphabet.get_combination j (M.get_alphabet spec) in
      let ks = Alphabet.CodeSet.union is js in
      let cost,assign =
        Alphabet.CodeSet.fold (fun istate ->
          Alphabet.CodeSet.fold
            (fun jstate ((cost,assign) as acc) ->
              let ncost, nassign = find_median_pair spec istate jstate ks in
              if M.eq spec ncost cost
                then (cost, nassign@assign)
              else if M.lt spec ncost cost
                then (ncost, nassign)
                else acc)
              js)
            is
          (M.inf spec,[])
      in
      cost, M.compress spec assign

    let get_median model i j =
      if Hashtbl.mem model.cost_matrix (i,j)
        then Hashtbl.find model.cost_matrix (i,j),
             Hashtbl.find model.assign_matrix (i,j)
        else 
          let c,m as res = model.median_fn model.spec i j in
          let () = Hashtbl.add model.cost_matrix (i,j) c in
          let () = Hashtbl.add model.assign_matrix (i,j) m in
          res

    let create spec =
      let alphabet = M.get_alphabet spec in
      let size = match alphabet.Alphabet.kind with
        | Alphabet.BitFlag             -> 1 lsl (Alphabet.size alphabet)
        | Alphabet.Sequential          -> Alphabet.size alphabet
        | Alphabet.CombinationLevels _ -> Alphabet.(CodeMap.cardinal alphabet.comb_data.comb_set)
        | Alphabet.Continuous          -> raise (Error (`Alphabet_Does_Not_Support_Cost_Matrix alphabet))
      in
      let cost_matrix = Hashtbl.create 1789 and assign_matrix = Hashtbl.create 1789 in
      let indel = match alphabet.Alphabet.gap with
        | None   -> raise (Error (`Alphabet_Does_Not_Contain_Gap alphabet))
        | Some x -> x
      and median_fn : spec -> elt -> elt -> cost * elt = 
        match M.is_equal spec, M.is_symmetric spec, M.is_metric spec with
        | true,_,_ -> find_median_equal
        | _, true, true ->
            (fun s i j ->
              let i,j = if i < j then i,j else j,i in
              find_median_metric s i j)
        | _, _, true -> find_median_metric
      in
      {cost_matrix; assign_matrix; spec; indel; median_fn; }

    (** extend to match [Alignment.AssignCostMatrix] *)
    let zero t = M.zero t.spec
    let inf t  = M.inf t.spec
    let add t  = M.add t.spec
    let eq t   = M.eq t.spec
    let lt t   = M.lt t.spec

    let l_cost t = M.l_cost t.spec
    let l_elt t = M.l_elt t.spec

    let assign t i j = snd @@ get_median t i j
    let cost t i j   = fst @@ get_median t i j
    let median t i j =        get_median t i j

    let indel t = t.indel

  end

