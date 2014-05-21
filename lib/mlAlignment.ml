
module MlCostAssign : Alignment.AssignCost =
  struct
 
    type cost  = float
    type elt   = int
    type model =
      { matrix : (float * int) array;
        indel : elt;
        model : MlModel.t;
        (** todo; how to better deal with these branches *)
        branches : float * float;
      }

    let zero _ = 0.0
    and inf  _ = infinity

    and lt _ x y = x <  y
    and gt _ x y = x >  y
    and eq _ x y = x =. y

    and add _ x y = x +. y

    and cost m x y = fst m.matrix.(x).(y)
    and indel m = m.indel
    and assign m x y = snd m.matrix.(x).(y)
    and median m x y = m.matrix.(x).(y)

    let l_cost = string_of_float
    let l_elt = string_of_int
    
    let pp_cost = Format.pp_print_float
    let pp_int = Format.pp_print_int

    (** return the minimum cost/median *)
    let get_min_costassign a ti tj i j =
      let cost,asgn =
        CodeMap.fold
          (fun k _ ((ocst,asgn) as acc) ->
            let ncst = ti.(i).(k) +. tj.(j).(k) in
            if ncst =. ocst
              then ((min ncst ocst),k::asgn)
            else if ncst < ocst
              then ncst,[k]
            else
              acc)
          a.Alphabet.elements
          (inf,[])
      in
      cost, Alphabet.compress_polymorphism asgn

    let of_tcms ti tj a : (float * int) array array =
      let matrix = Array.make_matrix a.Alphabet.size a.Alphabet.size (0.0,0) in
      for i = 0 to a.Alphabet.size -1 do
        for j = 0 to a.Alphabet.size -1 do
          matrix.(i).(j) <- get_min_cost a ti tj i j
        done;
      done;
      matrix

    let generate_model model tx ty =
      let indel = match (MlModel.get_alphabet model.MlModel.spec).gap with
        | None -> failwith "no indel in model"
        | Some x -> x
      in
      failwith "not done"

  end

