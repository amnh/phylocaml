open Internal

module X : CostMatrix.TCM =
  struct
 
    type t =
      { matrix : MlModel.t;
        branches : float * float;
      }
 
    type cost  = float
    type elt   = Alphabet.code

    let zero _ = 0.0
    and inf  _ = infinity

    and lt _ x y = x <  y
    and gt _ x y = x >  y
    and eq _ x y = x =. y

    and add _ x y = x +. y

    and cost m x y = assert false
    and assign m x y = assert false
    and median m x y = assert false
    and assign_cost m x y = assert false

    let l_cost _ = string_of_float
    let l_elt _ = string_of_int
    
    (* return the minimum cost/median 
    let get_min_costassign a ti tj i j =
      let cost,asgn =
        Alphabet.CodeMap.fold
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
    *)
  end

include X
