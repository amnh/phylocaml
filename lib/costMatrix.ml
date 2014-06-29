
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

    val assign : spec -> elt -> elt -> elt list
    val cost : spec -> elt -> elt -> cost
    val assign_cost : spec -> elt -> elt -> cost * elt list
    val compress : spec -> elt list -> elt

    val l_cost : spec -> cost Ppl.pp_l
    val l_elt  : spec -> elt Ppl.pp_l
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
        specification : spec;
        indel         : elt;
      }

    let fill_cm size t =
      let () =
        for i = 0 to size - 1 do for j = 0 to size-1 do
          let cost,assign = M.assign_cost t.specification i j in
          t.cost_matrix.(i).(j)   <- cost;
          t.assign_matrix.(i).(j) <- M.compress t.specification assign;
        done done
      in
      ()

    let create model =
      let alphabet = M.get_alphabet model in
      let size = match alphabet.Alphabet.kind with
        | Alphabet.BitFlag             -> 1 lsl (Alphabet.size alphabet)
        | Alphabet.Sequential          -> Alphabet.size alphabet
        | Alphabet.CombinationLevels _ -> Alphabet.(CodeMap.cardinal alphabet.comb_data.comb_set)
        | Alphabet.Continuous          -> raise (Error (`Alphabet_Does_Not_Support_Cost_Matrix alphabet))
      in
      let cost_matrix = Array.make_matrix size size (M.zero model)
      and assign_matrix = Array.make_matrix size size 0 in
      let indel = match alphabet.Alphabet.gap with
        | None   -> raise (Error (`Alphabet_Does_Not_Contain_Gap alphabet))
        | Some x -> x
      in
      let t = {cost_matrix; assign_matrix; specification = model; indel;} in
      let () = fill_cm size t in
      t

    (** extend to match [Alignment.AssignCostMatrix] *)
    let zero t = M.zero t.specification
    let inf t  = M.inf t.specification
    let add t  = M.add t.specification
    let eq t   = M.eq t.specification
    let lt t   = M.lt t.specification

    let l_cost t = assert false
    let l_elt t = assert false

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
        specification : spec;
        indel         : elt;
      }

    let create model =
      let alphabet = M.get_alphabet model in
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
      in
      {cost_matrix; assign_matrix; specification = model; indel;}

    (** extend to match [Alignment.AssignCostMatrix] *)
    let zero t = M.zero t.specification
    let inf t  = M.inf t.specification
    let add t  = M.add t.specification
    let eq t   = M.eq t.specification
    let lt t   = M.lt t.specification

    let l_cost t = assert false
    let l_elt t = assert false

    let assign t i j =
      if Hashtbl.mem t.assign_matrix (i,j)
        then Hashtbl.find t.assign_matrix (i,j)
        else begin
          let cost,assign = M.assign_cost t.specification i j in
          let assign =  M.compress t.specification assign in
          Hashtbl.add t.cost_matrix (i,j) cost;
          Hashtbl.add t.assign_matrix (i,j) assign;
          assign
        end

    let cost t i j =
      if Hashtbl.mem t.cost_matrix (i,j)
        then Hashtbl.find t.cost_matrix (i,j)
        else begin
          let cost,assign = M.assign_cost t.specification i j in
          let assign =  M.compress t.specification assign in
          Hashtbl.add t.cost_matrix (i,j) cost;
          Hashtbl.add t.assign_matrix (i,j) assign;
          cost
        end

    let median t i j = (cost t i j), (assign t i j)

    let indel t = t.indel

  end

