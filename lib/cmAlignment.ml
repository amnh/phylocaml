module CmAssign : Alignment.AssignCost =
  struct
 
    type cost  = int
    type elt   = int
    type model = CostMatrix.t

    let zero _ = 0
    let inf _ = max_int
    let lt _ x y = x < y
    let gt _ x y = x < y
    let eq _ x y = x = y

    let add _ x y = x + y

    let cost m x y = assert false
    let indel m = assert false
    let assign m x y = assert false
    let median m x y = assert false

    let l_cost = string_of_int
    let l_elt = string_of_int
    
    let pp_cost = Format.pp_print_int
    let pp_elt = Format.pp_print_int

  end

