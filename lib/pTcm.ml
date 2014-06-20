module X : CostMatrix.TCM =
  struct
 
    type t = int array array (* tcm *)
    type cost = int
    type elt = Alphabet.code

    let zero _ = 0
    let inf _ = max_int
    let lt _ x y = x < y
    let gt _ x y = x < y
    let eq _ x y = x = y

    let add _ x y = x + y

    let cost m x y = assert false
    let assign m x y = assert false
    let median m x y = assert false
    let assign_cost m x y = assert false

    let l_cost _ = string_of_int
    let l_elt _ = string_of_int
    
  end

include X
