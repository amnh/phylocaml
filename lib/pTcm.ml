module X : CostMatrix.TCM =
  struct
 
    type spec = { matrix : int array array; alphabet : Alphabet.t; }
    type cost = int
    type elt = Alphabet.code
    
    let get_alphabet spec = spec.alphabet

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

    and compress m e = Alphabet.compress_polymorphisms e @@ get_alphabet m

    let l_cost _ = string_of_int
    let l_elt _ = string_of_int
    
  end

include X
