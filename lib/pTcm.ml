open Internal 

module X : CostMatrix.TCM =
  struct

    type spec = { matrix : int array array; 
                alphabet : Alphabet.t;
              properties : bool * bool * bool; }

    type cost = int
    type elt = Alphabet.code

    let get_alphabet spec = spec.alphabet

    let zero _ = 0
    let inf _ = max_int

    let lt _ x y = x < y
    let eq _ x y = x = y

    let add _ x y = x + y
    let cost m x y = m.matrix.(x).(y)
    let compress m e = Alphabet.compress_polymorphisms e @@ get_alphabet m

    let to_string_cost _ = string_of_int
    let to_string_elt _  = string_of_int

    let is_equal m = let r,_,_ = m.properties in r

    let is_symmetric m = let _,r,_ = m.properties in r

    let is_metric m = let _,_,r = m.properties in r

    let create matrix alphabet : spec = 
      let properties l =
        let w = Array.length l in
        let is_positive = ref true and is_triangle = ref true in
        let is_equal = ref true and is_symmetric = ref true in
        for i = 0 to w - 1 do
          for j = 0 to w - 1 do
            for k = 0 to w - 1 do
              is_triangle := !is_triangle && (l.(i).(j) <= l.(i).(k) + l.(k).(j))
            done;
            is_positive := !is_positive && (l.(i).(j) >= 0);
            is_symmetric := !is_symmetric && (l.(i).(j) = l.(j).(i));
            is_equal := !is_equal && l.(i).(j) = l.(0).(1);
          done;
        done;
        !is_equal, !is_symmetric, !is_positive && !is_triangle
      in
      { matrix; alphabet; properties = properties matrix; }
  end

include X
