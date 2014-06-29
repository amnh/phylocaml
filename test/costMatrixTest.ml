open OUnit2
open TestInternal
open Topology

module MockTCM =
  struct
    type spec = Alphabet.t
    type cost = int
    type elt = Alphabet.code

    let get_alphabet s = s

    let zero _ = 0
    let inf _ = max_int
    let lt _ x y = x < y
    let eq _ x y = x = y
    let add _ x y = x + y

    (* define a matrix of 0s along diagonal, and 1s everywhere else *)
    let assign _ x y = if x = y then [x] else [x;y]
    let cost _ x y = if x = y then 0 else 1
    let assign_cost t x y = (cost t x y),(assign t x y)

    let compress a e = Alphabet.compress_polymorphisms e a

    let l_cost _ = string_of_int
    let l_elt _ = string_of_int
  end

module MockCostMatrix = CostMatrix.Make(MockTCM)

module MockLazyCostMatrix = CostMatrix.MakeLazy(MockTCM)

let tests = "CostMatrix" >:::
[
  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (Sequential)" >::
  (fun _ctxt ->
    let alphabet = Alphabet.generate_seq_alphabet (1 + (Random.int 5)) in
    let cost_matrix = MockCostMatrix.create alphabet in
    let lazy_cost_matrix = MockLazyCostMatrix.create alphabet in
    Alphabet.CodeSet.iter (fun i ->
      Alphabet.CodeSet.iter (fun j ->
        let c11 = MockCostMatrix.cost cost_matrix i j
        and a11 = MockCostMatrix.assign cost_matrix i j
        and c12,a12 = MockCostMatrix.median cost_matrix i j in
        let c21 = MockLazyCostMatrix.cost lazy_cost_matrix i j
        and a21 = MockLazyCostMatrix.assign lazy_cost_matrix i j
        and c22,a22 = MockLazyCostMatrix.median lazy_cost_matrix i j in
        assert_equal_int c11 c12; assert_equal_int a11 a12;
        assert_equal_int c21 c22; assert_equal_int a21 a22;
        assert_equal_int c11 c22; assert_equal_int a11 a22)
      alphabet.Alphabet.atomic)
    alphabet.Alphabet.atomic);

  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (BitSet)" >::
  (fun _ctxt ->
    let alphabet = Alphabet.to_bitflag @@ Alphabet.generate_seq_alphabet (1 + (Random.int 5)) in
    let cost_matrix = MockCostMatrix.create alphabet in
    let lazy_cost_matrix = MockLazyCostMatrix.create alphabet in
    Alphabet.CodeSet.iter (fun i ->
      Alphabet.CodeSet.iter (fun j ->
        let c11 = MockCostMatrix.cost cost_matrix i j
        and a11 = MockCostMatrix.assign cost_matrix i j
        and c12,a12 = MockCostMatrix.median cost_matrix i j in
        let c21 = MockLazyCostMatrix.cost lazy_cost_matrix i j
        and a21 = MockLazyCostMatrix.assign lazy_cost_matrix i j
        and c22,a22 = MockLazyCostMatrix.median lazy_cost_matrix i j in
        assert_equal_int c11 c12; assert_equal_int a11 a12;
        assert_equal_int c21 c22; assert_equal_int a21 a22;
        assert_equal_int c11 c22; assert_equal_int a11 a22)
      alphabet.Alphabet.atomic)
    alphabet.Alphabet.atomic);

  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (Level)" >::
  (fun _ctxt ->
    let alphabet =
      let alphabet = Alphabet.generate_seq_alphabet (1 + (Random.int 5)) in
      let level = 2 + Random.int (Alphabet.CodeSet.cardinal alphabet.Alphabet.atomic) in
      Alphabet.to_level level alphabet
    in
    let cost_matrix = MockCostMatrix.create alphabet in
    let lazy_cost_matrix = MockLazyCostMatrix.create alphabet in
    Alphabet.CodeSet.iter (fun i ->
      Alphabet.CodeSet.iter (fun j ->
        let c11 = MockCostMatrix.cost cost_matrix i j
        and a11 = MockCostMatrix.assign cost_matrix i j
        and c12,a12 = MockCostMatrix.median cost_matrix i j in
        let c21 = MockLazyCostMatrix.cost lazy_cost_matrix i j
        and a21 = MockLazyCostMatrix.assign lazy_cost_matrix i j
        and c22,a22 = MockLazyCostMatrix.median lazy_cost_matrix i j in
        assert_equal_int c11 c12; assert_equal_int a11 a12;
        assert_equal_int c21 c22; assert_equal_int a21 a22;
        assert_equal_int c11 c22; assert_equal_int a11 a22)
      alphabet.Alphabet.atomic)
    alphabet.Alphabet.atomic);

  "Verify failure on continuous alphabet" >::
  (fun _ctxt ->
    let a () = MockCostMatrix.create Alphabet.continuous in
    let b () = MockLazyCostMatrix.create Alphabet.continuous in
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Support_Cost_Matrix Alphabet.continuous)) a;
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Support_Cost_Matrix Alphabet.continuous)) b);
      
]

