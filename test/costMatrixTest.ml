open OUnit2
open TestInternal

module MockTCM =
  struct
    type spec = { alphabet : Alphabet.t; properties : bool * bool * bool; }
    type cost = int
    type elt = Alphabet.code

    let get_alphabet s = s.alphabet

    let zero _ = 0
    let inf _ = max_int
    let lt _ x y = x < y
    let eq _ x y = x = y
    let add _ x y = x + y
    let cost a x y =
      let x = Alphabet.get_combination x a.alphabet
      and y = Alphabet.get_combination y a.alphabet in
      let xy = Alphabet.CodeSet.inter x y in
      if Alphabet.CodeSet.is_empty xy then 1 else 0

    let compress a e = Alphabet.compress_polymorphisms e a.alphabet

    let to_string_cost _ = string_of_int
    let to_string_elt _ = string_of_int

    let is_symmetric x = let s,_,_ = x.properties in s
    let is_equal     x = let _,e,_ = x.properties in e
    let is_metric    x = let _,_,m = x.properties in m
  end

module type INTCM = CostMatrix.CM with type cost = int and type spec = MockTCM.spec

module MockCostMatrix = CostMatrix.Make(MockTCM)

module MockLazyCostMatrix = CostMatrix.MakeLazy(MockTCM)

let make_spec alphabet s e m =
  { MockTCM.alphabet = alphabet; properties=(s,e,m); }

let compare_two_matrices (module X:INTCM) (module Y:INTCM) xspec yspec set =
  let xmodel = X.create xspec in
  let ymodel = Y.create yspec in 
  Alphabet.CodeSet.iter (fun i ->
    Alphabet.CodeSet.iter (fun j ->
      let c11 = X.cost xmodel i j
      and a11 = X.assign xmodel i j
      and c12,a12 = X.median xmodel i j in
      let c21 = Y.cost ymodel i j
      and a21 = Y.assign ymodel i j
      and c22,a22 = Y.median ymodel i j in
      assert_equal_int c11 c12; assert_equal_int a11 a12;
      assert_equal_int c21 c22; assert_equal_int a21 a22;
      assert_equal_int c11 c22; assert_equal_int a11 a22)
    set)
  set

let tests = "CostMatrix" >:::
[
  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (Sequential)" >::
  (fun _ctxt ->
    let alphabet = Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
    let spec = make_spec alphabet true true true in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockLazyCostMatrix:INTCM) spec spec alphabet.Alphabet.atomic);

  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (BitSet)" >::
  (fun _ctxt ->
    let alphabet = Alphabet.to_bitflag @@ Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
    let spec = make_spec alphabet true true true in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockLazyCostMatrix:INTCM) spec spec alphabet.Alphabet.atomic);

  "Compare all pairs for Cost Matrix and Lazy Cost Matrix (Level)" >::
  (fun _ctxt ->
    let alphabet =
      let alphabet = Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
      let level = 2 + Random.int (Alphabet.CodeSet.cardinal alphabet.Alphabet.atomic) in
      Alphabet.to_level level alphabet
    in
    let spec = make_spec alphabet true true true in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockLazyCostMatrix:INTCM) spec spec alphabet.Alphabet.atomic);

  "Test some basic alignment costs and assignments" >::
  (fun _ctxt ->
    let verify_transformation matrix x y =
      let xs = Alphabet.get_combination x Alphabet.dna in
      let ys = Alphabet.get_combination y Alphabet.dna in
      let exmed, excst =
        let median,cost =
          if Alphabet.CodeSet.is_empty (Alphabet.CodeSet.inter xs ys)
            then Alphabet.CodeSet.union xs ys,1
            else Alphabet.CodeSet.inter xs ys,0
        in
        match Alphabet.get_state_combination median Alphabet.dna with
          | None -> assert false
          | Some x -> x,cost
      in
      let cst, med = MockCostMatrix.median matrix x y in
      assert_equal_int exmed med;
      assert_equal_int excst cst
    in
    let spec = make_spec Alphabet.dna true true true in
    let matrix = MockCostMatrix.create spec in
    Alphabet.CodeMap.iter (fun k1 _ ->
      Alphabet.CodeMap.iter (fun k2 _ ->
        verify_transformation matrix k1 k2)
      Alphabet.dna.Alphabet.code_name)
    Alphabet.dna.Alphabet.code_name);
 
  "Compare costs between General and Equal cost matrix" >::
  (fun _ctxt ->
    let alphabet = Alphabet.to_bitflag @@ Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
    let spec1 = make_spec alphabet true true true in
    let spec2 = make_spec alphabet false false false in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockCostMatrix:INTCM) spec1 spec2 alphabet.Alphabet.atomic);

  "Compare costs between Metric and Equal cost matrix" >::
  (fun _ctxt ->
    let alphabet = Alphabet.to_bitflag @@ Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
    let spec1 = make_spec alphabet true true true in
    let spec2 = make_spec alphabet false false true in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockCostMatrix:INTCM) spec1 spec2 alphabet.Alphabet.atomic);

  "Compare costs between Symmetric and Equal cost matrix" >::
  (fun _ctxt ->
    let alphabet = Alphabet.to_bitflag @@ Alphabet.generate_seq_alphabet (1 + (Random.int 3)) in
    let spec1 = make_spec alphabet true true true in
    let spec2 = make_spec alphabet true false true in
    compare_two_matrices (module MockCostMatrix:INTCM) (module MockCostMatrix:INTCM) spec1 spec2 alphabet.Alphabet.atomic);

  "Verify failure on continuous alphabet" >::
  (fun _ctxt ->
    let spec = make_spec Alphabet.continuous true true true in
    let a () = MockCostMatrix.create spec in
    let b () = MockLazyCostMatrix.create spec in
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Support_Cost_Matrix Alphabet.continuous)) a;
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Support_Cost_Matrix Alphabet.continuous)) b);

  "Verify failure on alphabet without gap/indel character" >::
  (fun _ctxt ->
    let alphabet = 
      Alphabet.bitflag_alphabet ~states:[("1",None);("2",None);("3",None);("4",None)]
                                ~equates:[] ~gap:None  ~all:None ~missing:None
                                ~orientation:true ~case:true
    in
    let spec = make_spec alphabet true true true in
    let a () = MockCostMatrix.create spec in
    let b () = MockLazyCostMatrix.create spec in
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Contain_Gap alphabet)) a;
    assert_raises (CostMatrix.Error (`Alphabet_Does_Not_Contain_Gap alphabet)) b);
]

