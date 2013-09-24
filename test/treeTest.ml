open OUnit
open Internal
open Topology

let test_0 () =
  let tree = Tree.empty in
  assert_equal 0 (IDSet.cardinal   (Tree.get_leaves tree));
  assert_equal 0 (IDSet.cardinal   (Tree.get_singles tree));
  assert_equal 0 (EdgeSet.cardinal (Tree.get_all_edges tree));
  ()

let test_1 () =
  let ids = 0 -- 9 in (* 10 leaves *)
  let tree = Tree.random ids in
  assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
  assert_equal  0 (IDSet.cardinal   (Tree.get_singles tree));
  assert_equal 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
  ()

let test_2 () = 
  let ids = 0 -- 9 in (* 10 leaves *)
  let tree = Tree.random ids |> Tree.disjoint in
  assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
  assert_equal 10 (IDSet.cardinal   (Tree.get_singles tree));
  assert_equal  0 (EdgeSet.cardinal (Tree.get_all_edges tree));
  ()

let test_3 () =
  let tree = Tree.create (0 -- 9) in (* 10 leaves *)
  let tree =
    match IDSet.elements (Tree.get_singles tree) with
    | x1::x2::xs ->
      let tree = fst @@ Tree.join (`Single x1) (`Single x2) tree in
      List.fold_left
        (fun tree n ->
          let e = Tree.random_edge tree in
          fst @@ Tree.join (`Single n) (`Edge e) tree)
        tree
        xs
    | xs ->
      assert_equal 10 (List.length xs);
      tree
  in
  assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
  assert_equal  0 (IDSet.cardinal   (Tree.get_singles tree));
  assert_equal 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
  ()

let test_4 () =
  let tree = Tree.random (0 -- 9) in
  EdgeSet.iter
    (fun ((a,b) as e) ->
      let errmsg = Printf.sprintf "Non-Disjoint Partition from %d and %d)" a b in
      let l,r,d = Tree.partition_edge e tree in
      let l_minus_r = IDSet.inter l r and r_minus_l = IDSet.inter r l in
      let () = assert_bool errmsg d in
      let () = errmsg @? (IDSet.is_empty l_minus_r) in
      let () = errmsg @? (IDSet.is_empty r_minus_l) in
      let () = assert_equal 10 (IDSet.cardinal (IDSet.union l r)) in
      ())
    (Tree.get_all_edges tree);
  ()


let local_tests = 
  [
    "Empty Tree property cardinalities"   >:: test_1;
    "Tree property cardinalities"         >:: test_1;
    "Disjoint Tree property cardinalites" >:: test_2; 
    "Iterative Tree Construction"         >:: test_3;
    "Partition Tree Edges"                >:: test_4;
  ]

let tests = "Tree" >::: local_tests

