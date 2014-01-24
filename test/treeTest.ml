open OUnit
open TestInternal
open Topology

let tests = "Tree" >:::
[
  "Empty Tree property cardinalities" >::
  (fun () ->
    let tree = Tree.empty in
    assert_equal_int 0 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int 0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int 0 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Tree property cardinalities" >::
  (fun () ->
    let ids = 0 -- 9 in (* 10 leaves *)
    let tree = Tree.random ids in
    assert_equal_int  1 (IDSet.cardinal   (Tree.get_handles tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int  0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Disjoint Tree property cardinalites" >::
  (fun () ->
    let ids = 0 -- 9 in (* 10 leaves *)
    let tree = Tree.random ids |> Tree.disjoint in
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_handles tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int  0 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Iterative Tree Construction" >::
  (fun () ->
    let tree = Tree.create (0 -- 9) in (* 10 leaves *)
    let tree = match IDSet.elements (Tree.get_singles tree) with
      | x1::x2::xs ->
        let tree = fst @@ Tree.join (`Single x1) (`Single x2) tree in
        List.fold_left
          (fun tree n ->
            let e = Tree.random_edge tree in
            fst @@ Tree.join (`Single n) (`Edge e) tree)
          tree
          xs
      | xs ->
        assert_equal_int 10 (List.length xs); (* fails always *)
        tree
    in
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int  0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

    "Partition Tree Edges" >::
    (fun () ->
      let tree = Tree.random (0 -- 9) in
      EdgeSet.iter
        (fun ((a,b) as e) ->
          let errmsg = Printf.sprintf "Non-Disjoint Partition from %d and %d)" a b in
          let l,r,d = Tree.partition_edge e tree in
          let l_minus_r = IDSet.inter l r and r_minus_l = IDSet.inter r l in
          let () = assert_bool errmsg d in
          let () = errmsg @? (IDSet.is_empty l_minus_r) in
          let () = errmsg @? (IDSet.is_empty r_minus_l) in
          let () = assert_equal_int 10 (IDSet.cardinal (IDSet.union l r)) in
          ())
        (Tree.get_all_edges tree);
      ());

    "Handle Of Functions" >::
    (fun () ->
      let tree = Tree.random (0 -- 9) in
      let handles =
        List.map (fun (x,_) -> Tree.handle_of x tree) @@ IDMap.bindings tree.Tree.nodes 
      in
      match handles with
        | x::xs -> List.iter (fun y -> assert_equal_int x y) xs
        | _ -> assert false);

    "Break and Join functions" >::
    (fun () -> ());

    "Reroot/Handle Functions" >::
    (fun () -> 
      let t1 = Tree.random (0 -- 9) in
      let x = Tree.random_node t1 in
      let t2,p1 = Tree.reroot x t1 in
      assert_equal_int x (Tree.handle_of 0 t2));

    "Compare Function" >::
    (fun () ->
      let r_state = Random.get_state () in
      let tree1 = Tree.random (0 -- 9) in
      let () = Random.set_state r_state in
      let tree2 = Tree.random (0 -- 9) in
      "Compare Equal Trees" @? (0 = (Tree.compare tree1 tree2)));

    "Path Function" >::
    (fun () ->
      let tree = Tree.random (0 -- 9) in
      let rec test_once x =
        let x = Tree.random_node tree
        and y = Tree.random_node tree in
        let p1 = Tree.path_of x y tree
        and p2 = Tree.path_of y x tree in
        assert_equal p1 (List.rev p2);
        if x = 0 then test_once (x-1) else ()
      in
      test_once 10);

    "Num Trees Function" >::
    (fun () ->
      let cmp = Num.eq_num in
      let  input = List.map Num.num_of_int [0;1;2;3;4; 5; 6;   7;    8;     9;     10;      11]
      and output = List.map Num.num_of_int [0;1;1;1;3;15;105;945;10395;135135;2027025;34459425] in
      List.iter2
        (fun i o -> assert_equal_num ~cmp o (Tree.num_trees i))
        input output);

    "Partition Edge Function" >::
    (fun () ->
      let tree = Tree.random (0 -- 9) in
      EdgeSet.iter
        (fun edge ->
          let a,b,x = Tree.partition_edge edge tree in
          assert_equal true x;
          assert_equal IDSet.empty (IDSet.inter a b))
        (Tree.get_all_edges tree));
]

