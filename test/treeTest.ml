open OUnit
open Phylocaml
open Internal
open Topology

let tests = "Tree" >:::
[
  "Empty Tree property cardinalities" >::
  (fun () ->
    let tree = Tree.empty in
    assert_equal 0 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal 0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal 0 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Tree property cardinalities" >::
  (fun () ->
    let ids = 0 -- 9 in (* 10 leaves *)
    let tree = Tree.random ids in
    assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal  0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Disjoint Tree property cardinalites" >::
  (fun () ->
    let ids = 0 -- 9 in (* 10 leaves *)
    let tree = Tree.random ids |> Tree.disjoint in
    assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal 10 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal  0 (EdgeSet.cardinal (Tree.get_all_edges tree));
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
        assert_equal 10 (List.length xs); (* fails always *)
        tree
    in
    assert_equal 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal  0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
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
          let () = assert_equal 10 (IDSet.cardinal (IDSet.union l r)) in
          ())
        (Tree.get_all_edges tree);
      ());

    "Break and Join functions" >::
    (fun () -> ());

    "Reroot/Handle Functions" >::
    (fun () -> 
      let t1 = Tree.random (0 -- 9) in
      let x = Tree.random_node t1 in
      let t2,p1 = Tree.reroot x t1 in
      assert_equal x (Tree.handle_of 4 t2));

    "Compare Function" >::
    (fun () -> ());

    "Path Function" >::
    (fun () ->
      let tree = Tree.random (0 -- 9) in
      let x = Tree.random_node tree
      and y = Tree.random_node tree in
      let p1 = Tree.path_of x y tree
      and p2 = Tree.path_of y x tree in
      assert_equal p1 (List.rev p2));

    "Post Order Edge Traversal Function" >::
    (fun () -> ());

    "Pre Order Node Traversal Function" >::
    (fun () -> ());

    "Pre Order Edge Traversal Function" >::
    (fun () -> ());

    "Partition Edge Function" >::
    (fun () -> ());
]

