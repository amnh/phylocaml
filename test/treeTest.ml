open OUnit
open TestInternal
open Topology

let verify_delta delta t =
  let assert_edge should_exist x y =
    assert_bool
      (Printf.sprintf "Edge should%s exist (%d,%d)" (if should_exist then "" else " not") x y)
      ((not should_exist) <> (Tree.is_edge x y t))
  and assert_node should_exist x =
    assert_bool
      (Printf.sprintf "Node should%s exist %d" (if should_exist then "" else " not") x)
      ((not should_exist) <> (Tree.is_node x t))
  in
  List.iter (fun (x,y) -> assert_edge true x y) delta.created.d_edges;
  List.iter (fun (x,y) -> assert_edge false x y) delta.removed.d_edges;
  List.iter (fun x -> assert_node true x) delta.created.d_nodes;
  List.iter (fun x -> assert_node false x) delta.removed.d_nodes;
  ()

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
            let x,d = Tree.join (`Single n) (`Edge e) tree in
            let () = verify_delta d x in
            x)
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
          let () = assert_bool errmsg d in
          let () = assert_equal_ids IDSet.empty (IDSet.inter l r) in
          let () = assert_equal_ids l (IDSet.diff l r) in
          let () = assert_equal_ids r (IDSet.diff r l) in
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

    "Break/Disjoint Functions" >::
    (fun () ->
      let t1 = Tree.random (0 -- 9) in
      let rec break_all tree =
        let all = Tree.get_all_edges tree in
        if EdgeSet.is_empty all
          then tree
          else Tree.break (EdgeSet.choose all) tree |> fst |> break_all
      in
      "Compare fully broken and disjoint tree"
        @? (0 == (Tree.compare (Tree.disjoint t1) (break_all t1))));

    "Break and Join Function Consistency and Delta" >::
    (fun () ->
      let single_jxn () = function
        | `Edge (a,b) -> Printf.sprintf "(%d,%d)" a b
        | `Single x   -> Printf.sprintf "(%d)" x
      in
      let msg_of_jxn a b =
        Printf.sprintf "Joining/Breaking %a and %a failed" single_jxn a single_jxn b
      in
      let t1 = Tree.random (0 -- 9) in
      let break_and_join tree edge =
        let t,d = Tree.break edge tree in
        let () = verify_delta d t in
        let x,y = Tree.jxn_of_delta d in
        let t,d = Tree.join x y t in
        let () = verify_delta d t in
        let msg = msg_of_jxn x y in
        assert_equal_tree ~msg tree t
      in
      EdgeSet.iter (break_and_join t1) (Tree.get_all_edges t1));


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
]

