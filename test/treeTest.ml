open OUnit2
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

let minimum_leaf_nodes =
  Conf.make_int "min_leaf_nodes" 10 "Minimum leaf nodes for tree tests"
            
let maximum_leaf_nodes =
  Conf.make_int "max_leaf_nodes" 50 "Maximum leaf nodes for tree tests"

let create_random_tree ctxt =
  let min_leaf = minimum_leaf_nodes ctxt
  and max_leaf = maximum_leaf_nodes ctxt in
  let leaves = min_leaf + (Random.int (max_leaf - min_leaf)) in
  Tree.random (0 -- leaves)

let tests = "Tree" >:::
[
  "Empty Tree property cardinalities" >::
  (fun _ctxt ->
    let tree = Tree.empty in
    assert_equal_int 0 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int 0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int 0 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());
  
  "Tree property cardinalities" >::
  (fun _ctxt ->
    let tree = Tree.random (0 -- 9) in
    assert_equal_int  1 (IDSet.cardinal   (Tree.get_handles tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int  0 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int 17 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Disjoint Tree property cardinalites" >::
  (fun _ctxt ->
    let tree = Tree.random (0 -- 9) |> Tree.disjoint in
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_handles tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_leaves tree));
    assert_equal_int 10 (IDSet.cardinal   (Tree.get_singles tree));
    assert_equal_int  0 (EdgeSet.cardinal (Tree.get_all_edges tree));
    ());

  "Iterative Tree Construction" >::
  (fun _ctxt ->
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
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    EdgeSet.iter
      (fun ((a,b) as e) ->
        let errmsg = Printf.sprintf "Non-Disjoint Partition from %d and %d)" a b in
        let l,r,d = Tree.partition_edge e tree in
        let () = assert_bool errmsg d in
        let () = assert_equal_ids IDSet.empty (IDSet.inter l r) in
        let () = assert_equal_ids l (IDSet.diff l r) in
        let () = assert_equal_ids r (IDSet.diff r l) in
        let () = assert_equal_ids (Tree.get_leaves tree) (IDSet.union l r) in
        ())
      (Tree.get_all_edges tree);
    ());

  "Handle Of Functions" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let handles =
      List.map (fun (x,_) -> Tree.handle_of x tree) @@ IDMap.bindings tree.Tree.nodes 
    in
    match handles with
      | x::xs -> List.iter (fun y -> assert_equal_int x y) xs
      | _ -> assert false);

  "Break/Disjoint Functions" >::
  (fun ctxt ->
    let t1 = create_random_tree ctxt in
    let rec break_all tree =
      let all = Tree.get_all_edges tree in
      if EdgeSet.is_empty all
        then tree
        else Tree.break (EdgeSet.choose all) tree |> fst |> break_all
    in
    assert_bool 
      "Compare fully broken and disjoint tree"
      (0 == (Tree.compare (Tree.disjoint t1) (break_all t1))));

  "In Subtree" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let each_edge tree ((a,b) as e) =
      let l,r,_ = Tree.partition_edge e tree in
      let msg b p n i =
        let b = if not b then " not" else "" in
        Printf.sprintf "Subtree of %d->%d does%s contain %d in subtree" p n b i
      in
      IDSet.iter (fun n -> assert_bool (msg false b a n) (not (Tree.in_subtree n b a tree))) r;
      IDSet.iter (fun n -> assert_bool (msg true  b a n) (Tree.in_subtree n b a tree)) l;
      IDSet.iter (fun n -> assert_bool (msg false a b n) (not (Tree.in_subtree n a b tree))) l;
      IDSet.iter (fun n -> assert_bool (msg true  a b n) (Tree.in_subtree n a b tree)) r;
    in
    EdgeSet.iter (each_edge tree) (Tree.get_all_edges tree));

  "Break and Join Function Consistency and Delta" >::
  (fun ctxt ->
    let rec single_jxn () = function
      | `Edge (a,b) -> Printf.sprintf "(%d,%d)" a b
      | `Single x   -> Printf.sprintf "(%d)" x
    and msg_of_jxn a b =
      Printf.sprintf "Joining/Breaking %a and %a failed" single_jxn a single_jxn b
    in
    let t1 = create_random_tree ctxt in
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
  (fun ctxt -> 
    let t1 = create_random_tree ctxt in
    let x = Tree.random_node t1 in
    let t2,_ = Tree.reroot x t1 in
    assert_equal_int x (Tree.handle_of 0 t2));

  "Path Function" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let rec test_once n =
      let x = Tree.random_node tree
      and y = Tree.random_node tree in
      let p1 = Tree.path_of x y tree
      and p2 = Tree.path_of y x tree in
      assert_equal p1 (List.rev p2);
      if n = 0 then () else test_once (n-1)
    in
    test_once 10);

  "Neighborhood Function" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let x = Tree.random_node tree in
    let edge = match Tree.get_node x tree with
      | Tree.Leaf (x,y)
      | Tree.Interior (x,y,_,_)-> (x,y)
      | Tree.Single _ -> assert false
    in
    let l0 = Tree.get_neighborhood 0 edge tree in
    assert_equal_int 1 (Topology.EdgeSet.cardinal l0);
    let l1 = Topology.EdgeSet.cardinal @@ Tree.get_edges x tree in
    let l2 = Topology.EdgeSet.cardinal @@ Tree.get_neighborhood l1 edge tree in
    assert_equal_int l1 l2);

  "Num Edges matches Built Tree" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let l0 = tree |> Tree.get_leaves |> Topology.IDSet.cardinal
                  |> Num.num_of_int  |> Tree.num_edges |> Num.int_of_num
    in
    let l1 = Topology.EdgeSet.cardinal @@ Tree.get_edges 0 tree in
    assert_equal_int l0 l1);

  "Num Nodes matches Built Tree" >::
  (fun ctxt ->
    let tree = create_random_tree ctxt in
    let l0 = tree |> Tree.get_leaves |> Topology.IDSet.cardinal
                  |> Num.num_of_int  |> Tree.num_nodes |> Num.int_of_num
    in
    let l1 = Topology.IDMap.cardinal tree.Tree.nodes in
    assert_equal_int l0 l1);

  "Num Unrooted Trees Function" >::
  (fun _ctxt ->
    let cmp = Num.eq_num in
    let  input = List.map Num.num_of_int [0;1;2;3;4; 5; 6;   7;    8;     9; 10;      11;           12]
    and output = List.map Num.num_of_int [0;1;1;1;3;15;105;945;10395;135135;2027025;34459425;654729075] in
    List.iter2
      (fun i o -> assert_equal_num ~cmp o (Tree.num_unrooted_trees i))
      input output);

  "Num Rooted Trees Function" >::
  (fun _ctxt ->
    let cmp = Num.eq_num in
    let  input = List.map Num.num_of_int [0;1;2;3; 4;  5;  6;    7;     8;      9;      10]
    and output = List.map Num.num_of_int [0;1;1;3;15;105;945;10395;135135;2027025;34459425] in
    List.iter2
      (fun i o -> assert_equal_num ~cmp o (Tree.num_rooted_trees i))
      input output);

  "Num Nodes with N leaves" >::
  (fun _ctxt ->
    let cmp = Num.eq_num in
    let  input = List.map Num.num_of_int [0;1;2;3;4;5; 6; 7; 8; 9;10]
    and output = List.map Num.num_of_int [0;1;2;4;6;8;10;12;14;16;18] in
    List.iter2
      (fun i o -> assert_equal_num ~cmp o (Tree.num_nodes i))
      input output);

  "Num Edges with N leaves" >::
  (fun _ctxt ->
    let cmp = Num.eq_num in
    let  input = List.map Num.num_of_int [0;1;2;3;4;5;6; 7; 8; 9;10]
    and output = List.map Num.num_of_int [0;0;1;3;5;7;9;11;13;15;17] in
    List.iter2
      (fun i o -> assert_equal_num ~cmp o (Tree.num_edges i))
      input output);
]

