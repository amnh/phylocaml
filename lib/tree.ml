open Internal

open Topology

module NodeComparator : Topology.NodeComparator =
  struct
    type t = int

    let create x = x

    let ancestor_1 _ x = x
    let ancestor_2 _ x y = min x y
    let ancestor_n _ xs = List.fold_left min max_int xs

    let order f x y = if (f x) > (f y) then y,x else x,y
    let sort f xs = List.sort (fun x y -> Pervasives.compare (f x) (f y)) xs
  end

type node =
  | Leaf of id * id
  | Interior of id * id * id * id
  | Single of id

type t =
  { name  : string option;
    nodes : node IDMap.t;
    edges : EdgeSet.t;
    handles : HandleSet.t;
    avail_codes : int * int list;
  }

exception InvalidNodeID of id

exception InvalidEdge of edge

exception InvalidHandle of handle

let empty =
  { name = None;
    nodes = IDMap.empty;
    edges = EdgeSet.empty;
    handles = HandleSet.empty;
    avail_codes = 0,[];
  }


let debug_print t chan =
  Printf.fprintf chan "Edges : ";
  EdgeSet.iter (fun (a,b) -> Printf.fprintf chan "(%d,%d) " a b) t.edges;
  print_newline ();
  Printf.fprintf chan "Nodes : ";
  IDMap.iter
    (fun _ -> function
      | Leaf (a,b) -> Printf.fprintf chan "L(%d,%d) " a b
      | Interior (a,b,c,d) -> Printf.fprintf chan "N(%d,%d,%d,%d) " a b c d
      | Single a -> Printf.fprintf chan "S(%d) " a)
    t.nodes;
  print_newline ();
  ()

let next_code t : int * t = match t.avail_codes with
  | y,x::xs -> x, {t with avail_codes =y,xs; }
  | y,[]    -> y, {t with avail_codes =y+1,[]; }

let add_code i t : t = match t.avail_codes with
  | y,x when i = (y-1) -> {t with avail_codes = y-1,x; }
  | y,x -> {t with avail_codes = y,i::x; }

let is_edge x y t = EdgeSet.mem (x,y) t.edges

let is_node x t = IDMap.mem x t.nodes

let is_handle x t = HandleSet.mem x t.handles

let set_name x t = {t with name = Some x}

let get_name t = t.name

let get_other_two p a b c =
       if p = a then b,c
  else if p = b then a,c
  else if p = c then a,b
                else assert false

let remove_replace node o_id n_id = match node with
  | Leaf (a,b)         when b = o_id -> Leaf (a,n_id)
  | Interior (a,b,c,d) when b = o_id -> Interior (a,n_id,c,d)
  | Interior (a,b,c,d) when c = o_id -> Interior (a,b,n_id,d)
  | Interior (a,b,c,d) when d = o_id -> Interior (a,b,c,n_id)
  | Interior _ | Leaf _ | Single _ -> assert false

let is_leaf = function
  | Single _   -> true
  | Leaf _     -> true
  | Interior _ -> false

and is_single = function
  | Single _   -> true
  | Leaf _     -> false
  | Interior _ -> false

let get_leaves t : IDSet.t =
  let leaves,_ = IDMap.partition (fun _ v -> (is_leaf v)) t.nodes in
  IDMap.fold (fun k _ t -> IDSet.add k t) leaves IDSet.empty

let get_singles t : IDSet.t =
  let leaves,_ = IDMap.partition (fun _ v -> (is_single v)) t.nodes in
  IDMap.fold (fun k _ t -> IDSet.add k t) leaves IDSet.empty

let get_edge a b t =
  if EdgeSet.mem (a,b) t.edges
    then (a,b)
    else raise Not_found

let get_node a t =
  IDMap.find a t.nodes

let get_neighbors x t = match get_node x t with
  | Single _ -> []
  | Leaf (_,x) -> [x]
  | Interior (_,x,y,z) -> [x;y;z]

let compare _ _ = failwith "TODO"

let holes_and_max ids : int * int list =
  let rec holesmax holes max xxs = match xxs with
    | [] -> max,holes
    | x::xs when x = max -> holesmax holes (max+1) xs
    | _ -> holesmax (max::holes) (max+1) xxs
  in
  holesmax [] 0 (List.sort Pervasives.compare ids)

let create ids =
  let nodes,handles =
    List.fold_left
      (fun (map,set) i -> IDMap.add i (Single i) map, HandleSet.add i set)
      (IDMap.empty,HandleSet.empty)
      (ids)
  and edges = EdgeSet.empty
  and avail_codes = holes_and_max ids in
  {empty with
    edges; nodes; handles; avail_codes; }

let disjoint t =
  let leaves = get_leaves t in
  let nodes =
    IDSet.fold (fun i acc -> IDMap.add i (Single i) acc) leaves IDMap.empty
  and edges = EdgeSet.empty
  and handles = 
    IDSet.fold (fun i acc -> HandleSet.add i acc) leaves HandleSet.empty
  and avail_codes = holes_and_max (IDSet.elements leaves) in
  {t with
    edges; nodes; handles; avail_codes; }

let random_edge t = Topology.random_edgeset t.edges

let random_node t = fst @@ Topology.random_nodemap t.nodes

let random_leaf t = Topology.random_nodeset @@ get_leaves t

let random_single t = Topology.random_nodeset @@ get_singles t

let is_leaf x t = is_leaf @@ IDMap.find x t.nodes

let is_single x t = is_single @@ IDMap.find x t.nodes

let pre_order_nodes f h t acc =
  let rec processor prev curr acc =
    match get_node curr t, prev with
    | Single x, _ ->
      assert(x=curr);
      f prev curr acc
    | Leaf (_,p), None ->
      processor (Some curr) p (f prev curr acc)
    | Leaf (_,p), Some x ->
      assert(x=p);
      f prev curr acc
    | Interior(_,a,b,c),None ->
      f prev curr acc
        |> processor (Some curr) a
        |> processor (Some curr) b
        |> processor (Some curr) c
    | Interior(_,a,b,c),Some x ->
      let a,b = get_other_two x a b c in
      f prev curr acc
        |> processor (Some curr) a
        |> processor (Some curr) b
  in
  processor None h acc

let pre_order_edges f ((a,b) as e) t acc =
  let rec each_edge prev curr accum =
    match get_node curr t with
    | Single _ -> assert false
    | Interior (_,a,b,c) ->
      let a,b = get_other_two prev a b c in
      f (curr,a) accum
        |> each_edge curr a
        |> f (curr,b)
        |> each_edge curr b
    | Leaf _ -> accum
  in
  f e acc
    |> each_edge a b
    |> each_edge b a

let post_order_edges f g (a, b) bt accum =
  let rec processor prev curr accum =
    match get_node curr bt with
    | Leaf (nd, nbr) ->
      assert(prev = nbr);
      assert(curr = nd);
      f prev curr accum
    | Interior (nd, nbr1, nbr2, nbr3) ->
      let a, b = get_other_two prev nbr1 nbr2 nbr3 in
      let aacc = processor nd a accum
      and bacc = processor nd b accum in
      g prev curr aacc bacc
    | Single _ -> accum
  in
  let a = processor b a accum
  and b = processor a b accum in
  a, b

let get_edges _ _ = failwith "TODO"

let get_all_edges t = t.edges

let partition_edge edge t =
  let aset,bset =
    post_order_edges
      (fun _ x set -> IDSet.add x set)
      (fun _ _ s1 s2 -> IDSet.union s1 s2)
      edge
      t
      IDSet.empty
  in
  aset,bset,true

(* we can do better if we assume the second element in the interior tuple is
 * pointing to the parent/handle. should we? this should be a traversal call. *)
let handle_of n t =
  let rec handle_of p n t =
    if is_handle n t then
      Some n
    else
      match get_node n t with
        | Single _ -> assert false (* should be a handle already *)
        | Leaf (_,b) -> handle_of n b t
        | Interior (_,a,b,c) ->
          let a,b = get_other_two p a b c in
          begin match handle_of n a t, handle_of n b t with
            | ((Some _) as a, None)
            | None, ((Some _) as a) -> a
            | None, None -> None
            | (Some _),(Some _) -> assert false (* only one handle valid *)
          end
  in
  match get_node n t with
  | Single x -> assert( is_handle x t ); n
  | Leaf (_,b) ->
    begin match handle_of n b t with
      | Some x -> x
      | None   -> assert false
    end
  | Interior (_,a,b,c) ->
    begin match handle_of n a t, handle_of n b t, handle_of n c t with
      | Some x, None, None
      | None, Some x, None
      | None, None, Some x -> x
      | _ , _, _ -> assert false
    end

let path_of a b t =
  let rec build_path acc a = match get_node a t with
    | Leaf     (_,x)     when x = b -> x :: acc
    | Interior (_,x,_,_) when x = b -> x :: acc
    | Interior (_,_,x,_) when x = b -> x :: acc
    | Interior (_,_,_,x) when x = b -> x :: acc
    | Interior (_,x,y,z) ->
      begin
        try build_path (a::acc) x with Not_found ->
        try build_path (a::acc) y with Not_found ->
            build_path (a::acc) z
      end
    | Leaf _   -> raise Not_found
    | Single _ -> assert false
  in
  let rev_path = match get_node a t with
    | Leaf (_,b) -> build_path [a] b
    | Single _   -> raise Not_found
    | Interior (_,b,c,d) ->
      begin
        try build_path [a] b with Not_found ->
        try build_path [a] c with Not_found ->
            build_path [a] d
      end
  in
  List.rev rev_path


let traverse_path _ _ _ = failwith "TODO"

let disjoint_edge _ _ = true


let break (x,y) t =
  (* Fix a and b with x; leave c up to call. *)
  let clean_up_nodes a b c x t =
    let nodes =
      t.nodes
        |> IDMap.add a (remove_replace (get_node a t) x b)
        |> IDMap.add b (remove_replace (get_node b t) x a)
        |> IDMap.remove x
    and edges =
      t.edges
        |> EdgeSet.remove (x,a)
        |> EdgeSet.remove (x,b)
        |> EdgeSet.remove (x,c)
        |> EdgeSet.add (a,b)
    in
    {t with nodes; edges; }
  in
  assert( is_edge x y t );
  match get_node x t, get_node y t with
  | (Single _, _ | _, Single _) -> assert false
  (* a -- x ---> a + x *)
  | Leaf (a,b), Leaf (x,y) ->
    assert((a = y) && (b = x));
    let nodes =
      t.nodes
        |> IDMap.add a (Single a)
        |> IDMap.add x (Single x)
    and edges = EdgeSet.remove (a,x) t.edges
    and handles,h =
      if a = (handle_of a t)
        then HandleSet.add x t.handles,x
        else HandleSet.add a t.handles,a
    in
    let delta =
      { created = {empty_side with d_handles = [h]; };
        removed = {empty_side with d_edges = [(a,x);];} }
    in
    {t with nodes; edges; handles;},delta
  (*       b           b
   *      /            |
   * x---a   --->  x + |
   *      \            |
   *       c           c *)
  | Leaf (x,y), Interior(a,b,c,d)
  | Interior(a,b,c,d), Leaf (x,y) ->
    assert( y = a );
    let b,c = get_other_two x b c d in
    let h = handle_of a t in
    let t = clean_up_nodes b c x a t in
    let nodes = IDMap.add x (Single x) t.nodes
    and handles =
      t.handles
        |> HandleSet.remove h
        |> HandleSet.add x
        |> HandleSet.add b (* or c, choice is arbitrary *)
    in
    let t = add_code a {t with nodes; handles;} in
    let delta =
      let add_hs = if h = x then [b] else if h = b then [x] else [x;b]
      and rem_hs = if h = x || h = b then [] else [h] in
      { created = {d_nodes = []; d_edges = [(b,c)]; d_handles = add_hs;};
        removed = {d_nodes = [a];d_edges = [(a,x);(a,b);(a,c)]; d_handles = rem_hs;}; }
    in
    t,delta
  (* b       x     b   x
   *  \     /      |   |
   *   a---w  ---> | + |
   *  /     \      |   |
   * c       y     c   y    *)
  | Interior (a,b,c,d), Interior (w,x,y,z) ->
    let b,c = get_other_two w b c d
    and x,y = get_other_two a x y z in
    let h = handle_of a t in
    let t = clean_up_nodes b c w a t in
    let t = clean_up_nodes x y a w t in
    let handles =
      t.handles
        |> HandleSet.remove h
        |> HandleSet.add b (* or c, choice is arbitrary *)
        |> HandleSet.add x (* or y, choice is arbitrary *)
    in
    let t = add_code a @@ add_code w {t with handles; } in
    let delta =
      let add_hs = if h = b then [x] else if h = x then [b] else [x;b]
      and rem_hs = if h = b || h = x then [] else [h] in
      { created = {d_nodes = []; d_edges = [(b,c);(x,y)]; d_handles = add_hs;};
        removed = {d_nodes = [a;w]; d_edges = [(a,b);(a,c);(w,x);(w,y);(a,w)];
                   d_handles = rem_hs;};}
    in
    t,delta
   

let join j1 j2 t =
  match j1, j2 with
  (* x + y ---> x -- y *)
  | `Single x, `Single y ->
    assert( is_single x t );
    assert( is_single y t );
    let nodes =
      t.nodes
        |> IDMap.add x (Leaf (x,y))
        |> IDMap.add y (Leaf (y,x))
    and edges =
      EdgeSet.add (x,y) t.edges
    and handles =
      HandleSet.remove x t.handles
    in
    let delta =
      { created = {empty_side with d_edges = [(x,y)];};
        removed = {empty_side with d_handles = [x];} }
    in
    {t with edges; nodes; handles;},delta
  (*     y            y
   *     |           /
   * x + | ---> x---a
   *     |           \
   *     z            z *)
  | `Single x, `Edge (y,z)
  | `Edge (y,z), `Single x ->
    assert( is_single x t );
    assert( is_edge y z t );
    let n_id,t = next_code t in
    let n = Interior (n_id, x, y, z) in
    let nodes =
      t.nodes
        |> IDMap.add y (remove_replace (get_node y t) z n_id)
        |> IDMap.add z (remove_replace (get_node z t) y n_id)
        |> IDMap.add x (Leaf (x,n_id))
        |> IDMap.add n_id n
    and edges =
      t.edges
        |> EdgeSet.remove (y,z)
        |> EdgeSet.add (n_id,y)
        |> EdgeSet.add (n_id,z)
        |> EdgeSet.add (n_id,x)
    and handles =
      HandleSet.remove x t.handles
    in
    let delta =
      { created = { d_edges = [(n_id,x);(n_id,y);(n_id,z)];
                    d_nodes = [n_id]; d_handles = []; };
        removed = { d_nodes = []; d_edges = [(y,z)]; d_handles = [x]; }; }
    in
    {t with edges; nodes; handles;}, delta
  (*  w   y    w       y
   *  |   |     \     /
   *  | + | ---> a---b
   *  |   |     /     \
   *  x   z    x       z *)
  | `Edge (w,x), `Edge (y,z) ->
    assert( is_edge w x t );
    assert( is_edge y z t );
    let a, t = next_code t in
    let b, t = next_code t in
    let nodes =
      t.nodes
        |> IDMap.add w (remove_replace (get_node w t) x a)
        |> IDMap.add x (remove_replace (get_node x t) w a)
        |> IDMap.add y (remove_replace (get_node y t) z b)
        |> IDMap.add z (remove_replace (get_node z t) y b)
        |> IDMap.add a (Interior (a,x,w,b))
        |> IDMap.add b (Interior (b,y,z,a))
    and edges =
      t.edges
        |> EdgeSet.remove (w,x)
        |> EdgeSet.remove (y,z)
        |> EdgeSet.add (a,w)
        |> EdgeSet.add (a,x)
        |> EdgeSet.add (b,y)
        |> EdgeSet.add (b,z)
        |> EdgeSet.add (a,b)
    and handles =
      let x = handle_of x t in
      HandleSet.remove x t.handles
    in
    let delta =
      { created = { d_edges = [(a,w);(a,x);(a,b);(b,y);(b,z);];
                    d_nodes = [a;b]; d_handles = []; };
        removed = { d_nodes = []; d_edges = [(y,z);(w,x)]; d_handles = [x]; }; }
    in
    {t with edges; nodes; handles; }, delta

let move_handle n t =
  let h = handle_of n t in
  let p = path_of h n t in
  let handles =
    t.handles
      |> HandleSet.remove h
      |> HandleSet.add n
  in
  {t with handles;},p

let reroot x t =
  let handle = handle_of x t in
  let delta  = path_of x handle t in
  t.handles
    |> HandleSet.remove handle
    |> HandleSet.add x
    |> fun x -> {t with handles=x;},delta

let random lst =
  let add_node t x =
    let (a,b) = random_edge t in
    fst @@ join (`Single x) (`Edge (a,b)) t
  in
  let t = create lst in
  match lst with
  | x1::x2::xs -> 
    let tree = fst @@ join (`Single x2) (`Single x1) t in
    List.fold_left add_node tree xs
  | _::[] | [] -> t


(** {2 I/O Functions} *)

(** Define the data on the nodes and leaves of the tree structure. *)
type data =
  [ `BranchLength of float | `Name of string | `Support of float ] list

(** Type for a tree from a parsed source. This is not binary, so it can be used
    for collapsed branches in output, or unresolved topologies in input. *)
type parsed = [`Node of data * parsed list | `Leaf of data ]

(** Generate a tree from a parsed tree; we return a tree, and a table of id's to
    the data for future diagnosis. *)
let of_parsed _ = failwith "TODO"

(** Generate a parsed tree from a tree and functions that generate a branch
    length, name and support values for the edges or sub-tree. The root(s) are
    set to the handle(s) of the tree.
    
    [b ida idb] - Return branch-length of ida and idb.
    [s ida idb] - Return the support of the clade below ida with parent idb.
    [n ida idb] - Return the name of the clade defined at ida with parent idb.
                  Often this will be a single taxa, but clades can be labled for
                  generality. *)
let to_parsed _ _ _ _ = failwith "TODO"

(** Generate a string from the parsed tree; see to_parsed tree for how to
    generate details. *)
let to_string _ = failwith "TODO"


(** {2 Math Functions} *)

let num_edges = function
  | n when n < 0 -> assert false (* raise proper exception *)
  | 0 | 1 -> 0
  | n     -> 2 * n - 3

let num_nodes n = match n with
  | _ when n < 0 -> assert false (* raise proper exception *)
  | 0 | 1 -> n
  | n     -> 2 * n - 2

let num_trees n =
  let d_fact n =
    let rec d_fact final acc n =
      if n = final then acc
      else d_fact final (acc*.n) (n-.2.0)
    in
    if n mod 2 = 0
      then (* d_fact 2.0 1.0 (float_of_int n) *)
           assert false (* since 2n-5 is always odd *)
      else d_fact 1.0 1.0 (float_of_int n)
  in
  match n with
  | _ when n < 0 -> assert false (* raise proper exception *)
  | 0            -> 0.0
  | 1 | 2 | 3    -> 1.0
  | n            -> d_fact (2*n - 5)
