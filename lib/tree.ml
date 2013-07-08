open Phylocaml_pervasives

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

let next_code t : int * t = match t.avail_codes with
  | y,x::xs -> x, {t with avail_codes =y,xs; }
  | y,[]    -> y, {t with avail_codes =y+1,[]; }

let add_code i t : t = match t.avail_codes with
  | y,x when i = (y-1) -> {t with avail_codes = y-1,x; }
  | y,x -> {t with avail_codes = y,i::x; }

let is_edge x y t = EdgeSet.mem (x,y) t.edges

let is_node x t = IDMap.mem x t.nodes

let get_other_two p a b c =
  if p = a then b,c
  else if p = b then a,c
  else
    let () = assert (p = c) in
    a,b

let remove_replace node o_id n_id = match node with
  | Leaf (a,b)         when b = o_id -> Leaf (a,n_id)
  | Interior (a,b,c,d) when b = o_id -> Interior (a,n_id,c,d)
  | Interior (a,b,c,d) when c = o_id -> Interior (a,b,n_id,d)
  | Interior (a,b,c,d) when d = o_id -> Interior (a,b,c,n_id)
  | Interior _ | Single _ | Leaf _ -> assert false

let is_leaf = function
  | Single _   -> true (* TODO is there utility in including this? *)
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

let compare a b = failwith "TODO"

let holes_and_max ids =
  let rec holesmax holes max xxs = match xxs with
    | [] -> max,holes
    | x::xs when x = max -> holesmax holes (max+1) xs
    | x::xs -> holesmax (max::holes) (max+1) xxs
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
  and handles = leaves
  and avail_codes = holes_and_max leaves in
  {t with
    edges; nodes; handles; avail_codes; }

let random_elt_nodemap (type t) (m: t IDMap.t) : IDMap.key * t =
  let n = IDMap.cardinal m in
  let i = Random.int n in
  let module M = struct exception E of IDMap.key * t end in
  try 
    IDMap.fold
      (fun k v j -> if i = j then raise (M.E (k,v)) else succ j) m 0
        |> ignore;
    assert false
  with M.E (k,v) -> k,v

let random_elt_nodeset (s: IDSet.t) =
  let n = IDSet.cardinal s in
  let i = Random.int n in
  let module M = struct exception E of IDSet.elt end in
  try 
    IDSet.fold
      (fun k j -> if i = j then raise (M.E k) else succ j) s 0
        |> ignore;
    assert false
  with M.E k -> k

let random_elt_edgeset (s: EdgeSet.t) : edge =
  let n = EdgeSet.cardinal s in
  let i = Random.int n in
  let module M = struct exception E of EdgeSet.elt end in
  try
    EdgeSet.fold (fun k j -> if i = j then raise (M.E k) else succ j) s 0
      |> ignore;
    assert false
  with M.E k -> k

let random_edge t =
  random_elt_edgeset t.edges

let random_node t =
  fst (random_elt_nodemap t.nodes)

let random_leaf t =
  get_leaves t |> random_elt_nodeset

let random_single t =
  let single,_ = IDMap.partition (fun _ v -> is_single v) t.nodes in
  fst (random_elt_nodemap single)

let is_leaf x t =
  is_leaf (IDMap.find x t.nodes)

let is_single x t =
  is_single (IDMap.find x t.nodes)

let pre_order_nodes f h t acc =
  let rec processor prev curr acc =
    match get_node curr with
    | Single x, _ -> f prev curr acc
    | Leaf (_,p), None -> processor (Some curr) p (f prev curr acc)
    | Leaf (_,p), Some x -> assert (x=p); f prev curr acc
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
    | Leaf (a,b) -> accum
  in
  f e acc
    |> each_edge a b acc
    |> each_edge b a acc

let pre_order_edges_root _ _ _ _ = failwith "TODO"

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

let get_edges h t = match get_node h t with
  | Single _ -> EdgeSet.empty
  | Leaf (a,b)
  | Interior (a,b,_,_) ->
    pre_order_edges_root
      (fun e acc -> EdgeSet.add e acc)
      (fun e acc -> EdgeSet.add e acc)
      (a,b)
      t
      EdgeSet.empty

let partition_edge edge t =
  let aset,bset =
    post_order_edges
      (fun _ x set -> IDSet.add x set)
      (fun _ x s1 s2 -> IDSet.union (IDSet.add x s1) s2)
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
      | Single _ -> assert false (* should be a handle *)
      | Leaf (_,b) -> handle_of b t
      | Interior (_,a,b,c) ->
        let a,b = get_other_two n a b c in
        begin match handle_of n a t, handle_of n a t with
        | (Some _) as a, None
        | None, (Some _) as a -> a
        | None, None -> None
        | _ , _ -> assert false (* only one handle valid *)
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
    begin match handle_of a t, handle_of b t, handle_of c t with
    | Some x, None, None
    | None, Some x, None
    | None, None, Some x -> x
    | _ , _, _ -> assert false
    end

let path_of _ _ _ = failwith "TODO"

let disjoint_edge _ _ = true


(* TODO: deal with DELTA *)
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
    assert( (a = y) && (b = x));
    let nodes =
      t.nodes
        |> IDMap.add a (Single a)
        |> IDMap.add x (Single x)
    and edges = EdgeSet.remove (a,x) t.edges
    and handles =
      if a = (handle_of a t)
        then HandleSet.add x t.handles
        else HandleSet.add a t.handles
    in
    {t with nodes; edges; handles;}, ()
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
        |> HandleSet.add b (* or c *)
    in
    {t with nodes; handles; }, () 
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
        |> HandleSet.add b (* or c *)
        |> HandleSet.add x (* or y *)
    in
    {t with handles; },()
   

(* TODO: deal with DELTA *)
let join j1 j2 t = match j1, j2 with
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
    {t with edges; nodes; handles;}, ()
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
    {t with edges; nodes; handles;}, ()
  (*  w   y    w       y
   *  |   |     \     /
   *  | + | ---> a---b
   *  |   |     /     \
   *  x   z    x       z *)
  | `Edge (w,x), `Edge (y,z) ->
    assert( is_edge w x t );
    assert( is_edge y z t );
    let n_id1,t = next_code t in
    let n_id2,t = next_code t in
    let nodes =
      t.nodes
        |> IDMap.add w (remove_replace (get_node w t) x n_id1)
        |> IDMap.add x (remove_replace (get_node x t) w n_id1)
        |> IDMap.add y (remove_replace (get_node y t) z n_id2)
        |> IDMap.add z (remove_replace (get_node z t) y n_id2)
    and edges =
      t.edges
        |> EdgeSet.remove (w,x)
        |> EdgeSet.remove (y,z)
        |> EdgeSet.add (n_id1, w)
        |> EdgeSet.add (n_id1, x)
        |> EdgeSet.add (n_id2, y)
        |> EdgeSet.add (n_id2, z)
        |> EdgeSet.add (n_id1, n_id2)
    and handles =
      let x = handle_of x t in
      HandleSet.remove x t.handles
    in
    {t with edges; nodes; handles; }, ()

let move_handle n t =
  let h = handle_of n t in
  let p = path_of t h n in
  let handles =
    t.handles
      |> HandleSet.remove h
      |> HandleSet.add n
  in
  {t with handles;},p

let reroot _ t = t

let random lst =
  let add_node t x =
    let (a,b) = random_edge t in
    fst (join (`Single x) (`Edge (a,b)) t)
  in
  List.fold_left add_node empty lst


type 'a fuse_location = id * t
type 'a fuse_locations = 'a fuse_location list

let fuse_locations _ _ = failwith "TODO"
let fuse_all_locations _ = failwith "TODO"
let fuse _ _ = failwith "TODO"

let to_string _ = failwith "TODO"
let print _ = failwith "TODO"

let of_parsed _ = failwith "TODO"
let to_parsed _ = failwith "TODO"
