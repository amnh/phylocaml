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
    let sort f xs =
      List.sort (fun x y -> Pervasives.compare (f x) (f y)) xs
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
  | Single _   -> true
  | Leaf _     -> true
  | Interior _ -> false

and is_single = function
  | Single _   -> true
  | Leaf _     -> false
  | Interior _ -> false

let get_leaves t : IDSet.t =
  let leaves,_ = IDMap.partition (fun _ v -> (is_leaf v) || (is_single v)) t.nodes in
  IDMap.fold (fun k _ t -> IDSet.add k t) leaves IDSet.empty

let get_edge a b t =
  if EdgeSet.mem (a,b) t.edges
    then (a,b)
    else raise Not_found

let get_node a t =
  IDMap.find a t.nodes

let get_neighbors x t = match get_node x t with
  | Single x
  | Leaf (_,x) -> [x]
  | Interior (_,x,y,z) -> [x;y;z]

let create ids = (* TODO deal with handles *)
  let nodes = List.fold_left (fun acc i -> IDMap.add i (Single i) acc) IDMap.empty ids in
  {empty with
    edges = EdgeSet.empty;
    nodes = nodes; }


let disjoint t = (* TODO deal with handles *)
  let nodes =
    IDSet.fold (fun i acc -> IDMap.add i (Single i) acc) (get_leaves t) IDMap.empty
  in
  {t with
    edges = EdgeSet.empty;
    nodes = nodes; }

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

let pre_order_nodes f t acc h =
  let rec pre_order prev n acc =
    match get_node n t with
    | Single _   -> assert false
    | Leaf (x,y) -> assert (prev = y); f (Some prev) x acc
    | Interior (a,x,y,z) ->
      let c,d = get_other_two prev x y z in
      f (Some prev) a acc
        |> pre_order a c
        |> pre_order a d
  in
  match get_node h t with
  | Single x   -> f None x acc
  | Leaf (x,y) -> pre_order x y (f None x acc)
  | Interior (a,x,y,z) ->
    f None a acc
      |> pre_order a x
      |> pre_order a y
      |> pre_order a z

let pre_order_edges _ _ _ _ = failwith "TODO"

let pre_order_edges_root _ _ _ _ = failwith "TODO"

let post_order_edges f g (a, b) bt accum =
  let rec processor prev curr accum =
    match get_node curr bt with
    | Leaf (nd, nbr) ->
      assert( prev = nbr );
      assert( curr = nd );
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

let partition_edge _ _ = failwith "TODO"

let handle_of _ _ = failwith "TODO"

let path_of _ _ _ = failwith "TODO"

let disjoint_edge _ _ = true


 (* TODO: deal with DELTA and HANDLES *)
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
  | Single _, _
  | _, Single _ -> assert false
  (* a -- x ---> a + x *)
  | Leaf (a,b), Leaf (x,y) ->
    assert( (a = y) && (b = x));
    let nodes =
      t.nodes
        |> IDMap.add a (Single a)
        |> IDMap.add x (Single x)
    and edges = EdgeSet.remove (a,x) t.edges in
    {t with nodes; edges; }, ()
  (*       b           b
   *      /            |
   * x---a   --->  x + |
   *      \            |
   *       c           c *)
  | Leaf (x,y), Interior(a,b,c,d)
  | Interior(a,b,c,d), Leaf (x,y) ->
    assert( y = a );
    let b,c = get_other_two x b c d in
    let t   = clean_up_nodes b c x a t in
    {t with
      nodes = IDMap.add x (Single x) t.nodes; }, ()
  (* b       x     b   x
   *  \     /      |   |
   *   a---w  ---> | + |
   *  /     \      |   |
   * c       y     c   y    *)
  | Interior (a,b,c,d), Interior (w,x,y,z) ->
    let b,c = get_other_two w b c d
    and x,y = get_other_two a x y z in
    let t = clean_up_nodes b c w a t in
    let t = clean_up_nodes x y a w t in
    t,()
   

 (* TODO: deal with DELTA and HANDLES *)
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
      EdgeSet.add (x,y) t.edges in
    {t with edges; nodes;}, ()
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
    in
    {t with edges; nodes;}, ()
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
    let n1 = Interior (n_id1, n_id2, y, z)
    and n2 = Interior (n_id2, n_id1, w, x) in
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
    in
    {t with edges; nodes; }, ()

let reroot _ _ = failwith "TODO"

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
