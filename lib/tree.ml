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
    avail_codes : IDManager.t;
  }

let empty =
  { name = None;
    nodes = IDMap.empty;
    edges = EdgeSet.empty;
    handles = HandleSet.empty;
    avail_codes = IDManager.empty;
  }

let is_edge x y t = EdgeSet.mem (x,y) t.edges

let is_node x t = IDMap.mem x t.nodes

let is_handle x t = HandleSet.mem x t.handles

let set_name x t = {t with name = Some x}

let get_name t = t.name

let get_other_two _n p a b c =
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

let get_handles t = t.handles

let get_edge a b t =
  if EdgeSet.mem (a,b) t.edges
    then (a,b)
    else raise Not_found (* TODO *)

let get_node a t =
  IDMap.find a t.nodes

let create ids =
  let nodes,handles =
    List.fold_left
      (fun (map,set) i -> IDMap.add i (Single i) map, HandleSet.add i set)
      (IDMap.empty,HandleSet.empty)
      (ids)
  and edges = EdgeSet.empty
  and avail_codes = IDManager.of_list ids in
  {empty with
    edges; nodes; handles; avail_codes; }

let disjoint t =
  let leaves = get_leaves t in
  let nodes =
    IDSet.fold (fun i acc -> IDMap.add i (Single i) acc) leaves IDMap.empty
  and edges = EdgeSet.empty
  and handles = 
    IDSet.fold (fun i acc -> HandleSet.add i acc) leaves HandleSet.empty
  and avail_codes = IDManager.of_list (IDSet.elements leaves) in
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
    | Single x, None ->
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
      let a,b = get_other_two curr x a b c in
      f prev curr acc
        |> processor (Some curr) a
        |> processor (Some curr) b
    | Single _, Some _ ->
      assert false
  in
  processor None h acc

let pre_order_edges ?dist f ((a,b) as e) t acc =
  let rec each_edge dist prev curr accum =
    if dist = 0 then
      accum
    else match get_node curr t with
      | Single _ -> assert false
      | Interior (_,a,b,c) ->
        let a,b = get_other_two curr prev a b c in
        f (curr,a) accum
          |> each_edge (dist-1) curr a
          |> f (curr,b)
          |> each_edge (dist-1) curr b
      | Leaf _ -> accum
  in
  let d = match dist with
    | None   -> -1
    | Some x -> x
  in
  f e acc
    |> each_edge d a b
    |> each_edge d b a

let post_order_edges f g (a, b) bt accum =
  let rec processor prev curr accum =
    match get_node curr bt with
    | Leaf (nd, nbr) ->
      assert(prev = nbr);
      assert(curr = nd);
      f prev curr accum
    | Interior (nd, nbr1, nbr2, nbr3) ->
      let a, b = get_other_two nd prev nbr1 nbr2 nbr3 in
      let aacc = processor nd a accum
      and bacc = processor nd b accum in
      g prev curr aacc bacc
    | Single _ -> assert false
  in
  let a = processor b a accum
  and b = processor a b accum in
  a, b

let get_edges h t = match get_node h t with
  | Interior (a,b,_,_) | Leaf (a,b) ->
    pre_order_edges EdgeSet.add (a,b) t EdgeSet.empty
  | Single _ ->
    EdgeSet.empty

let get_all_edges t = t.edges

let get_neighbors x t = match get_node x t with
  | Single _ -> []
  | Leaf (_,x) -> [x]
  | Interior (_,x,y,z) -> [x;y;z]

let get_neighborhood dist edge t =
  pre_order_edges ~dist EdgeSet.add edge t EdgeSet.empty

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

let compare_topology t1 t2 =
  let extract_set t x acc =
    let x,y,_ = partition_edge x t in
    IntSetSet.add x acc |> IntSetSet.add y
  in
  let t1s =
    List.fold_right (extract_set t1) (EdgeSet.elements t1.edges) IntSetSet.empty
  and t2s =
    List.fold_right (extract_set t2) (EdgeSet.elements t2.edges) IntSetSet.empty
  in
  IntSetSet.compare t1s t2s

let compare t1 t2 =
  if t1.nodes = t2.nodes && t1.edges = t2.edges
    then 0
    else compare_topology t1 t2

let handle_of n t =
  let module M = struct exception Found of id end in
  try
    let () =
      pre_order_nodes
        (fun _ x () ->
          if HandleSet.mem x t.handles then raise (M.Found x) else ()) n t ()
    in
    raise Not_found
  with M.Found x -> x

let path_of a b t =
  let rec build_path acc prev a = match get_node a t with
    | Leaf     (_,x)     when x = b -> x :: acc
    | Interior (_,x,_,_) when x = b -> x :: acc
    | Interior (_,_,x,_) when x = b -> x :: acc
    | Interior (_,_,_,x) when x = b -> x :: acc
    | Interior (_,x,y,z) ->
      let sa = Some a in
      begin match prev with
        | None ->
          begin
            try build_path (x::acc) sa x with Not_found ->
            try build_path (y::acc) sa y with Not_found ->
                build_path (z::acc) sa z
          end
        | Some p ->
          begin
            let x,y = get_other_two a p x y z in
            try build_path (x::acc) sa x with Not_found ->
                build_path (y::acc) sa y
          end
      end
    | Leaf (_,y) ->
      begin match prev with
        | None -> build_path (y::acc) (Some a) y
        | Some _ -> raise Not_found
      end
    | Single _   -> raise Not_found
  in
  if a = b
    then [a]
    else List.rev @@ build_path [a] None a

let rec traverse_path f ids t acc = match ids with
  | [] | [_] -> acc
  | x1::((x2::_) as xs) -> traverse_path f xs t (f acc x1 x2)

let disjoint_edge _ _ = true

let jxn_of_delta (d : break tdelta) : jxn * jxn =
  match d.jxn_of with
  | [x;y] -> x,y
  | _     -> assert false

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
  assert( is_edge x y t ); (* TODO *)
  match get_node x t, get_node y t with
  | (Single _, _ | _, Single _) ->
    assert false
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
        removed = {empty_side with d_edges = [(a,x)];};
         jxn_of = [ `Single a; `Single x ]; }
    in
    let t = {t with nodes; edges; handles;} in
    t,delta
  (*       b           b
   *      /            |
   * x---a   --->  x + |
   *      \            |
   *       c           c *)
  | Leaf (x,y), Interior(a,b,c,d)
  | Interior(a,b,c,d), Leaf (x,y) ->
    assert( y = a );
    let b,c = get_other_two a x b c d in
    let h = handle_of a t in
    let t = clean_up_nodes b c x a t in
    let nodes = IDMap.add x (Single x) t.nodes
    and handles,d_handles =
      if h = x
        then HandleSet.add b t.handles,[b]
        else HandleSet.add x t.handles,[x]
    in
    let t = {t with nodes; handles; avail_codes = IDManager.push a t.avail_codes; } in
    let delta =
      { created = {d_nodes = []; d_edges = [(b,c)]; d_handles;};
        removed = {d_nodes = [a];d_edges = [(a,x);(a,b);(a,c)]; d_handles = [];};
         jxn_of = [ `Single x; `Edge (b,c) ]; }
    in
    t,delta
  (* b       x     b   x
   *  \     /      |   |
   *   a---w  ---> | + |
   *  /     \      |   |
   * c       y     c   y    *)
  | Interior (a,b,c,d), Interior (w,x,y,z) ->
    let b,c = get_other_two a w b c d
    and x,y = get_other_two w a x y z in
    let h = handle_of a t in
    let t = clean_up_nodes b c w a t in
    let t = clean_up_nodes x y a w t in
    let handles = failwith "TODO" in
    let avail_codes = IDManager.push a @@ IDManager.push w t.avail_codes in
    let delta =
      let add_hs = if h = b then [x] else if h = x then [b] else [x;b]
      and rem_hs = if h = b || h = x then [] else [h] in
      { created = {d_nodes = []; d_edges = [(b,c);(x,y)]; d_handles = add_hs;};
        removed = {d_nodes = [a;w]; d_edges = [(a,b);(a,c);(w,x);(w,y);(a,w)];
                   d_handles = rem_hs;};
         jxn_of = [`Edge (b,c); `Edge (x,y)];}
    in
    let t = {t with avail_codes; handles; } in
    t,delta
   
let join j1 j2 t =
  let jxn_of = [j1;j2] in
  match j1, j2 with
  (* x + y ---> x -- y *)
  | `Single x, `Single y ->
    assert( is_single x t ); (* TODO *)
    assert( is_single y t ); (* TODO *)
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
        removed = {empty_side with d_handles = [x];};
         jxn_of; }
    in
    {t with edges; nodes; handles;},delta
  (*     y            y
   *     |           /
   * x + | ---> x---a
   *     |           \
   *     z            z *)
  | `Single x, `Edge (y,z)
  | `Edge (y,z), `Single x ->
    assert (is_single x t); (* TODO *)
    assert (is_edge y z t); (* TODO *)
    let n_id, avail_codes = IDManager.pop t.avail_codes in
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
        removed = { d_nodes = []; d_edges = [(y,z)]; d_handles = [x]; };
         jxn_of; }
    in
    {t with edges; nodes; handles; avail_codes;}, delta
  (*  w   y    w       y
   *  |   |     \     /
   *  | + | ---> a---b
   *  |   |     /     \
   *  x   z    x       z *)
  | `Edge (w,x), `Edge (y,z) ->
    assert (is_edge w x t); (* TODO *)
    assert (is_edge y z t); (* TODO *)
    let a, avail_codes = IDManager.pop t.avail_codes in
    let b, avail_codes = IDManager.pop avail_codes in
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
        removed = { d_nodes = []; d_edges = [(y,z);(w,x)]; d_handles = [x]; };
         jxn_of; }
    in
    {t with avail_codes; edges; nodes; handles; }, delta

let move_handle n t =
  let h = handle_of n t in
  let p = path_of h n t in
  let handles =
    t.handles
      |> HandleSet.remove h
      |> HandleSet.add n
  in
  {t with handles;},p

let reroot = move_handle

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


(** {1 Tree Specific Functions} *)

(** {2 Formatter/Printer Functions} *)

let pp_node ppf = function
  | Leaf (a,b) -> 
    Format.fprintf ppf "@[L(%d,%d)@]" a b
  | Interior (a,b,c,d) ->
    Format.fprintf ppf "@[N(%d,%d,%d,%d)@]" a b c d
  | Single a ->
    Format.fprintf ppf "@[S(%d)@]" a

and pp_tree ppf t =
  let outputf format = Format.fprintf ppf format in
  outputf "@[@[Handles :@] ";
  HandleSet.iter (fun a -> outputf "@[H(%d)@]@ " a) t.handles;
  outputf "@]@\n@[@[Edges :@] ";
  EdgeSet.iter (fun (a,b) -> outputf "@[(%d,%d)@]@ " a b) t.edges;
  outputf "@]@\n@[Nodes :@] ";
  IDMap.iter
    (fun _ -> function
      | Leaf (a,b) -> outputf "@[L(%d,%d)@] " a b
      | Interior (a,b,c,d) -> outputf "@[N(%d,%d,%d,%d)@] " a b c d
      | Single a -> outputf "@[S(%d)@] " a)
    t.nodes;
  outputf "@]@\n";
  ()

let dump output t =
  let outputf format = Printf.ksprintf (output) format in
  outputf "Handles : ";
  HandleSet.iter (fun a -> outputf "H(%d) " a) t.handles;
  outputf "\nEdges : ";
  EdgeSet.iter (fun (a,b) -> outputf "(%d,%d) " a b) t.edges;
  outputf "\nNodes : ";
  IDMap.iter
    (fun _ -> function
      | Leaf (a,b) -> outputf "L(%d,%d) " a b
      | Interior (a,b,c,d) -> outputf "N(%d,%d,%d,%d) " a b c d
      | Single a -> outputf "S(%d) " a)
    t.nodes;
  outputf "\n";
  ()


(** {2 Math Functions} *)

let zero  = Num.num_of_int 0
let one   = Num.num_of_int 1
let two   = Num.num_of_int 2
let three = Num.num_of_int 3
let five  = Num.num_of_int 5

let num_edges =
  (fun n ->
    if Num.lt_num n zero
      then raise Not_found (* TODO *)
    else if Num.eq_num n zero
      then n
    else if Num.eq_num n one
      then n
    else
      Num.sub_num (Num.mult_num two n) two)
 
let num_nodes =
  (fun n ->
    if Num.lt_num n zero
      then raise Not_found (* TODO *)
    else if Num.eq_num n zero
      then zero
    else if Num.eq_num n one
      then n
    else
      Num.sub_num (Num.mult_num two n) two)

let num_trees =
  let d_fact n =
    let rec d_fact final acc n =
      if Num.eq_num n final then acc
      else d_fact final (Num.mult_num n acc)
                        (Num.sub_num n two)
    in
    d_fact one one n
  in
  (fun n ->
    if Num.lt_num n zero
      then raise Not_found (* TODO *)
    else if Num.eq_num n zero
      then zero
    else if Num.lt_num n three
      then one
    else
      d_fact (Num.sub_num (Num.mult_num two n) five))

