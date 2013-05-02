open Phylocaml_pervasives

type collapse_type = [ `Single | `FinalMin | `FinalMax ]

type dir = [ `Parent of Topology.id list | `Children of Topology.id list ] option

module type R =
  sig

    type nd

    type n

    val of_data : Topology.id -> nd IntMap.t -> n

    val height : dir -> n -> int

    val cardinal : n -> int
    val filter_codes : IntSet.t -> n -> n
    val compare : n -> n -> int
    val recode : (int -> int) -> n -> n
    val get_codes : n -> IntSet.t
    
    val is_collapsable : collapse_type -> n -> n -> bool

    val get_preliminary : dir -> int -> n -> nd
    val get_adjusted : int -> n -> nd

    val distance_1 : n -> n -> float
    val distance_2 : n -> n -> n -> float

    val median_1 : Topology.id -> n option -> n -> n
    val median_2 : Topology.id -> n option -> n -> n -> n
    val median_n : Topology.id -> n option -> n list -> n

    val readjust_3 : ?prelim:bool -> IntSet.t option -> n -> n -> n -> n -> n * IntSet.t
    val readjust_n : ?prelim:bool -> IntSet.t option -> n -> n list -> n * IntSet.t

    val final_states : n -> n -> n -> n -> n

    val uppass_heuristic_internal_3 : n -> n -> n -> n -> n
    val uppass_heuristic_internal_n : n -> n list -> n
    val uppass_heuristic_leaf : n -> n -> n
    val uppass_heuristic_root : n -> n -> n -> n

    val to_single : n option -> n -> n -> n -> n

    val tree_cost : dir -> n -> float

    val node_cost : dir -> n -> float

    val to_string : n -> string

    val to_xml : Pervasives.out_channel -> n -> unit
end


module type S =
    functor (NodeData : NodeData.S) -> R with type nd = NodeData.t


module Make1D (Ordering : Topology.NodeComparator) : S =
    functor (NodeData : NodeData.S) ->
  struct

    type nd = NodeData.t

    type n  = {
      prelim: nd IntMap.t;
      final : nd IntMap.t option;
      topot : Ordering.t;
      height : int;
      cost : float;
      code : int; 
    }

    let accum_over_map f a i t =
      IntMap.fold (fun _ v acc -> a (f v) acc) t i

    let of_data id x =
      let cost = accum_over_map NodeData.leaf_cost (+.) 0.0 x in
      { prelim = x; 
        final = None;
        topot = Ordering.create id;
        height = 0;
        cost = cost;
        code = id;
      }

    let height _ x = x.height

    let cardinal x = accum_over_map NodeData.cardinal (+) 0 x.prelim

    let filter_codes cs x =
      let filter_char c =
        IntMap.fold
          (fun k v t -> match NodeData.filter_codes cs v with
            | Some x -> IntMap.add k x t
            | None   -> t)
          IntMap.empty
          c
      in
      { x with
        prelim = filter_char x.prelim;
        final  = match x.final with
               | None -> None
               | Some x -> Some (filter_char x); }

    let map_map3 f a b c =
      assert( (IntMap.cardinal a) = (IntMap.cardinal b));
      assert( (IntMap.cardinal a) = (IntMap.cardinal c));
      IntMap.mapi (fun k av -> f av (IntMap.find k b) (IntMap.find k c)) a

    let map_map2 f a b =
      assert( (IntMap.cardinal a) = (IntMap.cardinal b));
      IntMap.mapi (fun k av -> f av (IntMap.find k b)) a

    let get_val prelim =
      if prelim then
        (fun k x -> IntMap.find k x.prelim)
      else
        (fun k x -> match x.final with
          | None -> raise Not_found
          | Some x -> IntMap.find k x)

    let compare a b =
      IntMap.compare NodeData.compare a.prelim b.prelim

    let recode f x =
      {x with
        prelim = IntMap.map (NodeData.recode f) x.prelim;
        final  = match x.final with
               | None   -> None
               | Some x -> Some (IntMap.map (NodeData.recode f) x); }

    let get_codes x =
      accum_over_map NodeData.get_codes IntSet.union IntSet.empty x.prelim

    let is_collapsable _ _ _ = failwith "TODO"
      
    let get_preliminary _ = get_val true

    let get_adjusted = get_val false
    
    let get_topot x = x.topot

    let distance_1 a b =
      let a,b = Ordering.order get_topot a b in
      IntMap.fold
        (fun k av acc -> acc +. (NodeData.distance_1 av (IntMap.find k b.prelim)))
        a.prelim
        0.0

    let distance_2 a b c =
      IntMap.fold
        (fun k av acc ->
          acc +. (NodeData.distance_2 av (IntMap.find k b.prelim) (IntMap.find k c.prelim)))
        a.prelim
        0.0

    let median_1 id o a =
      let prelim = match o with
        | None   -> IntMap.map (NodeData.median_1 None) a.prelim
        | Some x ->
          assert( x.code = id );    
          map_map2 (fun o -> NodeData.median_1 (Some o)) x.prelim a.prelim
      in
      let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
      { prelim = prelim;
        final = None;
        height = a.height + 1;
        topot = Ordering.ancestor_1 id a.topot;
        cost = cost;
        code = id;
      }

    let median_2 id o a b =
      let a,b = Ordering.order get_topot a b in
      let prelim = match o with
        | None   -> map_map2 (NodeData.median_2 None) a.prelim b.prelim
        | Some x ->
          assert( x.code = id );    
          map_map3 (fun o -> NodeData.median_2 (Some o)) x.prelim a.prelim b.prelim
      in
      let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
      { prelim = prelim;
        final = None;
        height = (max a.height b.height) + 1;
        topot = Ordering.ancestor_2 id a.topot b.topot;
        cost = cost;
        code = id;
      }

    let median_n id o bs =
      let bs = Ordering.sort get_topot bs in
      let prelim = match o with
        | None   -> failwith "TODO"
        | Some _ -> failwith "TODO"
      in
      let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
      { prelim = prelim;
        final = None;
        height = (List.fold_left (fun acc a -> (max a.height acc)) 0 bs) + 1;
        topot = Ordering.ancestor_n id (List.map get_topot bs);
        cost = cost;
        code = id;
      }

    let readjust_3 ?(prelim=false) codes n a b c =
      let get_val = get_val prelim in
      let prelim,set =
        assert( (IntMap.cardinal n.prelim) = (IntMap.cardinal b.prelim));
        assert( (IntMap.cardinal a.prelim) = (IntMap.cardinal c.prelim));
        assert( (IntMap.cardinal a.prelim) = (IntMap.cardinal n.prelim));
        IntMap.fold
          (fun k _ (t,s) ->
            let nv,ns =
              NodeData.adjust_3 codes (get_val k n) (get_val k a)
                                      (get_val k b) (get_val k c) in
            IntMap.add k nv t, IntSet.union ns s)
          n.prelim
          (IntMap.empty, IntSet.empty)
      in
      {n with
        prelim = prelim;
        final  = Some prelim; }, set

    let readjust_n ?(prelim=false) _ n _ =
      let prelim,set = failwith "TODO" in
      {n with
        prelim = prelim;
        final = Some prelim; }, set

    let final_states _ _ _ _ = failwith "TODO"

    let uppass_heuristic_internal_3 _ _ _ _ = failwith "TODO"

    let uppass_heuristic_internal_n _ _ = failwith "TODO"

    let uppass_heuristic_leaf _ _ = failwith "TODO"

    let uppass_heuristic_root _ _ _ = failwith "TODO"

    let to_single _ _ _ _ = failwith "TODO"

    let tree_cost _ x = x.cost

    let node_cost _ x = x.cost

    let to_string _ = failwith "TODO"

    let to_xml _ _ = failwith "TODO"
end


module MakeLazy (Ordering : Topology.NodeComparator) : S =
  functor (NodeData : NodeData.S) ->
struct

  module Node = Make1D (Ordering) (NodeData)
  type nd = Node.nd

  type n = Node.n Lazy.t

  let force_opt x = match x with
    | Some x -> Some (Lazy.force_val x)
    | None   -> None

  and force = Lazy.force_val

  and force_lst = List.map (Lazy.force_val)

  let force_apply x f = f (Lazy.force_val x)

  let of_data i x = Lazy.lazy_from_val (Node.of_data i x)

  let height d x = force_apply x (Node.height d)
  let cardinal x = force_apply x Node.cardinal
  let compare a b = Node.compare (force a) (force b)
  let recode f x = Lazy.lazy_from_val (Node.recode f (force x))
  let is_collapsable t a b = Node.is_collapsable t (force a) (force b)

  let filter_codes cs x =
    Lazy.lazy_from_fun (fun () -> force_apply x (Node.filter_codes cs))

  let get_codes x = force_apply x Node.get_codes

  let get_preliminary d k y =
    force_apply y (Node.get_preliminary d k)

  let get_adjusted k y = force_apply y (Node.get_adjusted k)
    
  let distance_1 a b =
    Node.distance_1 (force a) (force b)
  let distance_2 a b c =
    Node.distance_2 (force a) (force b) (force c)

  let median_1 i a b =
    Lazy.from_fun (fun () -> Node.median_1 i (force_opt a) (force b))
  let median_2 i a b c =
    Lazy.from_fun (fun () -> Node.median_2 i (force_opt a) (force b) (force c))
  let median_n i a bs =
    Lazy.from_fun (fun () -> Node.median_n i (force_opt a) (force_lst bs))
    
  let readjust_3 ?prelim i a b c d =
    let n,i = Node.readjust_3 ?prelim i (force a) (force b) (force c) (force d) in
    Lazy.from_val n, i

  let readjust_n ?prelim i n ns =
    let n,i = Node.readjust_n ?prelim i (force n) (force_lst ns) in
    Lazy.from_val n, i

  let uppass_heuristic_internal_3 a b c d =
    Lazy.from_fun (fun () ->
      Node.uppass_heuristic_internal_3 (force a) (force b) (force c) (force d))

  let uppass_heuristic_internal_n a b =
    Lazy.from_fun (fun () ->
      Node.uppass_heuristic_internal_n (force a) (force_lst b))

  let uppass_heuristic_leaf a b =
    Lazy.from_fun (fun () -> Node.uppass_heuristic_leaf (force a) (force b))

  let uppass_heuristic_root a b c =
    Lazy.from_fun (fun () ->
      Node.uppass_heuristic_root (force a) (force b) (force c))

  let final_states a b c d =
    Lazy.from_fun (fun () -> Node.final_states (force a) (force b) (force c) (force d))
  let to_single a b c d =
    Lazy.from_fun (fun () -> Node.to_single (force_opt a) (force b) (force c) (force d))

  let tree_cost d a = force_apply a (Node.tree_cost d)
  let node_cost d a = force_apply a (Node.node_cost d)

  let to_string a = force_apply a Node.to_string
  let to_xml c a = force_apply a (Node.to_xml c)
end


module Make3D (Ordering : Topology.NodeComparator) : S =
  functor (NodeData : NodeData.S) ->
struct

  module Node = MakeLazy (Ordering) (NodeData)
    
  type nd  = Node.nd

  type nodes =
    | L of Node.n
    | I of (Node.n * (Topology.id * Topology.id)) list

  type n = {
    unadjusted : nodes;
    adjusted   : Node.n option;
    code       : int;
  }

  let not_with ids (_,(d1,d2)) = match ids with
    | [x] -> not ((x = d1) || (x = d2))
    |  _  -> assert false

  and yes_with ids (_,(d1,d2)) = match ids with
    | [x;y] -> ((d1 = x) && (d2 = y)) || ((d1 = y) && (d2 = x))
    |   _   -> assert false

  let get_any n = match n.unadjusted with
    | L x      -> x
    | I ((x,_)::_) -> x
    | I [] -> assert false

  let get_dir dir n : Node.n =
    let res = match n.unadjusted,dir with
      | L x,_ -> [x]
      | I xs, Some (`Parent ids) ->
        List.filter (not_with ids) xs |> List.map fst
      | I xs,Some (`Children ids) ->
        List.filter (yes_with ids) xs |> List.map fst
      | I _,None ->
        assert false
    in
    match res with
    | [x] -> x
    |  _  -> raise Not_found

(*  let acc_nodes f t =*)
(*    let unadj = match t.unadjusted with*)
(*      | L x -> x*)
(*      | I xs -> List.map fst xs in*)
(*    let all  = match t.adjusted with*)
(*      | None -> unadj*)
(*      | Some x -> x :: unadj*)
(*    in*)
(*    List.fold_right f all*)

  let map_nodes f t =
      let unadj = match t.unadjusted with
        | L x -> L (f x)
        | I xs -> I (List.map (fun (x,d) -> (f x,d)) xs)
      and adj = match t.adjusted with
        | None -> None
        | Some x -> Some (f x)
      in
      {t with
        unadjusted = unadj;
        adjusted = adj; }

  let of_data i x =
    { unadjusted = L (Node.of_data i x);
      adjusted = None;
      code = i;
    }

  let compare _ _ = failwith "TODO"

  let height d x =
    Node.height d (get_dir d x)

  let cardinal n =
    Node.cardinal (get_any n)

  let get_codes n =
    Node.get_codes (get_any n)

  let filter_codes c t =
    map_nodes (Node.filter_codes c) t

  let recode f t =
    map_nodes (Node.recode f) t
  
  let is_collapsable _ _ _ = failwith "TODO"

  let get_preliminary d c t =
    Node.get_preliminary d c (get_dir d t)

  let get_adjusted c t = match t.adjusted with
    | None -> raise Not_found
    | Some x -> Node.get_adjusted c x
    
  let distance_1 _ _ = failwith "TODO"
  let distance_2 _ _ _ = failwith "TODO"

  let median_1 _ _ = failwith "TODO"
  let median_2 _ _ _ = failwith "TODO"
  let median_n _ _ _ = failwith "TODO"

  let readjust_3 ?prelim _ _ _ _ _ = failwith "TODO"
  let readjust_n ?prelim _ _ _ = failwith "TODO"

  let uppass_heuristic_internal_3 _ _ _ _ = failwith "TODO"
  let uppass_heuristic_internal_n _ _ = failwith "TODO"
  let uppass_heuristic_leaf _ _ = failwith "TODO"
  let uppass_heuristic_root _ _ _ = failwith "TODO"

  let to_single _ _ _ _ = failwith "TODO"
  let final_states _ _ _ _ = failwith "TODO"

  let tree_cost _ _ = failwith "TODO"
  let node_cost _ _ = failwith "TODO"

  let to_string _ = failwith "TODO"
  let to_xml _ _ = failwith "TODO"
end

module MakeND (Ordering : Topology.NodeComparator) : S =
    functor (NodeData : NodeData.S) ->
  struct

    module Node = MakeLazy (Ordering) (NodeData)
    
    type nd  = Node.nd

    type n = unit

    let of_data _ _ = failwith "TODO"

    let height _ _ = failwith "TODO"

    let is_collapsable _ _ _ = failwith "TODO"
    let cardinal _ = failwith "TODO"
    let filter_codes _ _ = failwith "TODO"
    let compare _ _ = failwith "TODO"
    let recode _ _ = failwith "TODO"
    let get_codes _ = failwith "TODO"

    let get_preliminary _ _ _ = failwith "TODO"
    let get_adjusted _ _ = failwith "TODO"
    
    let distance_1 _ _ = failwith "TODO"
    let distance_2 _ _ _ = failwith "TODO"

    let median_1 _ _ = failwith "TODO"
    let median_2 _ _ _ = failwith "TODO"
    let median_n _ _ _ = failwith "TODO"

    let readjust_3 ?prelim _ _ _ _ _ = failwith "TODO"
    let readjust_n ?prelim _ _ _ = failwith "TODO"

    let final_states _ _ _ _ = failwith "TODO"
    let uppass_heuristic_internal_3 _ _ _ _ = failwith "TODO"
    let uppass_heuristic_internal_n _ _ = failwith "TODO"
    let uppass_heuristic_leaf _ _ = failwith "TODO"
    let uppass_heuristic_root _ _ _ = failwith "TODO"
    let to_single _ _ _ _ = failwith "TODO"

    let tree_cost _ = failwith "TODO"
    let node_cost _ = failwith "TODO"

    let to_string _ = failwith "TODO"
    let to_xml _ _ = failwith "TODO"
end

