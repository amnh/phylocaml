open Internal

type dir = [ `Parent of Topology.id list | `Children of Topology.id list ] option

module type S =
  sig

    type nd (* node abstraction *)

    type r  (* root data *)
    type n  (* node data *)
    type m  (* model     *)

    val of_data : Topology.id -> nd IntMap.t -> n

    val height : dir -> n -> int
    val cardinal : n -> int
    val compare : n -> n -> int

    val filter_codes : IntSet.t -> n -> n
    val recode : (int -> int) -> n -> n
    val get_codes : n -> IntSet.t
    
    val get_preliminary : dir -> int -> n -> nd
    val get_adjusted : int -> n -> nd

    val distance_1 : m -> n -> n -> float
    val distance_2 : m -> n -> n -> n -> float

    val median_1 : m -> Topology.id -> n option -> n -> n
    val median_2 : m -> Topology.id -> n option -> n -> n -> n
    val median_3 : m -> Topology.id -> n option -> n -> n -> n -> n
    val median_n : m -> Topology.id -> n option -> n list -> n

    val to_single : m -> n option -> n -> n -> n -> n
    val final_states : m -> n -> n -> n -> n -> n

    val readjust_3 : ?prelim:bool -> m -> IntSet.t option -> n -> n -> n -> n -> n * IntSet.t
    val readjust_n : ?prelim:bool -> m -> IntSet.t option -> n -> n list -> n * IntSet.t

    val uppass_heuristic_internal_3 : m -> n -> n -> n -> n -> n
    val uppass_heuristic_internal_n : m -> n -> n list -> n
    val uppass_heuristic_leaf : m -> n -> n -> n
    val uppass_heuristic_root : m -> n -> n -> n -> n

    val root_cost : dir -> n -> float
    val node_cost : dir -> n -> float

    val to_string : n -> string
    val to_xml : Pervasives.out_channel -> n -> unit
end


module type R =
    functor (Model    : Model.S) ->
    functor (NodeData : NodeData.S with type m = Model.t) ->
      S with type nd = NodeData.t with type m = Model.t

module Make1D (Ordering : Topology.NodeComparator) : R =
    functor (Model    : Model.S) ->
    functor (NodeData : NodeData.S with type m = Model.t) ->
struct

  type nd = NodeData.t
  type m = Model.t

  type n  = {
    prelim: nd IntMap.t;
    final : nd IntMap.t option;
    topot : Ordering.t;
    height : int;
    cost : float;
    code : int; 
  }

  type r = n

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

  let map_map4 f a b c d =
    assert( (IntMap.cardinal a) = (IntMap.cardinal b));
    assert( (IntMap.cardinal a) = (IntMap.cardinal c));
    assert( (IntMap.cardinal a) = (IntMap.cardinal d));
    IntMap.mapi
      (fun k av -> f av (IntMap.find k b) (IntMap.find k c) (IntMap.find k d)) a

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

  let get_preliminary _ = get_val true

  let get_adjusted = get_val false
  
  let get_topot x = x.topot

  let distance_1 m a b =
    let a,b = Ordering.order get_topot a b in
    IntMap.fold
      (fun k av acc -> acc +. (NodeData.distance_1 m av (IntMap.find k b.prelim)))
      a.prelim
      0.0

  let distance_2 m a b c =
    IntMap.fold
      (fun k av acc ->
        acc +. (NodeData.distance_2 m av (IntMap.find k b.prelim) (IntMap.find k c.prelim)))
      a.prelim
      0.0

  let median_1 m id o a =
    let prelim = match o with
      | None   -> IntMap.map (NodeData.median_1 m None) a.prelim
      | Some x ->
        assert( x.code = id );    
        map_map2 (fun o -> NodeData.median_1 m (Some o)) x.prelim a.prelim
    in
    let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
    { prelim = prelim;
      final = None;
      height = a.height + 1;
      topot = Ordering.ancestor_1 id a.topot;
      cost = cost;
      code = id;
    }

  let median_2 m id o a b =
    let a,b = Ordering.order get_topot a b in
    let prelim = match o with
      | None   -> map_map2 (NodeData.median_2 m None) a.prelim b.prelim
      | Some x ->
        assert( x.code = id );    
        map_map3 (fun o -> NodeData.median_2 m (Some o)) x.prelim a.prelim b.prelim
    in
    let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
    { prelim = prelim;
      final = None;
      height = (max a.height b.height) + 1;
      topot = Ordering.ancestor_2 id a.topot b.topot;
      cost = cost;
      code = id;
    }

  let median_3 m id o a b c =
    let a,b,c = match Ordering.sort get_topot [a;b;c] with
      | [a;b;c] -> a,b,c
      |  _      -> assert false
    in
    let prelim = match o with
      | None   ->
        map_map3 (NodeData.median_3 m None)
                 a.prelim b.prelim c.prelim
      | Some x ->
        assert( x.code = id );
        map_map4 (fun o -> NodeData.median_3 m (Some o))
                 x.prelim a.prelim b.prelim c.prelim
    in
    let cost = accum_over_map NodeData.cost (+.) 0.0 prelim in
    { prelim = prelim;
      final = None;
      height = (max a.height b.height) + 1;
      topot = Ordering.ancestor_2 id a.topot b.topot;
      cost = cost;
      code = id;
    }


  let median_n _m id o bs =
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

  let readjust_3 ?(prelim=false) m codes n a b c =
    let get_val = get_val prelim in
    let prelim,set =
      assert( (IntMap.cardinal n.prelim) = (IntMap.cardinal b.prelim));
      assert( (IntMap.cardinal a.prelim) = (IntMap.cardinal c.prelim));
      assert( (IntMap.cardinal a.prelim) = (IntMap.cardinal n.prelim));
      IntMap.fold
        (fun k _ (t,s) ->
          let nv,ns =
            NodeData.adjust_3 m codes (get_val k n) (get_val k a)
                                    (get_val k b) (get_val k c) in
          IntMap.add k nv t, IntSet.union ns s)
        n.prelim
        (IntMap.empty, IntSet.empty)
    in
    {n with
      prelim = prelim;
      final  = Some prelim; }, set

  let readjust_n ?prelim:_ _ _ _ _ = failwith "TODO"

  let final_states _ _ _ _ _ = failwith "TODO"

  let uppass_heuristic_internal_3 _ _ _ _ _ = failwith "TODO"

  let uppass_heuristic_internal_n _ _ _ = failwith "TODO"

  let uppass_heuristic_leaf _ _ _ = failwith "TODO"

  let uppass_heuristic_root _ _ _ _ = failwith "TODO"

  let to_single _ _ _ _ _ = failwith "TODO"

  let root_cost _ x = x.cost

  let node_cost _ x = x.cost

  let to_string _ = failwith "TODO"

  let to_xml _ _ = failwith "TODO"
end


module MakeLazy (Ordering : Topology.NodeComparator) : R =
  functor (Model    : Model.S) ->
  functor (NodeData : NodeData.S with type m = Model.t) ->
struct

  module Node = Make1D (Ordering) (Model) (NodeData)

  type nd = Node.nd
  type n = Node.n Lazy.t
  type m = Model.t
  type r = n

  let return a = Lazy.lazy_from_val a
  
  let return_f a = Lazy.lazy_from_fun a
  
  let (>>=) a f = match a with
    | None   -> None
    | Some x -> Some (f x)

  let of_data i x = return (Node.of_data i x)

  let height d x = Node.height d (!$ x)
  let cardinal x = Node.cardinal (!$ x)
  let compare a b = Node.compare (!$ a) (!$ b)
  let recode f x = return (Node.recode f (!$ x))

  let filter_codes cs x = return_f (fun () -> Node.filter_codes cs (!$ x))

  let get_codes x = Node.get_codes (!$ x)

  let get_preliminary d k y = Node.get_preliminary d k (!$ y)

  let get_adjusted k y = Node.get_adjusted k (!$ y)
    
  let distance_1 m a b = Node.distance_1 m (!$ a) (!$ b)
  let distance_2 m a b c = Node.distance_2 m (!$ a) (!$ b) (!$ c)

  let median_1 m i a b =
    return_f (fun () -> Node.median_1 m i (a >>= Lazy.force) (!$ b))
  let median_2 m i a b c =
    return_f (fun () -> Node.median_2 m i (a >>= Lazy.force) (!$ b) (!$ c))
  let median_3 m i a b c d =
    return_f (fun () -> Node.median_3 m i (a >>= Lazy.force) (!$ b) (!$ c) (!$ d))
  let median_n m i a bs =
    return_f (fun () -> Node.median_n m i (a >>= Lazy.force) (!$$ bs))
    
  let readjust_3 ?prelim m i a b c d =
    let n,i = Node.readjust_3 ?prelim m i (!$ a) (!$ b) (!$ c) (!$ d) in
    return n, i

  let readjust_n ?prelim m i n ns =
    let n,i = Node.readjust_n ?prelim m i (!$ n) (!$$ ns) in
    return n, i

  let uppass_heuristic_internal_3 m a b c d =
    return_f (fun () -> Node.uppass_heuristic_internal_3 m (!$ a) (!$ b) (!$ c) (!$ d))

  let uppass_heuristic_internal_n m a b =
    return_f (fun () -> Node.uppass_heuristic_internal_n m (!$ a) (!$$ b))

  let uppass_heuristic_leaf m a b =
    return_f (fun () -> Node.uppass_heuristic_leaf m (!$ a) (!$ b))

  let uppass_heuristic_root m a b c =
    return_f (fun () -> Node.uppass_heuristic_root m (!$ a) (!$ b) (!$ c))

  let final_states m a b c d =
    return_f (fun () -> Node.final_states m (!$ a) (!$ b) (!$ c) (!$ d))

  let to_single m a b c d =
    return_f (fun () -> Node.to_single m (a >>= Lazy.force) (!$ b) (!$ c) (!$ d))

  let root_cost d a = Node.root_cost d (!$ a)
  let node_cost d a = Node.node_cost d (!$ a)

  let to_string a = Node.to_string (!$ a)
  let to_xml c a  = Node.to_xml c  (!$ a)
end


module Make3D (Ordering : Topology.NodeComparator) : R =
  functor (Model    : Model.S) ->
  functor (NodeData : NodeData.S with type m = Model.t) ->
struct

  module Node = MakeLazy (Ordering) (Model) (NodeData)
    
  type m = Model.t
  type nd  = Node.nd
  type r = Node.r

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
  
  let get_preliminary d c t =
    Node.get_preliminary d c (get_dir d t)

  let get_adjusted c t = match t.adjusted with
    | None -> raise Not_found
    | Some x -> Node.get_adjusted c x
    
  let distance_1 _ _ _ = failwith "TODO"
  let distance_2 _ _ _ _ = failwith "TODO"

  let median_1 _ _ _ = failwith "TODO"
  let median_2 _ _ _ _ = failwith "TODO"
  let median_3 _ _ _ _ = failwith "TODO"
  let median_n _ _ _ _ = failwith "TODO"

  let readjust_3 ?prelim:_ _ _ _ _ _ _ = failwith "TODO"
  let readjust_n ?prelim:_ _ _ _ _ = failwith "TODO"

  let uppass_heuristic_internal_3 _ _ _ _ _ = failwith "TODO"
  let uppass_heuristic_internal_n _ _ _ = failwith "TODO"
  let uppass_heuristic_leaf _ _ _ = failwith "TODO"
  let uppass_heuristic_root _ _ _ _ = failwith "TODO"

  let to_single _ _ _ _ _ = failwith "TODO"
  let final_states _ _ _ _ _ = failwith "TODO"

  let root_cost _ _ = failwith "TODO"
  let node_cost _ _ = failwith "TODO"

  let to_string _ = failwith "TODO"
  let to_xml _ _ = failwith "TODO"
end

module MakeND (Ordering : Topology.NodeComparator) : R =
  functor (Model : Model.S) ->
  functor (NodeData : NodeData.S with type m = Model.t) ->
struct

  module Node = MakeLazy (Ordering) (Model) (NodeData)
    
  type nd  = Node.nd
  type m = Model.t
  type r = unit
  type n = unit

  let of_data _ _ = failwith "TODO"

  let height _ _ = failwith "TODO"

  let cardinal _ = failwith "TODO"
  let filter_codes _ _ = failwith "TODO"
  let compare _ _ = failwith "TODO"
  let recode _ _ = failwith "TODO"
  let get_codes _ = failwith "TODO"

  let get_preliminary _ _ _ = failwith "TODO"
  let get_adjusted _ _ = failwith "TODO"
  
  let distance_1 _ _ _ = failwith "TODO"
  let distance_2 _ _ _ _ = failwith "TODO"

  let median_1 _ _ _ _ = failwith "TODO"
  let median_2 _ _ _ _ _ = failwith "TODO"
  let median_3 _ _ _ _ _ _ = failwith "TODO"
  let median_n _ _ _ _ = failwith "TODO"

  let readjust_3 ?prelim:_ _ _ _ _ _ _ = failwith "TODO"
  let readjust_n ?prelim:_ _ _ _ _ = failwith "TODO"

  let final_states _ _ _ _ _ = failwith "TODO"
  let uppass_heuristic_internal_3 _ _ _ _ _ = failwith "TODO"
  let uppass_heuristic_internal_n _ _ _ = failwith "TODO"
  let uppass_heuristic_leaf _ _ _ = failwith "TODO"
  let uppass_heuristic_root _ _ _ _ = failwith "TODO"
  let to_single _ _ _ _ _ = failwith "TODO"

  let root_cost _ = failwith "TODO"
  let node_cost _ = failwith "TODO"

  let to_string _ = failwith "TODO"
  let to_xml _ _ = failwith "TODO"
end

