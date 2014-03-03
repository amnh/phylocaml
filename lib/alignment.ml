open Internal

module type AssignCost =
  sig
    type cost
    type elt
    type model

    val zero : model -> cost

    val inf  : model -> cost
    val add  : model -> cost -> cost -> cost
    val min  : model -> cost -> cost -> cost
    val cost   : model -> elt -> elt -> cost
    val min3 : model -> (cost * 'a) -> (cost * 'a) -> (cost * 'a) -> cost * 'a list
    val minN : model -> (cost * 'a) list -> cost * 'a list

    val indel  : model -> elt
    val assign : model -> elt -> elt -> elt
    val median : model -> elt -> elt -> cost * elt

    val pp_cost : Format.formatter -> cost -> unit
    val pp_elt : Format.formatter -> elt -> unit
  end

module type DataVector =
  sig
    type elt
    type t

    val length : t -> int
    val get : t -> int -> elt
    val set : t -> int -> elt -> t
    val unsafe_get : int -> t -> elt
    val unsafe_set : int -> elt -> t -> t
    val of_list : elt list -> t

    val pp_t : Format.formatter -> t -> unit
  end

module type Alignment =
  sig

    type t
    type m
    type c
    type mem

    val alignments : mem -> m -> t -> t -> t * t
    val backtrace : mem -> m -> t -> t -> t
    val align : mem -> m -> t -> t -> c

    val mem_to_latex : mem -> string
  end

module FullAlignment (V:DataVector) (C:AssignCost with type elt = V.elt) =
  struct

    type e = V.elt
    type t = V.t
    type m = C.model
    type c = C.cost

    type dir =
      Root | Align  of e | Delete of e | Insert of e
    
    type mem =
      (c * dir list) array array

    let mem_zero m = (C.zero m,[])
    let mem_ref = ref None
    let rec create_mem m x y : mem = match !mem_ref with
      | None ->
        let mem = Array.make_matrix (V.length x) (V.length y) @@ mem_zero m in
        mem_ref := Some mem;
        mem
      | Some mem ->
        if (Array.length mem) <= (V.length x) &&
           (Array.length mem.(0)) <= (V.length y) then
          mem
        else begin
          mem_ref := None;
          create_mem m x y
        end

    let choose_dir dirs =
      let is_del = function
        | Delete _ -> true
        | (Align _ | Insert _ | Root) -> false
      and is_aln = function
        | Align  _ -> true
        | (Delete _ | Insert _ | Root) -> false
      and is_ins = function
        | Insert _ -> true
        | (Align _ | Delete _ | Root) -> false
      and is_root = function
        | Root     -> true 
        | (Align _ | Delete _ | Insert _) -> false
      in
           if List.exists is_del dirs then List.find is_del dirs
      else if List.exists is_aln dirs then List.find is_aln dirs
      else if List.exists is_ins dirs then List.find is_ins dirs
      else match dirs with
           | [x] when is_root x -> Root
           |  _ -> assert false

    let alignments mem m x y =
      let indel = C.indel m in
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_alignments one two i j = match get_direction i j with
        | Align  _ -> build_alignments ((V.get x i)::one) ((V.get y j)::two) (i-1) (j-1)
        | Insert _ -> build_alignments (indel ::one) ((V.get y j)::two) (i) (j-1)
        | Delete _ -> build_alignments ((V.get x i)::one) (indel::two) (i-1) (j)
        | Root     -> V.of_list @@ indel::one, V.of_list @@ indel::two
      in
      build_alignments [] [] ((V.length x)-1) ((V.length y)-1)

    let backtrace mem _m x y =
      let get_direction i j = mem.(i).(j) |> snd |> choose_dir in
      let rec build_median acc i j = match get_direction i j with
        | Align  s -> build_median (s::acc) (i-1) (j-1)
        | Delete s -> build_median (s::acc) (i-1) j
        | Insert s -> build_median (s::acc) i (j-1)
        | Root     -> V.of_list ((V.get x 0)::acc)
      in
      build_median [] ((V.length x)-1) ((V.length y)-1)

    let align mem m x y =
      let xlen = V.length x and ylen = V.length y in
      let indel = C.indel m in
      let get_cost i j =
        if i = 0 && j = 0 then begin
          (C.zero m,[Root])
        end else if i = 0 then begin
          let cst,s = C.median m indel (V.get y j) in
          C.add m (fst mem.(i).(j-1)) cst,[Insert s]
        end else if j = 0 then begin
          let cst,s = C.median m (V.get x i) indel in
          C.add m (fst mem.(i-1).(j)) cst,[Delete s]
        end else begin
          let dcst,sd = C.median m (V.get x i) indel
          and icst,si = C.median m indel (V.get y j)
          and acst,sa = C.median m (V.get x i) (V.get y j) in
          let dcst = C.add m (fst mem.(i-1).(j)) dcst
          and icst = C.add m (fst mem.(i).(j-1)) icst
          and acst = C.add m (fst mem.(i-1).(j-1)) acst in
          C.min3 m (dcst,Delete sd) (icst,Insert si) (acst,Align sa)
        end
      in
      for i = 0 to xlen - 1 do
        for j = 0 to ylen - 1 do
          mem.(i).(j) <-get_cost i j
        done;
      done;
      fst mem.(xlen-1).(ylen-1)

    let mem_to_latex (mat : mem) = failwith "TODO"

  end


