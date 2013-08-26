
open Internal

(** {2 Definition of an Alphabet} *)

type kind =
  | Sequential
  | SimpleBitFlag
  | ExtendedBitFlag
  | Continuous
  | CombinationLevels of int

type t =
  { comb_set : IntSet.t IntMap.t;
    set_comb : int IntSetMap.t;
    name_code : int StringMap.t;
    code_name : string IntMap.t;
    comp_code : int IntMap.t;
    alphabet_type : kind;
    size : int;
    full_size : int;
    orientation : bool;
    gap : int option;
    missing: int option;
    all : int option;
  }

let print a =
  let str_of_ioption = function
    | None -> "none"
    | Some x -> string_of_int x
  and pp_ilst chan = function
    | [] -> ()
    | [x] ->
      Printf.fprintf chan "%d" x
    | x::xs ->
      Printf.fprintf chan "%d" x;
      List.iter (Printf.fprintf chan ",%d") xs
  in
  let print_code_names bits data = 
    IntMap.iter
      (fun k v ->
        let num_lst =
          if bits then BitSet.to_list (`Packed k) else [k]
        and cmp_lst =
          try let cmp = IntMap.find k a.comp_code in
              if bits then BitSet.to_list (`Packed cmp) else [cmp]
          with _ -> []
        in
        Printf.printf "\t%s -- {%a} -- comp:{%a}\n"
                      v pp_ilst num_lst pp_ilst cmp_lst)
      data;
  in
  match a.alphabet_type with
  | Continuous      ->
    Printf.printf "Continuous\n%!";
    ()
  | Sequential ->
    Printf.printf "Sequential :\n%!";
    Printf.printf "Size: %d, Gap:%s, Missing:%s, All:%s\n%!" a.size
      (str_of_ioption a.gap) (str_of_ioption a.missing) (str_of_ioption a.all);
    print_code_names false a.code_name;
    ()
  | ExtendedBitFlag ->
    Printf.printf "ExtendedBitFlag:\n%!";
    Printf.printf "Size: %d, Gap:%s, Missing:%s, All:%s\n%!" a.size
      (str_of_ioption a.gap) (str_of_ioption a.missing) (str_of_ioption a.all);
    print_code_names true a.code_name;
    ()
  | SimpleBitFlag   ->
    Printf.printf "SimpleBitFlag:\n%!";
    Printf.printf "Size: %d, Gap:%s, Missing:%s, All:%s\n%!" a.size
      (str_of_ioption a.gap) (str_of_ioption a.missing) (str_of_ioption a.all);
    print_code_names true a.code_name;
    ()
  | CombinationLevels l ->
    Printf.printf "Combination Level %d:\n%!" l;
    Printf.printf "Size: %d, Gap:%s, Missing:%s, All:%s\n%!" a.size
      (str_of_ioption a.gap) (str_of_ioption a.missing) (str_of_ioption a.all);
    IntMap.iter
      (fun k vset ->
        Printf.printf "\t%d -- {%a} -- comp:{%s}\n"
            k pp_ilst (IntSet.elements vset)
            (try string_of_int $ IntMap.find k a.comp_code with _ -> ""))
      a.comb_set;
    ()


(** {2 Error Module} *)

module Error = struct
  type t = [
    | `Missing_State_Sequential_Alphabet of int
    | `Missing_Name_Sequential_Alphabet of int
    | `Alphabet_Size_Expectation of int * int 
    | `Missing_Gap_Element of int
    | `Complement_Not_Transitive of int * int
    | `Polymorphisms_In_Continuous_Alphabet
    | `Unacceptable_Level_Argument of int
    | `Gap_Not_Atomic_BitFlag_Alphabet of int
    | `Unacceptable_Level_Argument of int
    | `Illegal_Character of string
    | `Illegal_Code of int
    | `Not_found
  ]

  let to_string = function
    | `Missing_State_Sequential_Alphabet x ->
      Printf.sprintf "Missing state %d in Sequential Alphabet" x
    | `Missing_Name_Sequential_Alphabet x ->
      Printf.sprintf "Missing name %d in Sequential Alphabet" x
    | `Alphabet_Size_Expectation (x,y) ->
      Printf.sprintf "Alphabet size expected to be %d, actually %d" x y
    | `Missing_Gap_Element x ->
      Printf.sprintf "Expected gap code %d in set of codes" x
    | `Complement_Not_Transitive (x,y) ->
      Printf.sprintf "Expected complement of %d and %d to be transitive." x y
    | `Polymorphisms_In_Continuous_Alphabet ->
      Printf.sprintf "Continuous Alphabets do not have polymorphisms"
    | `Unacceptable_Level_Argument x ->
      Printf.sprintf "Level argument is %d, should be > 0" x
    | `Gap_Not_Atomic_BitFlag_Alphabet x ->
      Printf.sprintf "Gap character %d is not atomic (multiple bits set)" x
    | `Illegal_Character str ->
      Printf.sprintf "Cannot find character '%s' in alphabet" str
    | `Illegal_Code i ->
      Printf.sprintf "Cannot find character %d in alphabet" i
    | `Not_found ->
      Printf.sprintf "Not Found"
end

exception Error of Error.t


(** {2 Constants *)

let default_gap = "-"

let default_missing = "?"

let default_orientation = "~"


(** {2 Combination Functions} *)

let generate_combinational_elements ~level codes =
  let incr r = incr r; !r in
  let cross_product map1 map2 k =
    IntMap.fold
      (fun _ v1 ->
        IntMap.fold
          (fun _ v2 -> IntMap.add (incr k) (IntSet.union v1 v2))
          map2)
      map1
      IntMap.empty
  and inverse map =
    IntMap.fold (fun k v -> IntSetMap.add v k) map IntSetMap.empty
  and merge_maps =
    IntMap.merge
      (fun _ v1 v2 -> match v1,v2 with
        | None, None | Some _, Some _ -> assert false
        | None, ((Some _) as x)
        | ((Some _) as x), None -> x)
  in
  let l = Array.make level IntMap.empty in
  l.(0) <- IntMap.fold (fun i _ a -> IntMap.add i (IntSet.singleton i) a)
                       codes IntMap.empty;
  let code = ref (fst $ IntMap.max_binding l.(0)) in
  for i = 1 to level-1 do
    l.(i) <- cross_product l.(0) l.(i-1) code
  done;
  let comb_set = Array.fold_left merge_maps IntMap.empty l in
  comb_set, inverse comb_set

(** Tests any number of things to verify the integrity of an alphabet
    1 - that sequential alphabets are 0 indexed.
    2 - that the correct number of combinations exists.
    3 - the sequential alphabets don't skip numbers *)
let verify_alphabet a : unit = match a.alphabet_type with
  | Sequential ->
    let rec verify i =
      if i = a.size then
        ()
      else if IntMap.mem i a.code_name then begin
        if (StringMap.mem (IntMap.find i a.code_name) a.name_code)
          then (verify (i+1))
          else raise (Error (`Missing_Name_Sequential_Alphabet i))
      end else begin
        raise (Error (`Missing_State_Sequential_Alphabet i))
      end
    in
    let () =
      let a1 = IntMap.cardinal a.code_name
      and a2 = StringMap.cardinal a.name_code in
      if a1 = a2 && a1 = a.size
        then ()
        else
          raise (Error (`Alphabet_Size_Expectation (a.size,(a2+a1+a.size)-(2*a.size))))
    and () = verify 0 in
    ()
  | CombinationLevels _ -> ()
  | ExtendedBitFlag ->
    let is_atomic x = 0 = (x land (x-1)) in
    let () = match a.gap with
      | Some x when is_atomic x -> ()
      | None -> ()
      | Some x -> raise (Error (`Gap_Not_Atomic_BitFlag_Alphabet x))
    and () =
      let at =IntMap.cardinal a.code_name in
      if a.size = at
        then ()
        else raise (Error (`Alphabet_Size_Expectation (a.size,at)))
    in
    ()
  | SimpleBitFlag
  | Continuous -> ()

let of_list lst gap all missing alphabet_type orientation =
  let add_one (cname,ncode,ccode) (name,code,comp) =
    let name = String.uppercase name in
    let ccode = match comp with
      | None -> ccode
      | Some x -> IntMap.add code x ccode
    in
    StringMap.add name code cname, IntMap.add code name ncode, ccode
  in
  let name_code,code_name,comp_code =
    List.fold_left add_one (StringMap.empty,IntMap.empty,IntMap.empty) lst
  in
  let comb_set,set_comb = match alphabet_type with
    | CombinationLevels level      -> generate_combinational_elements ~level code_name
    | Sequential | SimpleBitFlag
    | Continuous | ExtendedBitFlag -> IntMap.empty, IntSetMap.empty
  in
  let () = match gap with
    | None -> ()
    | Some x when IntMap.mem x code_name -> ()
    | Some x -> raise (Error (`Missing_Gap_Element x))
  in
  let a = {
    size = IntMap.cardinal code_name;
    full_size = IntMap.cardinal comb_set;
    alphabet_type; orientation;
    code_name; name_code;
    comb_set; set_comb;
    comp_code;
    all; gap; missing;
  } in
  let () = verify_alphabet a in
  a


(** {2 Basic Alphabets} *)

let continuous =
  { alphabet_type = Continuous;
    comb_set = IntMap.empty;
    set_comb = IntSetMap.empty;
    name_code = StringMap.empty;
    code_name = IntMap.empty;
    comp_code = IntMap.empty;
    size      = max_int;
    full_size = ~-1; (* no matrix exists *)
    gap       = None;
    missing   = None;
    all       = None;
    orientation = false;
  }

let present_absent =
  let lst = [("present", 1, None); ("absent", 0, None) ] in
  of_list lst (Some 0) None None Sequential false

let dna =
  let lst = [
    ("A", 0b00001, Some 0b01000);
    ("C", 0b00010, Some 0b00100);
    ("G", 0b00100, Some 0b00010);
    ("T", 0b01000, Some 0b00001);
    (default_gap, 0b10000, None);
    ("X", 0b11111, Some 0b11111); ]
  in
  of_list lst (Some 0b10000) (Some 0b11111) (Some 0b10000) SimpleBitFlag false

let nucleotides =
  let lst = [ 
    ("A", 0b00001, Some 0b01000);
    ("C", 0b00010, Some 0b00100);
    ("G", 0b00100, Some 0b00010);
    ("T", 0b01000, Some 0b00001);
    ("M", 0b00011, Some 0b01100); 
    ("R", 0b00101, Some 0b01010); 
    ("W", 0b01001, Some 0b01001);
    ("S", 0b00110, Some 0b00110);
    ("Y", 0b01010, Some 0b00101);
    ("K", 0b01100, Some 0b00011);
    ("V", 0b00111, Some 0b01110);
    ("H", 0b01011, Some 0b01101);
    ("D", 0b01101, Some 0b01011);
    ("B", 0b01110, Some 0b00111);
    ("N", 0b01111, Some 0b01111);
    ("X", 0b01111, Some 0b01111);
    (default_gap, 0b10000, None);
    ("1", 0b10001, Some 0b11000);
    ("2", 0b10010, Some 0b10100);
    ("3", 0b10011, Some 0b11100);
    ("4", 0b10100, Some 0b10010);
    ("5", 0b10101, Some 0b11010);
    ("6", 0b10110, Some 0b10110);
    ("7", 0b10111, Some 0b11110);
    ("8", 0b11000, Some 0b10001);
    ("9", 0b11001, Some 0b11001);
    ("0", 0b11010, Some 0b10101);
    ("E", 0b11011, Some 0b11101);
    ("F", 0b11100, Some 0b10011);
    ("I", 0b11101, Some 0b11011);
    ("J", 0b11110, Some 0b10111);
    ("P", 0b11111, Some 0b11111);
    (default_missing, 0b11111, Some 0b11111);
  ] in
  of_list lst (Some 0b10000) (Some 0b11111) (Some 0b11111) ExtendedBitFlag false

let aminoacids =
  let lst = [
    ("A",  0, None); (* alanine *)
    ("R",  1, None); (* arginine *)
    ("N",  2, None); (* asparagine *)
    ("D",  3, None); (* aspartic *)
    ("C",  4, None); (* cysteine *)
    ("Q",  5, None); (* glutamine *)
    ("E",  6, None); (* glutamic *)
    ("G",  7, None); (* glycine *)
    ("H",  8, None); (* histidine *)
    ("I",  9, None); (* isoleucine *)
    ("L", 10, None); (* leucine *)
    ("K", 11, None); (* lysine *)
    ("M", 12, None); (* methionine *)
    ("F", 13, None); (* phenylalanine *)
    ("P", 14, None); (* proline *)
    ("S", 15, None); (* serine *)
    ("T", 16, None); (* threonine *)
    ("W", 17, None); (* tryptophan *)
    ("Y", 18, None); (* tyrosine *)
    ("V", 19, None); (* valine *)
    ("X", 20, None); (* all element *)
    (default_gap, 21, None); (* gap *)
  ] in
  of_list lst (Some 21) (Some 20) (Some 20) Sequential false



(** {2 Basic Functions for querying alphabets *)

let get_gap t = match t.gap with
  | Some x -> x
  | None   -> raise (Error `Not_found)

let has_gap t = match t.gap with
  | Some _ -> true
  | None   -> false

let size t = t.size

let orientation t = t.orientation

let all_char t = t.all

let kind t = t.alphabet_type

let is_statebased t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> true
  | Continuous
  | ExtendedBitFlag
  | SimpleBitFlag -> false

let is_bitset t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential
  | Continuous -> false
  | ExtendedBitFlag
  | SimpleBitFlag -> true

let complement i t =
  if IntMap.mem i t.comp_code
    then Some (IntMap.find i t.comp_code)
    else None

let is_complement a b t =
  let is_complement a b t =
    if IntMap.mem a t.comp_code
      then ((IntMap.find a t.comp_code) = b)
      else false
  in
  let result = is_complement a b t in
  if result = is_complement b a t
    then result
    else raise (Error (`Complement_Not_Transitive (a,b)))

let get_combination i t : IntSet.t =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential    -> IntMap.find i t.comb_set
  | Continuous    -> IntSet.singleton i
  | ExtendedBitFlag
  | SimpleBitFlag -> BitSet.to_set (`Packed i)

let get_state_combination s t : int =
  match t.alphabet_type with
  | CombinationLevels _
  | Sequential -> IntSetMap.find s t.set_comb
  | Continuous ->
    if (IntSet.cardinal s) = 1
      then IntSet.choose s
      else raise (Error `Polymorphisms_In_Continuous_Alphabet)
  | ExtendedBitFlag
  | SimpleBitFlag    -> BitSet.to_packed (`Set s)

let get_code n t =
  try match t.alphabet_type with
    | Continuous -> int_of_string n
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential ->
      StringMap.find (String.uppercase n) t.name_code
  with Not_found -> raise (Error (`Illegal_Character n))

let get_name c t =
  try match t.alphabet_type with
    | Continuous -> string_of_int c
    | SimpleBitFlag | ExtendedBitFlag
    | CombinationLevels _ | Sequential -> IntMap.find c t.code_name
  with Not_found -> raise (Error (`Illegal_Code c))

let to_list t =
  StringMap.fold (fun k v lst -> (k,v,complement v t)::lst) t.name_code []


(** {2 Converting between types of alphabets *)

let rec to_sequential t =
  match t.alphabet_type with
  | Sequential
  | Continuous -> t
  | ExtendedBitFlag -> to_sequential (to_bitflag t)
  | SimpleBitFlag -> failwith "TODO"
  | CombinationLevels _ -> failwith "TODO"

and to_bitflag t =
  match t.alphabet_type with
  | Continuous
  | SimpleBitFlag -> t
  | CombinationLevels _ -> failwith "TODO"
  | ExtendedBitFlag ->
    let is_atomic x = 0 = (x land (x-1)) in
    let lst =
      IntMap.fold
        (fun code name lst ->
          if is_atomic code then (name,code,complement code t)::lst else lst)
        t.code_name
        []
    in
    of_list lst t.gap t.all t.missing SimpleBitFlag t.orientation
  | Sequential -> failwith "TODO"
    (* how to deal with large alphabets? *)
          
and simplify t =
  match t.alphabet_type with
  | Sequential
  | CombinationLevels _ -> to_sequential t
  | SimpleBitFlag 
  | ExtendedBitFlag -> to_bitflag t
  | Continuous -> t

and to_level level t =
  let () =
    if level > 0
      then ()
      else raise (Error (`Unacceptable_Level_Argument level))
  in
  match t.alphabet_type with
  | CombinationLevels _ when level = 1 -> to_sequential t
  | CombinationLevels l when level = l -> t
  | Sequential          when level = 1 -> t
  | CombinationLevels _                -> to_level level (to_sequential t)
  | Continuous
  | SimpleBitFlag
  | ExtendedBitFlag -> t
  | Sequential ->
    let combs,lsts = generate_combinational_elements ~level t.code_name in
    { t with
      comb_set = combs;
      set_comb = lsts;
      alphabet_type = CombinationLevels level; }

