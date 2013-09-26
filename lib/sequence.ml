open Internal

let safe = true

type s

type elt = int

type single = [ `Choose | `Max | `Min | `Random ]

external register : unit -> unit = "seq_CAML_register"

exception Invalid_Sequence of (string * string * int)

exception ReachedCapacity

exception Invalid_Base of elt

external c_reverse : (s -> s -> unit) = "seq_CAML_reverse"

external reverse_ip : (s -> unit) = "seq_CAML_reverse_ip"

external capacity : (s -> int) = "seq_CAML_get_cap"

external create : int -> s = "seq_CAML_create"

external copy : (s -> s -> unit) = "seq_CAML_copy"

external length : (s -> int) = "seq_CAML_length"

external get : (s -> int -> elt) = "seq_CAML_get"

(* external count : (int -> int -> s -> int) = "seq_CAML_count" *)

external set : (s -> int -> elt -> unit) = "seq_CAML_set"

external prepend : s -> elt -> unit = "seq_CAML_prepend"

let () = register ()

let safe_prepend seq x =
  if (capacity seq) > (length seq) then
    prepend seq x
  else
    raise ReachedCapacity

let prepend =
  if safe then safe_prepend else prepend

let missing a =
  let s = create 1 in
  prepend s (Alphabet.get_gap a);
  s

let mapi f s =
  let len = length s in
  let new_s = create len in
  for i = len - 1 downto 0 do
    prepend new_s (f (get s i) i);
  done;
  new_s

let map f s =
  let len = length s in
  let new_s = create len in
  for i = len - 1 downto 0 do
    prepend new_s (f (get s i));
  done;
  new_s

let fold_left f acc s =
  let len = length s in
  let rec folder acc pos =
    if pos < len
      then folder (f acc (get s pos)) (pos + 1)
      else acc
  in
  folder acc 0

let foldi_left f acc s =
  let len = length s in
  let rec folder acc pos =
    if pos < len
      then folder (f acc pos (get s pos)) (pos + 1)
      else acc
  in
  folder acc 0

let foldi_left_2 f acc s1 s2 =
  assert( (length s1) = (length s2) );
  let len = length s1 in
  let rec folder acc pos =
    if pos < len
      then folder (f acc pos (get s1 pos) (get s2 pos)) (pos + 1)
      else acc
  in
  folder acc 0

let fold_right f acc s =
  let len = length s in
  let rec folder acc pos =
    if pos > (-1)
      then folder (f (get s pos) acc) (pos - 1)
      else acc
  in
  folder acc (len - 1)

let foldi_right f acc s =
  let len = length s in
  let rec folder acc pos =
    if pos > (-1)
      then folder (f pos (get s pos) acc) (pos - 1)
      else acc
  in
  folder acc (len - 1)

let iter f s =
  let len = length s in
  for i = len - 1 downto 0 do
    f (get s i);
  done;
  ()

let init f len =
  let seq = create len in
  for i = len - 1 downto 0 do
    prepend seq (f i);
  done;
  seq

(* unused currently
let resize s ns =
  let v = create ns in
  copy !s v;
  n := v;
  ()
*)

let clone n =
  let sz = length n in
  let res = create sz in
  copy n res;
  res

let reverse s1 =
  let sp = create (length s1) in
  c_reverse s1 sp;
  sp

let safe_reverse x =
  let gap = get x 0 in
  let y = create (length x) in
  for i = 1 to (length x) - 1 do
    prepend y (get x i);
  done;
  prepend y gap;
  y

let reverse =
  if safe then safe_reverse else reverse

let of_list l =
  let seq = create (List.length l) in
  List.iter (fun x -> prepend seq x) (List.rev l);
  seq

let remove_base ?(prependbase=true) s gapcode =
  let remove_gap gap base seq =
    let () = if base <> gap then prepend seq base else () in
    seq
  in
  let res = fold_right (remove_gap gapcode) (create (length s)) s in
  if prependbase then prepend res gapcode;
  res

let is_missing seq alph =
  let gap = Alphabet.get_gap alph in
  let len = length seq in
  if len=0 then true
  else
    let rec check p =
      if p = len
        then true
        else
          if gap <> get seq p
            then false
            else check (p + 1)
    in
    check 0


(** {2 Parsers to Sequence data-type} *)

let of_string str alph =
  let rec get_base cur_str loc =
    let ch = Char.escaped str.[loc] in
    let new_str = ch ^ cur_str in
    try (Alphabet.get_code new_str alph,loc)
    with Alphabet.Error (`Illegal_Character _) ->
      if loc = 0
        then raise (Invalid_Sequence (str, new_str, loc))
        else get_base new_str (loc-1)
  and aux_parse seq alph = function
    | -1 -> seq
    | loc ->
      let base,loc = get_base "" loc in
      prepend seq base;
      aux_parse seq alph (loc-1)
  in
  let len = String.length str in
  let seq = create len in
  aux_parse seq alph (len - 1)

let of_state_list str_ls alph =
  List.fold_right
    (fun x seq -> let () = prepend seq @@ Alphabet.get_code x alph in seq)
    (str_ls)
    (create (List.length str_ls))

let to_string seq alph =
  let len = length seq in
  let seq,len =
    if len=0
      then (missing alph),1
      else seq,len
    in
    let b = Buffer.create len in
    for i = 0 to len - 1 do
        Buffer.add_string b @@ Alphabet.get_name (get seq i) alph;
    done;
    Buffer.contents b

let to_raw_string seq =
  let len = length seq in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    Buffer.add_string b @@ (string_of_int (get seq i))^" ";
  done;
  Buffer.contents b


let to_formater seq alph =
  let len = length seq in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let str =
      let v = Alphabet.get_name (get seq i) alph in
      let v = Str.global_replace (Str.regexp "@") "@@" v in
      let v = Str.global_replace (Str.regexp "%") "%%" v in
      v
    in
    Buffer.add_string b str
  done;
  Buffer.contents b

let print chn seq alph =
  Pervasives.output_string chn @@ to_string seq alph

let print_codes seq =
  let len = length seq in
  Printf.printf "len=%d,[%!" len;
  for i = 0 to (len-1) do
    Printf.printf "%d," (get seq i)
  done;
  Printf.printf "]\n%!"

let to_array s =
  Array.init (length s) (fun x -> get s x)

let of_array code_arr =
  let len = Array.length code_arr in
  init (fun idx -> code_arr.(idx)) len

let sub s st len =
  init (fun x -> get s (x + st)) len

let sub_ignore_base base s st len =
  let idx = ref st in
  let count = ref 0 in
  let slen = length s in
  let acclst = ref [] in
  while (!count < len) do
    assert( !idx < slen);
    let add = get s !idx in
    acclst := !acclst @ [add];
    idx := !idx +1;
    if add<>base then
      count := !count + 1
  done;
  of_array (Array.of_list !acclst) , !idx

(*
let length_without_gap gap s =
  let count = ref 0 in
  let slen = length s in
  for i = 0 to slen-1 do
    let tmp = get s i in
    if tmp<>gap then count := !count + 1
  done;
  !count
*)

let prepend_char seq element =
  let len = length seq in
  let ext_seq =
    init
      (fun index -> match  index with
        | 0  -> element
        | _ -> get seq (index - 1))
      (len + 1)
  in
  ext_seq

let del_first_char seq =
  let len = length seq in
  sub seq 1 (len - 1)

let compare a b =
  let la = length a
  and lb = length b in
  let lc = min la lb in
  let rec comparator cnt =
    if cnt < lc then begin
      let ca = get a cnt
      and cb = get b cnt in
      match ca - cb with
        | 0 -> comparator (cnt + 1)
        | v -> v
    end else
      0
  in
  match comparator 0 with
  | 0 -> la - lb
  | v -> v

let select_one ?(how=`Min) s alph =
  let selects = match how with
    | `Min    -> (fun x -> Alphabet.CodeSet.min_elt @@ Alphabet.get_combination x alph)
    | `Max    -> (fun x -> Alphabet.CodeSet.max_elt @@ Alphabet.get_combination x alph)
    | `Random -> failwith "TODO"
    | `Choose -> (fun x -> Alphabet.CodeSet.choose  @@ Alphabet.get_combination x alph)
  in
  map selects s

let split positions s =
  let len = (length s) - 1 in
  let positions =
    match List.sort Pervasives.compare positions with
    | (0::_) as xs -> xs
    | xs -> 0 :: xs
  in
  let do_one_pair a b acc =
    let first = a
    and last = b in
    let total = 1 + (last - first) in
    let seq = create (total + 1)  in
    for i = (last - 1) downto first do
      prepend seq (get s i);
    done;
    seq :: acc
  in
  let rec splitter acc = function
    | a :: ((c :: _) as t) ->
      splitter (do_one_pair a c acc) t
    | a :: [] ->
      List.rev (do_one_pair a (len + 1) acc)
    | [] -> []
  in
  splitter [] positions

let complement a s =
  let aux_complement start a s =
    let res =
      let acc = (create (length s)) in
      for i = start to (length s) - 1 do
        let y = get s i in
        match Alphabet.complement y a with
        | Some y -> prepend acc y
        | None   -> raise (Invalid_Base y)
      done;
      acc
    in
    res
  in
  let res = aux_complement 1 a s in
  prepend res (Alphabet.get_gap a);
  res

(*
let contains_code code seq =
  fold_left (fun existed c -> if c = code then true else existed) false seq

let cmp_num_all seq alph =
  let len = length seq in
  let gap = Alphabet.get_gap alph in
  let num_nu = ref 0 in
  for p = 0 to len - 1 do
    if (get seq p) land gap != gap then num_nu := !num_nu + 1;
  done;
  !num_nu

let cmp_num_not_gap seq alph =
  let len = length seq in
  let gap = Alphabet.get_gap alph in
  let num_nu = ref 0 in
  for p = 0 to len - 1 do
    if (get seq p) != gap then num_nu := !num_nu + 1;
  done;
  !num_nu
*)

let gap_saturation seq alph =
  assert( Alphabet.is_bitset alph );
  let gap = Alphabet.get_gap alph in
  let len = length seq
  and gaps =
    fold_left (fun acc base -> if 0 <> base land gap then acc + 1 else acc) 0 seq
  in
  proportion gaps len

let poly_saturation sequence alph n =
  let len = length sequence
  and poly =
    if Alphabet.is_bitset alph then
      fold_left
        (fun acc base ->
          if n = BitSet.size (`Packed base) then acc + 1 else acc) 0 sequence
    else if Alphabet.is_statebased alph then
      fold_left
        (fun acc base ->
          if n = (Alphabet.CodeSet.cardinal @@ Alphabet.get_combination base alph)
            then acc + 1
            else acc)
        0
        sequence
    else
      failwith "Cannot run polymorphic saturation on this data"
  in
  proportion poly len



let concat x =
  let len = List.fold_left (fun x y -> x + length y) 0 x in
  let ns = init (fun _ -> 0) len in
  let pos = ref 0 in
  let copier x =
    let len = length x in
    for i = 0 to len - 1 do
      set ns !pos  (get x i);
      Pervasives.incr pos;
  done;
  in
  List.iter (copier) x;
  ns


let unique_elements seq alph =
  let len = length seq in
  let rec check_char p acc =
    if p = len then
      true
    else begin
      let s = Alphabet.get_combination (get seq p) alph in
      if Alphabet.CodeSet.is_empty @@ Alphabet.CodeSet.inter s acc then
        check_char (p+1) @@ Alphabet.CodeSet.union s acc
      else
        false
    end
  in
  check_char 0 Alphabet.CodeSet.empty
