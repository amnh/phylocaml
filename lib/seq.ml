open Internal

(** The Sequence module allows the interaction of encoded arrays of data for C
 * usage. The sequence can be thought of as dynamically expandable arrays,
 * perfect for unaligned data. They are also pooled through the garbage
 * collector for speed when allocating new sequences. *)

type s

type single = [ `Choose | `Max | `Min | `Random ]

external register : unit -> unit = "seq_CAML_register"

exception Invalid_Sequence of (string * string * int)

external c_reverse : (s -> s -> unit) = "seq_CAML_reverse"

external reverse_ip : (s -> unit) = "seq_CAML_reverse_ip"

external capacity : (s -> int) = "seq_CAML_get_cap"

external create : int -> s = "seq_CAML_create"

external copy : (s -> s -> unit) = "seq_CAML_copy"

external length : (s -> int) = "seq_CAML_length"

external get : (s -> int -> int) = "seq_CAML_get"

external count : (int -> int -> s -> int ) = "seq_CAML_count"

external set : (s -> int -> int -> unit) = "seq_CAML_set"

external prepend : s -> int -> unit = "seq_CAML_prepend"

let () = register ()

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

let resize n ns =
  let v = create ns in
  copy !n v;
  n := v;
  ()

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

let of_list l =
  let rec aux_of_list seq l it = match l with
    | []   -> seq
    | h::t ->
      set seq it h;
      aux_of_list seq t (it + 1);
  in
  let length = List.length l in
  let seq = create length in
  aux_of_list seq l 0

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
    with Alphabet.Illegal_Character _ ->
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

let of_list str_ls alph =
  let rec aux_parse_ls seq = function
    | []     -> seq
    | hd::tl ->
      prepend seq $ Alphabet.get_code hd alph;
      aux_parse_ls seq tl
  in
  let len = List.length str_ls in
  let seq = create len in
  aux_parse_ls seq str_ls

let to_string seq alph =
  let len = length seq in
  let seq,len =
    if len=0
      then (missing alph),1
      else seq,len
    in
    let b = Buffer.create len in
    for i = 0 to len - 1 do
        Buffer.add_string b $ Alphabet.get_name (get seq i) alph;
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
  Pervasives.output_string chn $ to_string seq alph

let print_codes seq =
  let len = length seq in
  Printf.printf "len=%d,[%!" len;
  for i = 0 to (len-1) do
    Printf.printf "%d," (get seq i)
  done;
  Printf.printf "]\n%!"

let concat x =
  let copy_from_in x y z u =
    let to_copy = get x z in
    set y u to_copy
  in
  let len = List.fold_left (fun x y -> x + length y) 0 x in
  let ns = init (fun _x -> 0) len in
  let pos = ref 0 in
  let copier x =
    let len = length x in
    for i = 0 to len - 1 do
      copy_from_in x ns i !pos;
      Pervasives.incr pos;
  done;
  in
  List.iter (copier) x;
  ns

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

let length_without_gap gap s =
  let count = ref 0 in
  let slen = length s in
  for i = 0 to slen-1 do
    let tmp = get s i in
    if tmp<>gap then count := !count + 1
  done;
  !count

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
    | `Min    -> (fun x -> IntSet.min_elt $ Alphabet.get_combination x alph)
    | `Max    -> (fun x -> IntSet.max_elt $ Alphabet.get_combination x alph)
    | `Random -> failwith "TODO"
    | `Choose -> (fun x -> IntSet.choose  $ Alphabet.get_combination x alph)
  in
  map selects s

let split positions s alph =
  let gap = Alphabet.get_gap alph in
  let len = (length s) - 1 in
  let positions = (0, 0) :: positions in
  let do_one_pair a b acc =
    let first = a
    and last = b in
    let total = 1 + (last - first) in
    let seq = create (total + 1)  in
    for i = (last - 1) downto first do
      prepend seq (get s i);
    done;
    if first <> 0 then prepend seq gap;
    seq :: acc
  in
  let rec splitter acc = function
    | (a, _) :: (((c, b) :: _) as t) ->
      assert ( a <= b );
      splitter (do_one_pair a c acc) t
    | (a, _) :: [] ->
      List.rev (do_one_pair a (len + 1) acc)
    | [] -> []
  in
  splitter [] positions

let of_code_arr code_arr gap =
  let num_nus =
    Array.fold_left
      (fun num_nus code ->
        if (code = gap) || (code = 0)
          then num_nus
          else num_nus + 1)
      0
      code_arr
  in
  let seq = init (fun _ -> gap) num_nus in
  let _ =
    Array.fold_left
      (fun num_nus code ->
        if (code=gap) || (code = 0)
          then num_nus
          else begin
            set seq num_nus code;
            num_nus + 1
          end)
      0
      code_arr
  in
  seq

let complement a s =
  let aux_complement start a s =
    let res =
      let acc = (create (length s)) in
      for i = start to (length s) - 1 do
        match Alphabet.complement (get s i) a with
        | Some x -> prepend acc x
        | None -> failwith "I can't complement this alphabet"
      done;
      acc
    in
    res
  in
  let gap = Alphabet.get_gap a in
  let res = aux_complement 1 a s in
  prepend res gap;
  res

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
          if n = (IntSet.cardinal $ Alphabet.get_combination base alph)
            then acc + 1
            else acc)
        0
        sequence
    else
      failwith "Cannot run polymorphic saturation on continuous data"
  in
  proportion poly len

let concat (seq_ls : s list) =
  let total_len = List.fold_left (fun acc x -> acc + length x) 0 seq_ls in
  let concat_seq = create total_len in
  let concat_pos = ref 0 in
  let copier seq =
    let len = length seq in
    for pos = 0 to len - 1 do
      set concat_seq !concat_pos (get seq pos);
	  concat_pos := !concat_pos + 1
    done;
  in
  List.iter copier seq_ls;
  concat_seq

let unique_elements seq alph =
  let len = length seq in
  let rec check_char p acc =
    if p = len then
      true
    else begin
      let s : IntSet.t = Alphabet.get_combination (get seq p) alph in
      if IntSet.is_empty $ IntSet.inter s acc then
        check_char (p+1) $ IntSet.union s acc
      else
        false
    end
  in
  check_char 0 IntSet.empty
