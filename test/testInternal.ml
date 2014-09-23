IFDEF USE_EXTERNAL_LINKING THEN
include Phylocaml
END
include Internal

let random_seed =
  OUnit2.Conf.make_string_opt "random_seed" None "Random seed applied to each test"

let set_random_seed ctxt = 
  begin match random_seed ctxt with
    | None   -> truncate @@ Unix.time ()
    | Some x -> int_of_string x
  end
    |> Random.init

let assert_equal_list elt_printer elt_compare =
  let printer xs = 
    let b = Buffer.create 10 in
    List.iter (fun x -> Buffer.add_string b " ";
                        Buffer.add_string b (elt_printer x))
              xs;
    Buffer.contents b
  and cmp x y =
    let msg = "List length not equal" and printer = string_of_int in
    OUnit2.assert_equal ~msg ~printer (List.length x) (List.length y);
    List.fold_left2 (fun acc a b -> acc && elt_compare a b) true x y
  in
  OUnit2.assert_equal ~printer ~cmp

let assert_equal_array sep elt_printer elt_compare =
  let printer xs = 
    let b = Buffer.create 10 in
    Array.iter (fun x -> Buffer.add_string b sep;
                         Buffer.add_string b (elt_printer x))
              xs;
    Buffer.contents b
  and cmp x y =
    let msg = "List length not equal" and printer = string_of_int in
    OUnit2.assert_equal ~msg ~printer (Array.length x) (Array.length y);
    array_fold_left2 (fun acc a b -> acc && elt_compare a b) true x y
  in
  OUnit2.assert_equal ~printer ~cmp

let assert_equal_int_list =
  assert_equal_list string_of_int (=)

let assert_equal_str =
  let printer = Internal.id in
  OUnit2.assert_equal ~printer

let assert_equal_int =
  let printer = string_of_int in
  OUnit2.assert_equal ~printer

let assert_equal_ids =
  let printer set = "{|" ^ (IntSet.fold (fun x acc -> (string_of_int x)^"|"^acc) set "}")
  and cmp a b = IntSet.is_empty @@ IntSet.diff a b in
  OUnit2.assert_equal ~printer ~cmp

let assert_equal_num =
  let printer = Num.string_of_num in
  OUnit2.assert_equal ~printer

let assert_equal_tuple ?(container=("[","]")) ?(sep=",") to_string =
  let printer (a,b) = (fst container) ^ (to_string a) ^ sep ^ (to_string b) ^ (snd container)
  and cmp (a,b) (x,y) = a=x && b=y in
  OUnit2.assert_equal ~printer ~cmp
