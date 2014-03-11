IFDEF USE_EXTERNAL_LINKING THEN
include Phylocaml
END
include Internal

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

let assert_equal_int_list =
  assert_equal_list string_of_int (=)

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

let assert_equal_tree =
  let printer x =
    let b = Buffer.create 100 in
    let () = Tree.dump (Buffer.add_string b) x in
    Buffer.contents b
  and cmp a b = 0 = (Tree.compare a b) in
  OUnit2.assert_equal ~printer ~cmp

