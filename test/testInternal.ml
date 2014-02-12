IFDEF USE_EXTERNAL_LINKING THEN
include Phylocaml
END
include Internal

let assert_equal_int =
  let printer = string_of_int in
  OUnit.assert_equal ~printer

let assert_equal_ids =
  let printer set = "{|" ^ (IntSet.fold (fun x acc -> (string_of_int x)^"|"^acc) set "}")
  and cmp a b = IntSet.is_empty @@ IntSet.diff a b in
  OUnit.assert_equal ~printer ~cmp

let assert_equal_num =
  let printer = Num.string_of_num in
  OUnit.assert_equal ~printer

let assert_equal_tree =
  let printer x =
    let b = Buffer.create 100 in
    let () = Tree.dump (Buffer.add_string b) x in
    Buffer.contents b
  and cmp a b = 0 = (Tree.compare a b) in
  OUnit.assert_equal ~printer ~cmp

