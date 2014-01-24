IFDEF USE_EXTERNAL_LINKING THEN
include Phylocaml
END
include Internal

let assert_equal_int =
  let printer = string_of_int in
  OUnit.assert_equal ~printer

let assert_equal_num =
  let printer = Num.string_of_num in
  OUnit.assert_equal ~printer
