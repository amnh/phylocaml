open OUnit
open Phylocaml
open Internal

let a = Alphabet.dna

let s1_1 = [| 16;1;4;4;8;2;4;8;2;2;4;8;2;4;4;1;1;8;16;16;1 |]
let s1_2 = "-AGGTCGTCCGTCGGAAT--A"
let s1_3 = [16;1;4;4;8;2;4;8;2;2;4;8;2;4;4;1;1;8;16;16;1]
let s1_4 = ["-";"A";"G";"G";"T";"C";"G";"T";"C";"C";"G";"T";"C";"G";"G";"A";"A";"T";"-";"-";"A"]

let s2 = [|  2;2;4;8;2;4;4;1;1;8;16;16;16 |]
let s3 = [| 16;1;4;4;8;2;4;8;2;2;4;8;2;4;4;1;1;8;16;16; 1 |]
let s4 = [|  8;8;8;8;8;2;4;8;2;2;4;8;2;4;4;1;1;8;16;16;16 |]

let assert_equal_seq a b =
  let cmp a b = 0 = (Sequence.compare a b) in
  assert_equal ~cmp ~printer:Sequence.to_raw_string a b

let test_0 () =
  let s1_1 = Sequence.of_array s1_1 in
  let s1_2 = Sequence.of_string s1_2 a in
  let s1_3 = Sequence.of_list s1_3 in
  let s1_4 = Sequence.of_state_list s1_4 a in
  assert_equal_seq s1_1 s1_2;
  assert_equal_seq s1_1 s1_3;
  assert_equal_seq s1_1 s1_4;
  ()

let test_1 () =
  let s1 = Sequence.of_array s1_1 and s2 = Sequence.of_array s2
  and s3 = Sequence.of_array s3 and s4 = Sequence.of_array s4 in
  assert_equal_seq (Sequence.reverse (Sequence.reverse s1)) s1;
  assert_equal_seq (Sequence.reverse (Sequence.reverse s2)) s2;
  assert_equal_seq (Sequence.reverse (Sequence.reverse s3)) s3;
  assert_equal_seq (Sequence.reverse (Sequence.reverse s4)) s4;
  ()

let test_2 () =
  let s = Sequence.of_array s4 in
  let s_len = Sequence.length s in
  assert_equal (s_len-1) (Sequence.length (Sequence.del_first_char s));
  assert_equal (s_len) (Sequence.length s);
  assert_equal (s_len+1) (Sequence.length (Sequence.prepend_char s 16));
  assert_equal (s_len) (Sequence.length s);
  ()

let test_3 () =
  let s2 = Sequence.of_array s2 and s4 = Sequence.of_array s4 in
  let s2_len = Sequence.length s2 and s4_len = Sequence.length s4 in
  assert_equal (s2_len+s4_len) (Sequence.length (Sequence.concat [s2;s4]));
  ()

let test_4 () =
  let s = Sequence.of_array s1_1 in
  let s_clone = Sequence.clone s in
  assert_equal_seq s s_clone;
  assert_raises Sequence.ReachedCapacity (fun () -> Sequence.prepend s_clone 16);
  ()

let test_5 () = 
  let s = Sequence.of_array s1_1 in
  let ss= Sequence.split [0;20;15;] s in
  assert_equal_seq s (Sequence.concat ss);
  ()

let local_tests =
  [
    "Sequence generation functions"  >:: test_0;
    "Sequence reverse/compare"       >:: test_1;
    "Remove/Append characters"       >:: test_2;
    "Sequence concat"                >:: test_3;
    "Sequence prepend at capacity"   >:: test_4;
    "Sequence concat/split"          >:: test_5;
  ]

let tests = "Sequence" >::: local_tests

