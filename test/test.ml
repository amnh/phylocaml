open OUnit


(** Pure OCaml libraries *)
let tests = [
  AlphabetTest.tests;
  TreeTest.tests;
]

(** OCaml libraries with C backend *)
let run_tests_c = false
let tests_c = [
  MlmodelTest.tests;
  BitvectorTest.tests;
  SequenceTest.tests;
]

let () =
  ignore (OUnit.run_test_tt_main ("OCaml" >::: tests));
  if run_tests_c then ignore (OUnit.run_test_tt_main ("OCaml<-->C" >::: tests_c));
