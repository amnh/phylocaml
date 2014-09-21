open OUnit2

(** Pure OCaml libraries *)
let run_tests_ml = true
let tests = [
  AlphabetTest.tests;
  TreeTest.tests;
  TopologyTest.tests;
  CostMatrixTest.tests;
  (*AlignmentTest.tests;*)
  InternalTest.tests;
]

(** OCaml libraries with C backend *)
let run_tests_c = false (* Off for now *)
let tests_c = [
  MlModelTest.tests;
  BitvectorTest.tests;
  SequenceTest.tests;
]

let () =
  if run_tests_ml then ignore (run_test_tt_main ("OCaml" >::: tests));
  if run_tests_c  then ignore (run_test_tt_main ("C" >::: tests_c));
  ()

