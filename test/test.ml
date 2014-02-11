open OUnit

(** Pure OCaml libraries *)
let tests = [
  AlphabetTest.tests;
  TreeTest.tests;
]

(** OCaml libraries with C backend *)
let run_tests_c = false (* Off for now *)
let tests_c = [
  MlmodelTest.tests;
  BitvectorTest.tests;
  SequenceTest.tests;
]

let () =
  let () = Random.self_init () in
  ignore (OUnit.run_test_tt_main ("OCaml" >::: tests));
  if run_tests_c then ignore (OUnit.run_test_tt_main ("OCaml<-->C" >::: tests_c));
