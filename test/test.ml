open OUnit2
open TestInternal

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
  let seed = truncate (Unix.time ()) in
  Printf.printf "Setting random seed : %d\n%!" seed;
  let () = Random.init seed in
  ignore (run_test_tt_main ("OCaml" >::: tests));
  if run_tests_c then
    ignore (run_test_tt_main ("C" >::: tests_c));
  ()

