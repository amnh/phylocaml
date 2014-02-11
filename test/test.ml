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

(* Command Line Options *)

let seed = ref (truncate (Unix.time ()))
let mult = ref 1

let seedopt = Conf.make_int "seed" (truncate (Unix.time ())) "Set the random seed"
let multopt = Conf.make_int "mult" 1 "Multiply tests by value"

let () =
  Printf.printf "Setting random seed : %d\n%!" !seed;
  let () = Random.init !seed in
  ignore (run_test_tt_main ("OCaml" >::: (multiply_list !mult tests)));
  if run_tests_c then
    ignore (run_test_tt_main ("OCaml<-->C" >::: ((multiply_list !mult tests_c))));
  ()

