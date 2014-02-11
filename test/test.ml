open OUnit
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

let usage = Printf.sprintf "%s [OPTIONS]" (Filename.basename Sys.executable_name)

let seed = ref (truncate (Unix.time ()))
let mult = ref 1

let options =
  [ "-seed", Arg.Int (fun x -> seed := x), "Set random seed for tests";
    "-mult", Arg.Int (fun x -> mult := x), "Multiple runs for random tests"; ]

let () =
  Arg.parse (Arg.align options) ignore usage;
  Printf.printf "Setting random seed : %d\n%!" !seed;
  let () = Random.init !seed in
  ignore (OUnit.run_test_tt_main ("OCaml" >::: (multiply_list !mult tests)));
  if run_tests_c then
    ignore (OUnit.run_test_tt_main ("OCaml<-->C" >::: ((multiply_list !mult tests_c))));
  ()

