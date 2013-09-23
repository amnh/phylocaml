open OUnit

let tests = [
  AlphabetTest.tests;
]

let () =
  ignore (OUnit.run_test_tt_main ("All" >::: tests))
