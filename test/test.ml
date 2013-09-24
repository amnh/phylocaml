open OUnit

let tests = [
  AlphabetTest.tests;
  TreeTest.tests;
]

let () =
  ignore (OUnit.run_test_tt_main ("All" >::: tests))
