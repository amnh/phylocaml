open OUnit

let tests = [
  AlphabetTest.tests;
  BitvectorTest.tests;
  SequenceTest.tests;
  TreeTest.tests;
  MlmodelTest.tests;
]

let () =
  ignore (OUnit.run_test_tt_main ("All" >::: tests))
