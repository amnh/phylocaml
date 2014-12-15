open OUnit2
open TestInternal


module MockCostMatrix    = CostMatrixTest.MockCostMatrix
module MockFullAlignment = Alignment.FullAlignment (MockCostMatrix)
module MockUkkAlignment = Alignment.UkkAlignment (MockCostMatrix)

let generate_alphabet () =
  Alphabet.generate_seq_alphabet ((Random.int 3) + 3)

let generate_sequence a =
  let indel = false
  and gap = match a.Alphabet.gap with
    | None -> assert false
    | Some x -> x
  in
  Array.init (1 + (Random.int 200))
             (fun i -> if i = 0 then gap
                                else Alphabet.random ~indel a)

let sequence_of_string a str =
  Array.init
    (String.length str)
    (fun i -> flip Alphabet.get_code a @@ String.make 1 str.[i])

let print_sequence a x =
  Array.iter (fun i -> Printf.printf "%s" @@ try Alphabet.get_name i a 
                                             with _ -> string_of_int i) x

let assert_equal_align a x y =
  assert_equal_array "" (flip Alphabet.get_name a) (=) x y

let tests =
  "Alignment" >:::
    [
      "Full Alignment test of equal sequences of data" >:: 
        (fun cxt ->
          let () = set_random_seed cxt in 
          let a = generate_alphabet () in
          let x = generate_sequence a in
          let model = MockCostMatrix.create a in
          let mem = MockFullAlignment.create_mem model x x in
          let ()  = MockFullAlignment.fill mem model x x in
          let cst = MockFullAlignment.cost mem model x x in
          let () = assert_equal_int 0 cst in
          let x', x'', m = MockFullAlignment.alignments mem model x x in
          assert_equal_align a x m;
          assert_equal_align a x x';
          assert_equal_align a x x'');

      "Full Alignment test of two sequences that differ by one deletion" >:: 
        (fun cxt ->
          let x = sequence_of_string Alphabet.nucleotides "-ACCCCCCCTTG" in
          let y = sequence_of_string Alphabet.nucleotides "-ACCCCCCCCTTG" in
          let model = MockCostMatrix.create Alphabet.nucleotides in
          let mem = MockFullAlignment.create_mem model x y in
          let ()  = MockFullAlignment.fill mem model x y in
          let cst = MockFullAlignment.cost mem model x y in
          let x', y', m = MockFullAlignment.alignments mem model x y in
          assert_equal_int ((Array.length x)+1) (Array.length x');
          assert_equal_int (Array.length m) (Array.length x');
          assert_equal_int (Array.length x') (Array.length y');
          assert_equal_align Alphabet.nucleotides y y');

      "Ukkonen Alignment test of two sequences that differ by one substitution" >:: 
        (fun cxt ->
          let x = sequence_of_string Alphabet.nucleotides "-ACCCCCCCTTG" in
          let y = sequence_of_string Alphabet.nucleotides "-ACCCCCCCCTG" in
          let m = sequence_of_string Alphabet.nucleotides "-ACCCCCCCYTG" in
          let model = MockCostMatrix.create Alphabet.nucleotides in
          let mem = MockUkkAlignment.create_mem ~k:4 model x y in
          let ()  = MockUkkAlignment.fill mem model x y in
          let cst = MockUkkAlignment.cost mem model x y in
          let x', y', m' = MockUkkAlignment.alignments mem model x y in
          assert_equal_align Alphabet.nucleotides x x';
          assert_equal_align Alphabet.nucleotides m m';
          assert_equal_align Alphabet.nucleotides y y');

      "Ukkonen Alignment test of two sequences that differ by one deletion" >:: 
        (fun cxt ->
          let x = sequence_of_string Alphabet.nucleotides "-ACCCCCCCTTG" in
          let y = sequence_of_string Alphabet.nucleotides "-ACCCCCCCCTTG" in
          let model = MockCostMatrix.create Alphabet.nucleotides in
          let mem = MockUkkAlignment.create_mem ~k:4 model x y in
          let ()  = MockUkkAlignment.fill mem model x y in
          let cst = MockUkkAlignment.cost mem model x y in
          let x', y', m = MockUkkAlignment.alignments mem model x y in
          assert_equal_int ((Array.length x)+1) (Array.length x');
          assert_equal_int (Array.length m) (Array.length x');
          assert_equal_int (Array.length x') (Array.length y');
          assert_equal_align Alphabet.nucleotides y y');

      "Ukkonen Alignment test of two sequences with multiple consequtive indels" >:: 
        (fun cxt ->
          let x = sequence_of_string Alphabet.nucleotides "-ACCCTTTTCCTTG" in
          let y = sequence_of_string Alphabet.nucleotides "-ACCCCTTG" in
          let ya= sequence_of_string Alphabet.nucleotides "-ACCC----C-TTG" in
          let m = sequence_of_string Alphabet.nucleotides "-ACCC1111C4TTG" in
          let model = MockCostMatrix.create Alphabet.nucleotides in
          let mem = MockUkkAlignment.create_mem ~k:4 model x y in
          let ()  = MockUkkAlignment.fill mem model x y in
          let cst = MockUkkAlignment.cost mem model x y in
          let x', y', m' = MockUkkAlignment.alignments mem model x y in
          assert_equal_align Alphabet.nucleotides x x';
          assert_equal_align Alphabet.nucleotides m m';
          assert_equal_align Alphabet.nucleotides ya y');

      "Ukkonen Alignment test of equal sequences of data k = 1" >:: 
        (fun cxt ->
          let () = set_random_seed cxt in 
          let a = generate_alphabet () in
          let x = generate_sequence a in
          let model = MockCostMatrix.create a in
          let mem = MockUkkAlignment.create_mem ~k:1 model x x in
          let ()  = MockUkkAlignment.fill mem model x x in
          let cst = MockUkkAlignment.cost mem model x x in
          let () = assert_equal_int 0 cst in
          let x', x'', m = MockUkkAlignment.alignments mem model x x in
          assert_equal_align a x m;
          assert_equal_align a x x';
          assert_equal_align a x x'');
    ]
