open OUnit
open Internal

let dna = Alphabet.dna
let aminoacids = Alphabet.aminoacids

let test_1 () =
  assert_equal "A" (Alphabet.get_name 1 dna)

let test_2 () =
  let a = Alphabet.generate_seq_alphabet 25 in
  let () = assert_equal "00" (Alphabet.get_name  0 a) in
  let () = assert_equal "01" (Alphabet.get_name  1 a) in
  let () = assert_equal "02" (Alphabet.get_name  2 a) in
  let () = assert_equal "03" (Alphabet.get_name  3 a) in
  let () = assert_equal "04" (Alphabet.get_name  4 a) in
  let () = assert_equal "10" (Alphabet.get_name 10 a) in
  let () = assert_equal "11" (Alphabet.get_name 11 a) in
  let () = assert_equal "12" (Alphabet.get_name 12 a) in
  let () = assert_equal "13" (Alphabet.get_name 13 a) in
  let () = assert_equal "24" (Alphabet.get_name 24 a) in
  let () = assert_equal "-"  (Alphabet.get_name 25 a) in
  ()

let test_3 () =
  let a = Alphabet.to_sequential dna in
  assert_equal "A" (Alphabet.get_name 0 a)

let test_4 () =
  let a = Alphabet.to_sequential dna in
  let a = Alphabet.to_level 3 a in
  let a = Alphabet.to_bitflag a in
  assert_equal "A" (Alphabet.get_name 1 a)

let test_5 () =
  let a = Alphabet.generate_seq_alphabet 35 in
  let a = Alphabet.to_level 2 a in
  let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 60 a)) in
  let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 70 a)) in
  let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 40 a)) in
  let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  5 a)) in
  let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 25 a)) in
  ()

let test_6 () =
  let a = Alphabet.generate_seq_alphabet 35 in
  let a = Alphabet.to_level 3 a in
  let () = assert_equal 3 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 650 a)) in
  let () = assert_equal 3 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 700 a)) in
  let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  70 a)) in
  let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  40 a)) in
  let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination   5 a)) in
  let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  25 a)) in
  ()

let test_7 () =
  List.iter
    (fun (x,c,_) ->
      let () = assert_equal (Alphabet.get_code x aminoacids) c in
      let () = assert_equal (Alphabet.get_name c aminoacids) x in
      ())
    (Alphabet.to_list aminoacids)

let test_8 () =
  let states =
    List.map (fun k -> (Printf.sprintf "%02d" k,None))
             ((0 -- 8) @ (0 -- 8))
  in
  try Alphabet.of_list ~states ~equates:[] ~kind:Alphabet.BitFlag
              ~orientation:false ~case:false ~gap:None ~all:None ~missing:None
      |> ignore;
      false
  with | Alphabet.Error (`Illegal_Character _) -> true

let local_tests = 
  [
    "DNA Encoding"                >:: test_1;
    "Seq Alphabet Generation"     >:: test_2;
    "DNA/bits->Seq"               >:: test_3;
    "DNA/bits->Seq->level:2->bit" >:: test_4;
    "Seq Alphabet -> level:2"     >:: test_5;
(*     "Seq Alphabet -> level:3"     >:: test_6; *)
    "code(name) = name(code)"     >:: test_7;
(*     "Duplicate elements"          >:: test_8; *)
  ]

let tests = "Alphabet" >::: local_tests

