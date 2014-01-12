open OUnit
open Phylocaml
open Internal

let dna = Alphabet.dna
let aminoacids = Alphabet.aminoacids

let tests =
  "Alphabet" >:::
    [
      "DNA Encoding" >::
        (fun () ->
          assert_equal "A" (Alphabet.get_name 1 dna);
          assert_equal "C" (Alphabet.get_name 2 dna);
          assert_equal "G" (Alphabet.get_name 4 dna);
          assert_equal "T" (Alphabet.get_name 8 dna);
          assert_equal "-" (Alphabet.get_name 16 dna));

      "Seq Alphabet Generation" >::
        (fun () ->
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
          ());

      "DNA/bits->Seq" >::
        (fun () ->
          let a = Alphabet.to_sequential dna in
          assert_equal "A" (Alphabet.get_name 0 a));

      "DNA/bits->Seq->level:2->bit" >::
        (fun () ->
          let a = Alphabet.to_sequential dna in
          let a = Alphabet.to_level 3 a in
          let a = Alphabet.to_bitflag a in
          assert_equal "A" (Alphabet.get_name 1 a));

      "Seq Alphabet -> level:2" >::
        (fun () ->
          let a = Alphabet.generate_seq_alphabet 35 in
          let a = Alphabet.to_level 2 a in
          let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 60 a)) in
          let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 70 a)) in
          let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 40 a)) in
          let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  5 a)) in
          let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 25 a)) in
          ());

      "Seq Alphabet -> level:3" >::
        (fun () ->
          let a = Alphabet.generate_seq_alphabet 35 in
          let a = Alphabet.to_level 3 a in
          let () = assert_equal 3 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 650 a)) in
          let () = assert_equal 3 (Alphabet.CodeSet.cardinal (Alphabet.get_combination 700 a)) in
          let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  70 a)) in
          let () = assert_equal 2 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  40 a)) in
          let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination   5 a)) in
          let () = assert_equal 1 (Alphabet.CodeSet.cardinal (Alphabet.get_combination  25 a)) in
          ());

      "code(name) = name(code)" >::
        (fun () ->
          List.iter
            (fun (x,c,_) ->
              let () = assert_equal (Alphabet.get_code x aminoacids) c in
              let () = assert_equal (Alphabet.get_name c aminoacids) x in
              ())
            (Alphabet.to_list aminoacids));

(*    "Duplicate elements" >::
        (fun () ->
          let states =
            List.map (fun k -> (Printf.sprintf "%02d" k,None))
                    ((0 -- 8) @ (0 -- 8))
          in
          try Alphabet.of_list ~states ~equates:[] ~kind:Alphabet.BitFlag
                      ~orientation:false ~case:false ~gap:None ~all:None ~missing:None
              |> ignore;
              false
          with | Alphabet.Error (`Illegal_Character _) -> true); *)
    ]

