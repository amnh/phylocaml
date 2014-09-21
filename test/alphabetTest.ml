open OUnit2
open TestInternal

open Alphabet

let dna = dna
let aminoacids = aminoacids

let tests =
  "Alphabet" >::: [

    "DNA Encoding" >::
    (fun _ctxt ->
      assert_equal_str "A" (get_name 1 dna);
      assert_equal_str "C" (get_name 2 dna);
      assert_equal_str "G" (get_name 4 dna);
      assert_equal_str "T" (get_name 8 dna);
      assert_equal_str "-" (get_name 16 dna));

    "Seq Alphabet Generation" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 25 in
      let () = assert_equal_str "00" (get_name  0 a) in
      let () = assert_equal_str "01" (get_name  1 a) in
      let () = assert_equal_str "02" (get_name  2 a) in
      let () = assert_equal_str "03" (get_name  3 a) in
      let () = assert_equal_str "04" (get_name  4 a) in
      let () = assert_equal_str "10" (get_name 10 a) in
      let () = assert_equal_str "11" (get_name 11 a) in
      let () = assert_equal_str "12" (get_name 12 a) in
      let () = assert_equal_str "13" (get_name 13 a) in
      let () = assert_equal_str "24" (get_name 24 a) in
      let () = assert_equal_str "-"  (get_name 25 a) in
      ());

    "DNA/bits->Seq" >::
    (fun _ctxt ->
      let a = to_sequential dna in
      assert_equal_int (CodeSet.cardinal a.atomic)
                   (StringMap.cardinal a.name_code);
      assert_equal_str "A" (get_name 0 a);
      assert_equal_str "C" (get_name 1 a);
      assert_equal_str "G" (get_name 2 a);
      assert_equal_str "T" (get_name 3 a);
      assert_equal_str "-" (get_name 4 a));

    "DNA/bits->Seq->level:2->bit" >::
    (fun _ctxt ->
      let a = to_sequential dna in
      assert_equal (CodeSet.cardinal a.atomic)
                   (StringMap.cardinal a.name_code);
      let a = to_level 3 a in
      let a = to_bitflag a in
      assert_equal "A" (get_name 1 a));

    "Seq Alphabet -> level:2" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 35 in
      let a = to_level 2 a in
      let () = assert_equal (CodeSet.cardinal a.atomic)
                            (StringMap.cardinal a.name_code) in
      let () = assert_equal_int 2 (CodeSet.cardinal (get_combination 60 a)) in
      let () = assert_equal_int 2 (CodeSet.cardinal (get_combination 70 a)) in
      let () = assert_equal_int 2 (CodeSet.cardinal (get_combination 40 a)) in
      let () = assert_equal_int 1 (CodeSet.cardinal (get_combination  5 a)) in
      let () = assert_equal_int 1 (CodeSet.cardinal (get_combination 25 a)) in
      ());

    "Seq Alphabet -> level:3" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 35 in
      let comb1a = to_level 1 a in
      let () = assert_equal comb1a a in
      let a = to_level 3 a in
      let () = assert_equal_int 3 (CodeSet.cardinal (get_combination 1950 a)) in
      let () = assert_equal_int 3 (CodeSet.cardinal (get_combination 1500 a)) in
      let () = assert_equal_int 2 (CodeSet.cardinal (get_combination  70 a)) in
      let () = assert_equal_int 2 (CodeSet.cardinal (get_combination  40 a)) in
      let () = assert_equal_int 1 (CodeSet.cardinal (get_combination   5 a)) in
      let () = assert_equal_int 1 (CodeSet.cardinal (get_combination  25 a)) in
      ());

    "Compare two alphabet elements" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 15 |> to_bitflag in
      let b = generate_seq_alphabet 15 in
      assert_bool "Sequential and bitflag alphabets are the same"
                  @@ compare_elts a b);

    "Compare two of the same generated alphabets" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 15 in
      let b = generate_seq_alphabet 15 in
      assert_bool "Two generated Sequential alphabets are the same"
                  @@ compare_elts a b);

    "Two different kinded alphabets are not the same" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 15 in
      let b = generate_seq_alphabet 15 |> to_bitflag in
      assert_bool "Two generated Sequential alphabets are the same"
                  @@ not (compare a b));

    "Duplicate elements" >::
    (fun _ctxt ->
      let states =
        List.map (fun k -> (Printf.sprintf "%02d" k,None))
                 ((0 -- 8) @ [0]) in
      let fnc () = 
        bitflag_alphabet ~states ~equates:[] ~orientation:false
                  ~case:false ~gap:None ~all:None ~missing:None |> ignore
      in
      assert_raises (Error (`Illegal_Character "00")) fnc);

    "Compress Polymorphisms is idempotent of singletons (bitflag)" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 15 |> to_bitflag in
      CodeSet.iter
        (fun x -> 
          assert_equal_int x (compress_polymorphisms [x] a))
        a.atomic);

    "Explode Polymorphisms handles duplicates (bitflag)" >::
    (fun _ctxt ->
      let a = generate_seq_alphabet 15 |> to_bitflag in
      CodeSet.iter
        (fun x -> 
          assert_equal_int_list [x] (explode_polymorphisms [x;x] a))
        a.atomic);

    "DNA compress polymorphism is injective (bitflag)" >::
    (fun _ctxt ->
      CodeSet.iter
        (fun x ->
          CodeSet.iter
            (fun y ->
              let xy = compress_polymorphisms [x;y] dna in
              assert_equal_int xy (x + y);
              let xy = explode_polymorphisms [xy] dna in
              assert_equal_int 2 (List.length xy);
              assert_bool "Compress then Explode are correct" @@ List.mem x xy;
              assert_bool "Compress then Explode are correct" @@ List.mem y xy)
            (CodeSet.remove x dna.atomic))
        dna.atomic);

    "Get state combination from bitset" >::
    (fun _ctxt ->
      CodeSet.fold
        (fun e (setacc, result) ->
          let result = e + result in
          let setacc = CodeSet.add e setacc in
          let actual = get_state_combination_exn setacc dna in
          assert_equal_int result actual;
          (setacc, result))
        dna.atomic
        (CodeSet.empty, 0)
      |> ignore);

    "bitset Explode polymorphism recovers elements" >::
    (fun _ctxt ->
      CodeSet.iter (fun x ->
        CodeSet.iter (fun y ->
          CodeSet.iter (fun z ->
            let lst = explode_polymorphisms [x+y+z] dna in
            assert_equal_int_list (List.sort Pervasives.compare [x;y;z])
                                  (List.sort Pervasives.compare lst);
          ) (CodeSet.remove y @@ CodeSet.remove x dna.atomic)
        ) (CodeSet.remove x dna.atomic)
      ) dna.atomic;
    );
  ]
