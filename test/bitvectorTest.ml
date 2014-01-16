open OUnit
open TestInternal

let generate_for_module (module BV : Bitvector.BV) =
  let max_random_int = (1 lsl 30)-1 in
  let m = min max_random_int BV.max_width in
  let width = 1 + (Random.int m) in
  let ws = Printf.sprintf "(%d,%d)" BV.max_width width in
  [
    "Create/Copy/Compare Functions:"^ws >::
      (fun () ->
        let b1 = BV.create width 100 in
        let b2 = BV.copy b1 in
        assert_equal 0 (BV.compare b1 b2);
        ());

    "Cardinal/Width/Code:"^ws >::
      (fun () ->
        let b1 = BV.create width 100 in
        assert_equal 100 @@ BV.cardinal b1;
        assert_equal width @@ BV.width b1;
        "Code is positive and accessible" @? ((BV.code b1) >= 0);
        ());

    "Set/Get Elements/Converstion:"^ws >::
      (fun () ->
        let b1 = BV.create width 100 in
        for i = 0 to (BV.cardinal b1)-1 do
          BV.set_elt b1 i (BV.random_elt width)
        done;
        let x = match BV.ints_of_elt @@ BV.get_elt b1 10 with 
          | []    -> BV.elt_of_ints [1]
          | x::xs -> BV.elt_of_ints xs
        in
        BV.set_elt b1 10 x;
        let xs = BV.elt_states b1 10 and ys = BV.ints_of_elt x in
        List.iter2 (fun x y -> assert_equal x y)
          (List.sort Pervasives.compare xs) (List.sort Pervasives.compare ys);
        ());

    "Set/Get Bits Set:"^ws >::
      (fun () -> ());

    "Union/Intersection:"^ws >::
      (fun () ->
        let b1 = BV.create width 100
        and b2 = BV.create width 100 in
        for i = 0 to (BV.cardinal b1)-1 do
          BV.set_elt b1 i (BV.random_elt width);
          BV.set_elt b2 i (BV.random_elt width)
        done;
        let b3 = BV.union b1 b2 in
        let b4 = BV.inter b1 b2 in
        assert_equal b1 (BV.union b1 b1);
        assert_equal b2 (BV.inter b2 b2);
        assert_equal b4 (BV.inter b4 b3);
        ());

    "Fitch/Sankoff/Distance:"^ws >::
      (fun () ->
        let b1 = BV.create width 100
        and b2 = BV.create width 100 in
        for i = 0 to (BV.cardinal b1)-1 do
          BV.set_elt b1 i (BV.random_elt width);
          BV.set_elt b2 i (BV.random_elt width)
        done;
        let b3,cst = BV.fitch_median_2 b1 b2 in
        assert_equal cst (BV.distance b1 b2);
        ());

    "Saturation/Poly-Saturation:"^ws >::
      (fun () -> ())
  ]

let local_tests =
  List.map
    generate_for_module
    [ (module Bitvector.BV8); (module Bitvector.BV16); (module Bitvector.BV32);
      (module Bitvector.BV64); (module Bitvector.BVGen) ]
  |> List.flatten

let tests = "Bitvector" >::: local_tests
