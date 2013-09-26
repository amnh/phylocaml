open OUnit
open Internal

let test_0 (module BV : Bitvector.BV) width () =
  let b1 = BV.create width 100 in
  let b2 = BV.copy b1 in
  assert_equal 0 (BV.compare b1 b2);
  ()

and test_1 (module BV : Bitvector.BV) width () =
  let b1 = BV.create width 100 in
  assert_equal 100 @@ BV.cardinal b1;
  assert_equal width @@ BV.width b1;
  "Code is positive and accessible" @? ((BV.code b1) >= 0);
  ()

and test_2 (module BV : Bitvector.BV) width () =
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
  ()

and test_3 (module BV : Bitvector.BV) width () = ()


and test_4 (module BV : Bitvector.BV) width () =
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
  ()

and test_5 (module BV : Bitvector.BV) width () =
  let b1 = BV.create width 100
  and b2 = BV.create width 100 in
  for i = 0 to (BV.cardinal b1)-1 do
    BV.set_elt b1 i (BV.random_elt width);
    BV.set_elt b2 i (BV.random_elt width)
  done;
  let b3,cst = BV.fitch_median_2 b1 b2 in
  assert_equal cst (BV.distance b1 b2);
  ()

and test_6 (module BV : Bitvector.BV) width () = ()



let local_tests = 
  let max_random_int = (1 lsl 30)-1 in
  List.map
    (fun (module BV : Bitvector.BV) ->
      let m = min max_random_int BV.max_width in
      let w = 1 + (Random.int m) in
      let ws = Printf.sprintf "(%d,%d)" BV.max_width w in
      [
        "Create/Copy/Compare Functions:"^ws >:: (test_0 (module BV) w);
        "Cardinal/Width/Code:"^ws           >:: (test_1 (module BV) w);
        "Set/Get Elements/Converstion:"^ws  >:: (test_2 (module BV) w);
        "Set/Get Bits Set:"^ws              >:: (test_3 (module BV) w);
        "Union/Intersection:"^ws            >:: (test_4 (module BV) w);
        "Fitch/Sankoff/Distance:"^ws        >:: (test_5 (module BV) w);
        "Saturation/Poly-Saturation:"^ws    >:: (test_6 (module BV) w);
      ])
    [ (module Bitvector.BV8); (module Bitvector.BV16); (module Bitvector.BV32);
      (module Bitvector.BV64); (module Bitvector.BVGen) ]
  |> List.flatten

let tests = "Bitvector" >::: local_tests
