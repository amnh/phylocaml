open OUnit2
open TestInternal
open Topology

let tests = "Topology.IDManager" >:::
[
  "UnOrdered list of ids with 0" >::
  (fun _ctxt ->
    let h,hl = Topology.IDManager.of_list [0;1;2;3;4;5;6;7;8;9;] in
    assert_equal_int_list [] hl;
    assert_equal_int 10 h);

  "UnOrdered list of ids without 0" >::
  (fun _ctxt ->
    let h,hl = Topology.IDManager.of_list [1;2;3;4;5;6;7;8;9;] in
    assert_equal_int_list [0] hl;
    assert_equal_int 10 h);

(*"UnOrdered list of ids with negative value" >::
  (fun _ctxt ->
    todo "raise proper assertion in library";
    let h,hl = Topology.IDManager.of_list [1;2;0;-1;4;5;6;7;8;9;] in
    () ); *)

  "UnOrdered list of ids with a subseq of missing values" >::
  (fun _ctxt ->
    let h,hl = Topology.IDManager.of_list [1;0;4;5;9;15;] in
    assert_equal_int 16 h;
    assert_bool "Hole in sequence at 02" (List.mem  2 hl);
    assert_bool "Hole in sequence at 03" (List.mem  3 hl);
    assert_bool "Hole in sequence at 06" (List.mem  6 hl);
    assert_bool "Hole in sequence at 07" (List.mem  7 hl);
    assert_bool "Hole in sequence at 08" (List.mem  8 hl);
    assert_bool "Hole in sequence at 10" (List.mem 10 hl);
    assert_bool "Hole in sequence at 11" (List.mem 11 hl);
    assert_bool "Hole in sequence at 12" (List.mem 12 hl);
    assert_bool "Hole in sequence at 13" (List.mem 13 hl);
    assert_bool "Hole in sequence at 14" (List.mem 14 hl))
]

