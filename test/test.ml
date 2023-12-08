(* TEST PLAN:   
Manually tested parts:
- print statements (instructions, etc)
- timer (delay in instructions, duration of parties, etc)
Automated tests:
- queue functions
- table functions
- menu functions
- points functions
OUnit tested the Restaurant, Queue, Table, Menus, and Points modules. 
Test cases were developed using glass box testing to ensure maximum coverage. 
Therefore, more possible playouts of the game are tested and ensured that they 
run the way we intend them to. *)

open OUnit2
open Restaurant
open Queue
open Table
open Rest
open Points

let queue_tests =
  [
    ( "empty queue" >:: fun _ ->
      assert_equal [] (WaitlistQueue.to_list WaitlistQueue.empty) );
    ( "length of empty queue" >:: fun _ ->
      assert_equal 0 (WaitlistQueue.length WaitlistQueue.empty) );
    ( "length of nonempty queue" >:: fun _ ->
      assert_equal 3
        (WaitlistQueue.empty |> WaitlistQueue.enqueue 3
       |> WaitlistQueue.enqueue 6 |> WaitlistQueue.enqueue 1
       |> WaitlistQueue.length) );
    ( "enqueue one element" >:: fun _ ->
      assert_equal [ 3 ]
        (WaitlistQueue.empty |> WaitlistQueue.enqueue 3 |> WaitlistQueue.to_list)
    );
    ( "enqueue multiple elements" >:: fun _ ->
      assert_equal [ 3; 6; 1 ]
        (WaitlistQueue.empty |> WaitlistQueue.enqueue 3
       |> WaitlistQueue.enqueue 6 |> WaitlistQueue.enqueue 1
       |> WaitlistQueue.to_list) );
    ( "dequeue element from a nonempty queue" >:: fun _ ->
      assert_equal (Some 3)
        (WaitlistQueue.empty |> WaitlistQueue.enqueue 3
       |> WaitlistQueue.enqueue 6 |> WaitlistQueue.enqueue 1
       |> WaitlistQueue.dequeue) );
    ( "dequeue element from an empty queue" >:: fun _ ->
      assert_equal None (WaitlistQueue.empty |> WaitlistQueue.dequeue) );
  ]

let ready_table = ReadyTable.make 0 3
let occupied_table = OccupiedTable.make 4 8
let dirty_table = DirtyTable.make 2 4

let table_tests =
  [
    ( "ready table state" >:: fun _ ->
      assert_equal "Ready" (ReadyTable.state ready_table) );
    ( "ready table capacity" >:: fun _ ->
      assert_equal 3 (ReadyTable.capacity ready_table) );
    ( "ready table party size" >:: fun _ ->
      assert_equal 0 (ReadyTable.party_size ready_table) );
    ( "ready table\n       is ready" >:: fun _ ->
      assert_equal true (ReadyTable.isReady ready_table) );
    ( "occupied table state" >:: fun _ ->
      assert_equal "Occupied" (OccupiedTable.state occupied_table) );
    ( "occupied table capacity" >:: fun _ ->
      assert_equal 8 (OccupiedTable.capacity occupied_table) );
    ( "occupied table party size" >:: fun _ ->
      assert_equal 4 (OccupiedTable.party_size occupied_table) );
    ( "occupied table is ready" >:: fun _ ->
      assert_equal false (OccupiedTable.isReady occupied_table) );
    ( "dirty table state" >:: fun _ ->
      assert_equal "Dirty" (DirtyTable.state dirty_table) );
    ( "dirty table capacity" >:: fun _ ->
      assert_equal 4 (DirtyTable.capacity dirty_table) );
    ( "dirty table party size" >:: fun _ ->
      assert_equal 2 (DirtyTable.party_size dirty_table) );
    ( "dirty table\n       is ready" >:: fun _ ->
      assert_equal true (DirtyTable.isReady dirty_table) );
  ]

(* testing table coords *)

let four_table_coord_test =
  [
    ( "four tables table 1 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 2;
         Rest.get_act_table 1 |> Table.coord_list)
        [ (4, 6); (3, 8); (3, 4); (2, 6) ] );
    ( "four tables table 2 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 2;
         Rest.get_act_table 2 |> Table.coord_list)
        [ (4, 14); (3, 16); (3, 12); (2, 14) ] );
    ( "four tables table 3 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 2;
         Rest.get_act_table 3 |> Table.coord_list)
        [ (9, 6); (8, 8); (8, 4); (7, 6) ] );
    ( "four tables table 4 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 2;
         Rest.get_act_table 4 |> Table.coord_list)
        [ (9, 14); (8, 16); (8, 12); (7, 14) ] );
  ]

let nine_table_coord_test =
  [
    ( "nine tables table 1 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 1 |> Table.coord_list)
        [ (4, 6); (3, 8); (3, 4); (2, 6) ] );
    ( "nine tables table 2 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 2 |> Table.coord_list)
        [ (4, 14); (3, 16); (3, 12); (2, 14) ] );
    ( "nine tables table 3 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 3 |> Table.coord_list)
        [ (4, 22); (3, 24); (3, 20); (2, 22) ] );
    ( "nine tables table 4 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 4 |> Table.coord_list)
        [ (9, 6); (8, 8); (8, 4); (7, 6) ] );
    ( "nine tables table 5 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 5 |> Table.coord_list)
        [ (9, 14); (8, 16); (8, 12); (7, 14) ] );
    ( "nine tables table 6 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 6 |> Table.coord_list)
        [ (9, 22); (8, 24); (8, 20); (7, 22) ] );
    ( "nine tables table 7 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 7 |> Table.coord_list)
        [ (14, 6); (13, 8); (13, 4); (12, 6) ] );
    ( "nine tables table 8 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 8 |> Table.coord_list)
        [ (14, 14); (13, 16); (13, 12); (12, 14) ] );
    ( "nine tables table 9 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 3;
         Rest.get_act_table 9 |> Table.coord_list)
        [ (14, 22); (13, 24); (13, 20); (12, 22) ] );
  ]

let sixteen_table_coord_tests =
  [
    ( "sixteen tables table 1 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 1 |> Table.coord_list)
        [ (4, 6); (3, 8); (3, 4); (2, 6) ] );
    ( "sixteen tables table 2 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 2 |> Table.coord_list)
        [ (4, 14); (3, 16); (3, 12); (2, 14) ] );
    ( "sixteen tables table 3 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 3 |> Table.coord_list)
        [ (4, 22); (3, 24); (3, 20); (2, 22) ] );
    ( "sixteen tables table 4 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 4 |> Table.coord_list)
        [ (4, 30); (3, 32); (3, 28); (2, 30) ] );
    ( "sixteen tables table 5 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 5 |> Table.coord_list)
        [ (9, 6); (8, 8); (8, 4); (7, 6) ] );
    ( "sixteen tables table 6 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 6 |> Table.coord_list)
        [ (9, 14); (8, 16); (8, 12); (7, 14) ] );
    ( "sixteen tables table 7 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 7 |> Table.coord_list)
        [ (9, 22); (8, 24); (8, 20); (7, 22) ] );
    ( "sixteen tables table 8 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 8 |> Table.coord_list)
        [ (9, 30); (8, 32); (8, 28); (7, 30) ] );
    ( "sixteen tables table 9 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 9 |> Table.coord_list)
        [ (14, 6); (13, 8); (13, 4); (12, 6) ] );
    ( "sixteen tables table 10 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 10 |> Table.coord_list)
        [ (14, 14); (13, 16); (13, 12); (12, 14) ] );
    ( "sixteen tables table 11 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 11 |> Table.coord_list)
        [ (14, 22); (13, 24); (13, 20); (12, 22) ] );
    ( "sixteen tables table 12 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 12 |> Table.coord_list)
        [ (14, 30); (13, 32); (13, 28); (12, 30) ] );
    ( "sixteen tables table 13 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 13 |> Table.coord_list)
        [ (19, 6); (18, 8); (18, 4); (17, 6) ] );
    ( "sixteen tables table 14 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 14 |> Table.coord_list)
        [ (19, 14); (18, 16); (18, 12); (17, 14) ] );
    ( "sixteen tables table 15 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 15 |> Table.coord_list)
        [ (19, 22); (18, 24); (18, 20); (17, 22) ] );
    ( "sixteen tables table 16 coord" >:: fun _ ->
      assert_equal
        (Rest.make_restaurant 4;
         Rest.get_act_table 16 |> Table.coord_list)
        [ (19, 30); (18, 32); (18, 28); (17, 30) ] );
  ]

let table_coord_test =
  List.flatten
    [ four_table_coord_test; nine_table_coord_test; sixteen_table_coord_tests ]

(* testing seating and removing people *)

let seat_is_filled x y = Rest.get_coord_value x y = "*"

let seat_is_empty x y =
  Rest.get_coord_value x y = "-" || Rest.get_coord_value x y = "|"

(* testing seating people *)
let four_table_seating_test =
  [
    ( "four tables seat 4 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 4 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_filled 2 6
         && seat_is_filled 3 4) );
    ( "four tables seat 3 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 3 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_filled 3 4) );
    ( "four tables seat 2 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 2 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "four tables seat 1 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 1 1;
         seat_is_filled 4 6 && seat_is_empty 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "four tables seat 1 at 4" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 1 4;
         seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && seat_is_empty 8 16) );
    ( "four tables seat 3 at 2" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 3 2;
         seat_is_filled 4 14 && seat_is_filled 3 16 && seat_is_filled 3 12
         && seat_is_empty 2 14) );
    ( "four tables seat 3 at 2; seat 1 at 4, seat 2 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 3 2;
         Rest.seat_party 1 4;
         Rest.seat_party 2 1;
         seat_is_filled 4 14 && seat_is_filled 3 16 && seat_is_filled 3 12
         && seat_is_empty 2 14 && seat_is_filled 4 6 && seat_is_filled 3 8
         && seat_is_empty 2 6 && seat_is_empty 3 4 && seat_is_filled 9 14
         && seat_is_empty 8 12 && seat_is_empty 7 14 && seat_is_empty 8 16) );
  ]

let nine_table_seating_test =
  [
    ( "nine tables seat 4 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 4 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_filled 2 6
         && seat_is_filled 3 4) );
    ( "nine tables seat 3 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_filled 3 4) );
    ( "nine tables seat 2 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 2 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "nine tables seat 1 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 1 1;
         seat_is_filled 4 6 && seat_is_empty 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "nine tables seat 2 at 7" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 2 7;
         seat_is_filled 14 6 && seat_is_filled 13 8 && seat_is_empty 12 6
         && seat_is_empty 13 4) );
    ( "nine tables seat 1 at 5" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 1 5;
         seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && seat_is_empty 8 16) );
    ( "nine tables seat 3 at 2" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 2;
         seat_is_filled 4 14 && seat_is_filled 3 16 && seat_is_filled 3 12
         && seat_is_empty 2 14) );
    ( "nine tables seat 3 at 8" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 8;
         seat_is_filled 14 14 && seat_is_filled 13 16 && seat_is_filled 13 12
         && seat_is_empty 12 14) );
    ( "nine tables seat 1 at 3" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 1 3;
         seat_is_filled 4 22 && seat_is_empty 3 24 && seat_is_empty 3 20
         && seat_is_empty 2 22) );
    ( "nine tables seat 3 at 6" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 6;
         seat_is_filled 9 22 && seat_is_filled 8 24 && seat_is_filled 8 20
         && seat_is_empty 7 22) );
    ( "nine tables seat 3 at 2; seat 1 at 5, seat 2 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 2;
         Rest.seat_party 1 5;
         Rest.seat_party 2 1;
         seat_is_filled 4 14 && seat_is_filled 3 16 && seat_is_filled 3 12
         && seat_is_empty 2 14 && seat_is_filled 4 6 && seat_is_filled 3 8
         && seat_is_empty 2 6 && seat_is_empty 3 4 && seat_is_filled 9 14
         && seat_is_empty 8 12 && seat_is_empty 7 14 && seat_is_empty 8 16) );
    ( "nine tables seat 3 at 6; seat 1 at 5, seat 4 at 1, seat 2 at 7"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 3 6;
         Rest.seat_party 1 5;
         Rest.seat_party 4 1;
         Rest.seat_party 2 7;
         seat_is_filled 14 6 && seat_is_filled 13 8 && seat_is_empty 12 6
         && seat_is_empty 13 4 && seat_is_filled 4 6 && seat_is_filled 3 8
         && seat_is_filled 2 6 && seat_is_filled 3 4 && seat_is_filled 9 22
         && seat_is_filled 8 24 && seat_is_filled 8 20 && seat_is_empty 7 22
         && seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && seat_is_empty 8 16) );
  ]

let sixteen_table_seating_test =
  [
    ( "sixteen tables seat 4 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_filled 2 6
         && seat_is_filled 3 4) );
    ( "sixteen tables seat 3 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_filled 3 4) );
    ( "sixteen tables seat 2 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "sixteen tables seat 2 at 4" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 4;
         seat_is_filled 4 30 && seat_is_filled 3 32 && seat_is_empty 3 28
         && seat_is_empty 2 30) );
    ( "sixteen tables seat 1 at 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 1;
         seat_is_filled 4 6 && seat_is_empty 3 8 && seat_is_empty 2 6
         && seat_is_empty 3 4) );
    ( "sixteen tables seat 1 at 6" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 6;
         seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && seat_is_empty 8 16) );
    ( "sixteen tables seat 2 at 9" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 9;
         seat_is_filled 14 6 && seat_is_filled 13 8 && seat_is_empty 12 6
         && seat_is_empty 13 4) );
    ( "sixteen tables seat 3 at 10" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 10;
         seat_is_filled 14 14 && seat_is_filled 13 16 && seat_is_filled 13 12
         && seat_is_empty 12 14) );
    ( "sixteen tables seat 3 at 15" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 15;
         seat_is_filled 19 22 && seat_is_filled 18 24 && seat_is_filled 18 20
         && seat_is_empty 17 22) );
    ( "sixteen tables seat 4 at 8" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 8;
         seat_is_filled 9 30 && seat_is_filled 8 32 && seat_is_filled 8 28
         && seat_is_filled 7 30) );
    ( "sixteen tables seat 1 at 14" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 14;
         seat_is_filled 19 14 && seat_is_empty 18 12 && seat_is_empty 17 14
         && seat_is_empty 18 16) );
    ( "sixteen tables seat 3 at 2" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 2;
         seat_is_filled 4 14 && seat_is_filled 3 16 && seat_is_filled 3 12
         && seat_is_empty 2 14) );
    ( "sixteen tables seat 4 at 16" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 16;
         seat_is_filled 19 30 && seat_is_filled 18 32 && seat_is_filled 18 28
         && seat_is_filled 17 30) );
    ( "sixteen tables seat 1 at 3" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 3;
         seat_is_filled 4 22 && seat_is_empty 3 24 && seat_is_empty 3 20
         && seat_is_empty 2 22) );
    ( "sixteen tables seat 3 at 7" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 7;
         seat_is_filled 9 22 && seat_is_filled 8 24 && seat_is_filled 8 20
         && seat_is_empty 7 22) );
    ( "sixteen tables seat 2 at 12" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 12;
         seat_is_filled 14 30 && seat_is_filled 13 32 && seat_is_empty 13 28
         && seat_is_empty 12 30) );
    ( "sixteen tables seat 3 at 7; seat 1 at 14, seat 4 at 8" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 7;
         Rest.seat_party 1 14;
         Rest.seat_party 4 8;
         seat_is_filled 9 30 && seat_is_filled 8 32 && seat_is_filled 8 28
         && seat_is_filled 7 30 && seat_is_filled 19 14 && seat_is_empty 18 12
         && seat_is_empty 17 14 && seat_is_empty 18 16 && seat_is_filled 9 22
         && seat_is_filled 8 24 && seat_is_filled 8 20 && seat_is_empty 7 22) );
    ( "sixteen tables seat 4 at 16, seat 2 at 9, seat 1 at 14, seat 3 at 1 "
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 16;
         Rest.seat_party 2 9;
         Rest.seat_party 1 14;
         Rest.seat_party 3 1;
         seat_is_filled 4 6 && seat_is_filled 3 8 && seat_is_empty 2 6
         && seat_is_filled 3 4 && seat_is_filled 19 14 && seat_is_empty 18 12
         && seat_is_empty 17 14 && seat_is_empty 18 16 && seat_is_filled 14 6
         && seat_is_filled 13 8 && seat_is_empty 12 6 && seat_is_empty 13 4
         && seat_is_filled 19 30 && seat_is_filled 18 32 && seat_is_filled 18 28
         && seat_is_filled 17 30) );
    ( "sixteen tables seat 1 at 6, seat 4 at 8, seat 3 at 10, seat 3 at 10, \
       seat 2 at 11, seat 4 at 5  "
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 6;
         Rest.seat_party 4 8;
         Rest.seat_party 3 10;
         Rest.seat_party 2 11;
         Rest.seat_party 4 5;
         seat_is_filled 14 14 && seat_is_filled 13 16 && seat_is_filled 13 12
         && seat_is_empty 12 14 && seat_is_filled 9 30 && seat_is_filled 8 32
         && seat_is_filled 9 6 && seat_is_filled 8 8 && seat_is_filled 8 4
         && seat_is_filled 7 6 && seat_is_filled 8 28 && seat_is_filled 7 30
         && seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && seat_is_empty 8 16 && seat_is_filled 14 22 && seat_is_filled 13 24
         && seat_is_empty 13 20 && seat_is_empty 12 22) );
  ]

let table_seating_test =
  List.flatten
    [
      four_table_seating_test;
      nine_table_seating_test;
      sixteen_table_seating_test;
    ]

(* testing removing people *)

let table_is_empty id =
  let b1, b2 = List.nth (Table.coord_list (Rest.get_act_table id)) 0 in
  let t1, t2 = List.nth (Table.coord_list (Rest.get_act_table id)) 3 in
  let l1, l2 = List.nth (Table.coord_list (Rest.get_act_table id)) 2 in
  let r1, r2 = List.nth (Table.coord_list Rest.(get_act_table id)) 1 in
  Rest.get_coord_value b1 b2 = "-"
  && Rest.get_coord_value t1 t2 = "-"
  && Rest.get_coord_value r1 r2 = "|"
  && Rest.get_coord_value l1 l2 = "|"

(* testing seating people *)
let four_table_removing_test =
  [
    ( "four tables seat 4 at 1 then remove 1" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 4 1;
         Rest.finish_eating 1;
         table_is_empty 1) );
    ( "four tables seat 3 at 2; seat 1 at 4, seat 2 at 1, then remove 1 and 2"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 3 2;
         Rest.seat_party 1 4;
         Rest.seat_party 2 1;
         Rest.finish_eating 1;
         Rest.finish_eating 2;
         seat_is_filled 9 14 && seat_is_empty 8 12 && seat_is_empty 7 14
         && table_is_empty 1 && table_is_empty 2 && seat_is_empty 8 16) );
    ( "four tables seat 3 at 2; seat 1 at 4, seat 2 at 1, then remove all "
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 3 2;
         Rest.seat_party 1 4;
         Rest.seat_party 2 1;
         Rest.finish_eating 1;
         Rest.finish_eating 2;
         Rest.finish_eating 3;
         table_is_empty 1 && table_is_empty 2 && table_is_empty 3) );
    ( "four tables seat 2 at 2 then seat 3 at 1 then remove 1 then seat 4 at 1 \
       then remove 2 then remove 1"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 2;
         Rest.seat_party 2 2;
         Rest.seat_party 3 1;
         Rest.finish_eating 1;
         Rest.clean_table 1;
         Rest.seat_party 4 1;
         Rest.finish_eating 2;
         Rest.finish_eating 1;
         table_is_empty 1 && table_is_empty 2) );
  ]

let nine_table_removing_test =
  [
    ( "nine tables seat 4 at 4 then remove 4" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 4 4;
         Rest.finish_eating 4;
         table_is_empty 4) );
    ( "nine tables seat 2 at 6 then remove 6" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 2 6;
         Rest.finish_eating 6;
         table_is_empty 6) );
    ( "nine tables seat 1 at 9 then remove 9" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 1 9;
         Rest.finish_eating 9;
         table_is_empty 9) );
    ( "nine tables seat 2 at 3; seat 4 at 5, seat 3 at 8, then remove 3 and 5"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 2 3;
         Rest.seat_party 4 5;
         Rest.seat_party 3 8;
         Rest.finish_eating 3;
         Rest.finish_eating 5;
         seat_is_filled 14 14 && seat_is_filled 13 16 && seat_is_filled 13 12
         && seat_is_empty 12 14 && table_is_empty 3 && table_is_empty 5) );
    ( "nine tables seat 1 at 9; seat 4 at 3, seat 2 at 8, then remove all "
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 1 9;
         Rest.seat_party 4 3;
         Rest.seat_party 2 8;
         Rest.finish_eating 9;
         Rest.finish_eating 8;
         Rest.finish_eating 3;
         table_is_empty 9 && table_is_empty 8 && table_is_empty 3) );
    ( "nine tables seat 4 at 3; seat 1 at 7; remove 3; seat 3 at 2; remove 7; \
       seat 2 at 7remove 2; seat 2 at 2"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 3;
         Rest.seat_party 4 3;
         Rest.seat_party 1 7;
         Rest.finish_eating 3;
         Rest.seat_party 3 2;
         Rest.finish_eating 7;
         Rest.clean_table 7;
         Rest.seat_party 2 7;
         Rest.finish_eating 2;
         table_is_empty 3 && table_is_empty 2 && seat_is_filled 14 6
         && seat_is_filled 13 8 && seat_is_empty 12 6 && seat_is_empty 13 4) );
  ]

let sixteen_table_removing_test =
  [
    ( "sixteen tables seat 2 at 16 then remove 16" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 16;
         Rest.finish_eating 16;
         table_is_empty 16) );
    ( "sixteen tables seat 4 at 7 then remove 7" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 7;
         Rest.finish_eating 7;
         table_is_empty 7) );
    ( "sixteen tables seat 3 at 12 then remove 12" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 12;
         Rest.finish_eating 12;
         table_is_empty 12) );
    ( "sixteen tables seat 1 at 10 then remove 10" >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 1 10;
         Rest.finish_eating 10;
         table_is_empty 10) );
    ( "sixteen tables 2 at 11; seat 1 at 5; seat 4 at 7l seat 3 at 16; remove \
       5, 16, and 11"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 2 11;
         Rest.seat_party 1 5;
         Rest.seat_party 4 7;
         Rest.seat_party 3 16;
         Rest.finish_eating 5;
         Rest.finish_eating 16;
         Rest.finish_eating 11;
         seat_is_filled 9 22 && seat_is_filled 8 24 && seat_is_filled 8 20
         && table_is_empty 3 && table_is_empty 11 && table_is_empty 16
         && seat_is_filled 7 22) );
    ( "sixteen tables seat 4 at 3; seat 3 at 6; seat 2 at 14; then remove all "
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 4 3;
         Rest.seat_party 3 6;
         Rest.seat_party 2 14;
         Rest.finish_eating 14;
         Rest.finish_eating 3;
         Rest.finish_eating 6;
         table_is_empty 6 && table_is_empty 14 && table_is_empty 3) );
    ( "sixteen tables  seat 3 at 12; seat 1 at 2; finish seating 2; seat 4 at \
       14; seat 2 at 2; remove 12 and 14; seat 2 at 15; remove 2 and 15, seat \
       1 at 14"
    >:: fun _ ->
      assert_equal true
        (Rest.make_restaurant 4;
         Rest.seat_party 3 12;
         Rest.seat_party 1 2;
         Rest.finish_eating 2;
         Rest.seat_party 4 14;
         Rest.clean_table 2;
         Rest.seat_party 2 2;
         Rest.finish_eating 12;
         Rest.finish_eating 14;
         Rest.seat_party 2 15;
         Rest.finish_eating 2;
         Rest.finish_eating 15;
         Rest.clean_table 14;
         Rest.seat_party 1 14;
         seat_is_filled 19 14 && seat_is_empty 18 12 && seat_is_empty 17 14
         && table_is_empty 12 && table_is_empty 2 && table_is_empty 15
         && seat_is_empty 18 16) );
  ]

let table_removing_test =
  List.flatten
    [
      four_table_removing_test;
      nine_table_removing_test;
      sixteen_table_removing_test;
    ]

let restaurant_test =
  List.flatten [ table_coord_test; table_seating_test; table_removing_test ]

(* let menu_test = [ ("menu test" >:: fun _ -> assert_equal "" !Menus.cuisine)
   ] *)
let gamepts = Points.game_pts

(* let pp_string s = "\"" ^ s ^ "\"" *)
let pp_int i = "\"" ^ string_of_int i ^ "\""

let points_test =
  [
    (* points for queue management: parties_points *)
    ( "short wait time" >:: fun _ ->
      let _ = gamepts.queue_performance <- 0 in
      let _ = Points.parties_points 5 3 in
      assert_equal ~printer:pp_int 75 gamepts.queue_performance );
    (* values accumulate?.. so set back to 0 *)
    ( "medium wait time" >:: fun _ ->
      let _ = gamepts.queue_performance <- 0 in
      let _ = Points.parties_points 2 11 in
      assert_equal ~printer:pp_int 20 gamepts.queue_performance );
    ( "long wait time" >:: fun _ ->
      let _ = gamepts.queue_performance <- 0 in
      let _ = Points.parties_points 5 35 in
      assert_equal ~printer:pp_int 25 gamepts.queue_performance );
    (* points for treating parties at tables: meals_points *)
    ( "short food time" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 1 1 1 1 in
      assert_equal ~printer:pp_int 25 gamepts.meals );
    ( "long food time" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 1 30 2 1 in
      assert_equal ~printer:pp_int 15 gamepts.meals );
    ( "long duration at table" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 5 1 1 40 in
      assert_equal ~printer:pp_int 95 gamepts.meals );
    ( "short duration at table" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 5 2 1 1 in
      assert_equal ~printer:pp_int 45 gamepts.meals );
    ( "medium food time" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 1 12 1 12 in
      assert_equal ~printer:pp_int 17 gamepts.meals );
    ( "medium duration" >:: fun _ ->
      let _ = gamepts.meals <- 0 in
      let _ = Points.meals_points 1 3 1 25 in
      assert_equal ~printer:pp_int 30 gamepts.meals );
    (* points from money generated: profits_points *)
    ( "test profits" >:: fun _ ->
      let _ = Points.profits_points 25. in
      assert_equal ~printer:pp_int 625 gamepts.profits );
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ queue_tests; table_tests; points_test; restaurant_test ]

let () = run_test_tt_main suite
