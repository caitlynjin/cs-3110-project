open OUnit2
open Restaurant
open Queue
open Table

(* open Menus *)
open Points

(* TEST PLAN
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
run the way we intend them to.
 *)

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
    ("ready table is ready" >:: fun _ -> assert_equal true ReadyTable.isReady);
    ( "occupied table state" >:: fun _ ->
      assert_equal "Occupied" (OccupiedTable.state occupied_table) );
    ( "occupied table capacity" >:: fun _ ->
      assert_equal 8 (OccupiedTable.capacity occupied_table) );
    ( "occupied table party size" >:: fun _ ->
      assert_equal 4 (OccupiedTable.party_size occupied_table) );
    ( "occupied table is ready" >:: fun _ ->
      assert_equal false OccupiedTable.isReady );
    ( "dirty table state" >:: fun _ ->
      assert_equal "Dirty" (DirtyTable.state dirty_table) );
    ( "dirty table capacity" >:: fun _ ->
      assert_equal 4 (DirtyTable.capacity dirty_table) );
    ( "dirty table party size" >:: fun _ ->
      assert_equal 2 (DirtyTable.party_size dirty_table) );
    ("dirty table is ready" >:: fun _ -> assert_equal true DirtyTable.isReady);
  ]

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
  >::: List.flatten [ queue_tests; table_tests; points_test ]

let () = run_test_tt_main suite
