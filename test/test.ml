open OUnit2
open Restaurant
open Queue
open Table

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

let suite =
  "test suite for project" >::: List.flatten [ queue_tests; table_tests ]

let () = run_test_tt_main suite
