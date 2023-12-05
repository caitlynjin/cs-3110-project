open OUnit2
open Restaurant
open Queue

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

let suite = "test suite for project" >::: List.flatten [ queue_tests ]
let () = run_test_tt_main suite
