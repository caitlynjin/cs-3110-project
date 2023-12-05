open Lwt.Infix

let rec countdown_from n () =
  if n = 0 then Lwt.return_unit
  else
    Lwt_io.printf "Counting down from %d seconds...\n%!" n >>= fun () ->
    Lwt_unix.sleep 1. >>= countdown_from (n - 1)

let rec _input_every_5_seconds () =
  countdown_from 5 () >>= fun () ->
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | Some input ->
      if input = "exit" then Lwt.return ()
      else
        Lwt_io.printf "You entered: %s\n%!" input >>= fun () ->
        Lwt_io.printf "This is the restaurant being printed out.\n%!"
  | None -> _input_every_5_seconds ()

let rec random_int () =
  let x = Random.int 10 in
  if x = 0 then random_int () else x

let create_start_queue (len : int) : int Queue.t =
  let queue = Queue.create () in
  for i = 0 to len do
    Queue.add (random_int ()) queue
  done;
  queue

let get_next_party (q : int Queue.t) = Queue.take q

let rec check_table (init : int) =
  let new_duration = Random.int init in
  if new_duration = 0 then 0 else new_duration

(* TODO: Replace 0 above with changing state of table*)

let rec _run () =
  Lwt.pick
    [
      _input_every_5_seconds ();
      ( Lwt_unix.sleep 8. >>= fun () ->
        Lwt_io.printf "Timeout\n%!" >>= fun () -> Lwt.return () );
    ]
  >>= _run

(* To run this file, type in the command [make timer]. *)
let () =
  let game_duration = 20. in
  let waitlist_queue = create_start_queue 10 in
  let start_time = Unix.gettimeofday () in
  Lwt_main.run
    (Lwt.pick
       [ _run (); (Lwt_unix.sleep game_duration >>= fun () -> Lwt.return ()) ]);
  let end_time = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs\n%!" (end_time -. start_time)

(* IGNORE THE FOLLOWING CODE. These are functions that were being tested. Look
   above to see completed functions. *)

(* let rec _print_every_5_seconds () = countdown_from 5 () >>= fun () ->
   Lwt_io.printf "This is the restaurant being printed out.\n%!" >>= fun () ->
   Lwt_unix.sleep 1. >>= fun () -> _print_every_5_seconds () *)

(* let rec game_timer () = Lwt.bind (Lwt_unix.sleep 5.0) (fun () ->
   print_endline "5 seconds have passed in the game!"; game_timer ()) *)

(* let () = let line_promise : string Lwt.t = Lwt_io.(read_line stdin) in
   print_endline "Execution just continues";

   let line : string = Lwt_main.run line_promise in print_endline "Now
   unblocked"; ignore line *)

(* let () = let p : unit Lwt.t = let%lwt line_1 = Lwt_io.(read_line stdin) in
   let%lwt line_2 = Lwt_io.(read_line stdin) in Lwt_io.printf "%s and %s\n"
   line_1 line_2 in

   Lwt_main.run p *)

(* let rec run_game () : unit Lwt.t = let sleeper = Lwt_unix.sleep 5. in let
   reader = let%lwt line = Lwt_io.(read_line stdin) in Lwt.pick [ ( reader >>=
   fun _ -> Lwt.return_unit; sleeper >>= fun () -> Lwt_io.printl "5 seconds
   passed" >>= fun () -> run_game () ); ] >>= fun _ -> Lwt.return_unit *)

(* let () = let start_time = Unix.gettimeofday () in Lwt_main.run (run_game ())
   in let end_time = Unix.gettimeofday () in Printf.printf "Execution time:
   %fs\n%!" (end_time -. start_time) *)

(* let () = let start_time = Unix.gettimeofday () in Lwt_main.run begin let
   three_seconds : unit Lwt.t = Lwt_unix.sleep 3. in let five_seconds : unit
   Lwt.t = Lwt_unix.sleep 5. in let%lwt () = three_seconds in let%lwt () =
   Lwt_io.printl "3 seconds passed" in let%lwt () = five_seconds in
   Lwt_io.printl "Only 2 more seconds passed" end; let end_time =
   Unix.gettimeofday () in Printf.printf "Execution time: %fs\n%!" (end_time -.
   start_time) *)

(* let rec read_input () = Lwt_unix.sleep 5. >>= fun () -> Lwt_io.read_line_opt
   Lwt_io.stdin >>= function | Some input -> Lwt.return input | None ->
   Lwt_io.printl "5 seconds passed" >>= fun _ -> read_input ()

   let () = Lwt_main.run (read_input () >>= fun input -> Lwt_io.printf "You
   entered: %s\n" input) *)
