open Lwt.Infix

let rec countdown_from n () =
  if n = 0 then Lwt.return_unit
  else
    Lwt_io.printf "Counting down from %d seconds...\n%!" n >>= fun () ->
    Lwt_unix.sleep 1. >>= countdown_from (n - 1)

(* let rec input_every_5_seconds () = countdown_from 5 () >>= fun () ->
   Lwt_io.read_line_opt Lwt_io.stdin >>= function | Some _ -> Lwt_io.printf
   "This is the restaurant being printed out.\n%!" >>= fun () ->
   input_every_5_seconds () | None -> input_every_5_seconds () *)

let rec print_every_5_seconds () =
  countdown_from 5 () >>= fun () ->
  Lwt_io.printf "This is the restaurant being printed out.\n%!" >>= fun () ->
  Lwt_unix.sleep 1. >>= fun () -> print_every_5_seconds ()

(* To run this file, type in the command [make timer]. *)
let () =
  Lwt_main.run
    (* Replace [print_every_5_seconds] with [input_every_5_seconds] and vice
       versa. *)
    ( print_every_5_seconds () >>= fun input ->
      Lwt_io.printf "You entered: %s\n" input )

(* IGNORE THE FOLLOWING CODE. These are functions that were being tested. Look
   above to see completed functions. *)

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
