open Lwt.Infix
open Restaurant
open Menus
open Rest
open Points

(* let restaurant_data = (int * Table.t * TableQueue.t) list *)

let keys : string =
  "These are the valid commands for the game: \n\
  \ - \"next\" to seat the next party in queue \n\
  \ - \"help\" to see the valid key commands \n\
  \ - \"exit\" to quit the game \n"

(** Reads the user input for an integer. If not, then console alerts that this
    is an invalid input and continues reading for user input until valid. *)
let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

(** Reads the user input for any of the valid commands for the game. If "help"
    is entered, console displays the list of valid commands. If "exit" is
    entered, exits the game. If an enter key is inputted, then calls the next
    queue party. *)
let rec read_key () =
  (* TODO: edit these instructions *)
  print_string "Enter a command here: ";
  (* TODO: implement "a" to seat ppl *)
  let input = read_line () in
  if input = "next" then (
    (* TODO: call actual next queue party here *)
    Random.self_init ();
    let party_size = 1 + Random.int 3 in
    let start_time = Unix.gettimeofday () in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".");
    print_endline
      "Which table number do you want to seat the party at? (starts from 1 and \
       increases across the row first, then goes down columns) ";
    let rec seat n =
      try Rest.seat_party party_size n with
      | Rest.SizeError ->
          print_endline
            "Uh oh! Looks like that table is occupied. Try choosing another \
             one:";
          seat (read_int ())
      | Rest.TableOccupied ->
          print_endline
            "Oops, looks like that table hasn't been cleaned yet - try again:";
          seat (read_int ())
      | Rest.TableNotFound ->
          print_endline "That table doesn't exist! Try again:";
          seat (read_int ())
    in
    seat (read_int ());
    print_endline "Great job! Your restaurant now looks like this: ";
    Rest.display_filled_restaurant ();
    Points.parties_points
      (int_of_float (Unix.gettimeofday () -. start_time))
      party_size;
    Points.show_points ();
    read_key ())
  else if input = "help" then
    let _ = print_endline keys in
    read_key ()
  else if input = "exit" then exit 0
  else (
    print_endline
      "Invalid command. Type \"help\" to see the list of valid commands. ";
    read_key ())

(** Reads the user input and checks for the enter key. If not, then exits. *)
let read_enter () =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | Some input -> if input = "" then Lwt.return () else exit 0
  | None -> exit 0

(* let empty_restaurant size = Array.make size (ref "") *)

(** Creates and displays a restaurant with the user-specified number of tables. *)
let setup_num_tables () =
  print_endline
    "First, enter the number of tables for the width and the height of the \
     restaurant: ";
  Rest.make_restaurant (read_int ());
  Rest.display_filled_restaurant ();

  (* just for testing purposes *)
  (* fill_restaurant 4 1; display_filled_restaurant (); fill_restaurant 3 5;
     display_filled_restaurant (); *)

  (* just for testing purposes *)
  (* print_list (); *)
  Lwt.return ()

(** Runs the game. *)
let () =
  Lwt_main.run
    ( Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl "\nWelcome to Dish Dash Dilemma!\n " >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl
        "In this game, you will be the host of a restaurant. \n\
        \ You are in charge of seating customers and making sure they are \
         happy. \n\
        \ Let's see how well you do!"
      >>= fun () ->
      Lwt_unix.sleep 4. >>= fun () ->
      Lwt_io.printl
        " \nTo start the game, press enter. Press any other key to exit. \n "
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      read_enter () >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Menus.set_up_restaurant () >>= fun () ->
      Lwt_io.printl "The objective of the game is to manage the restaurant."
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl
        "\n\
         You will be given a queue of parties waiting to be seated.\n\
        \ You must seat them in the restaurant at appropriate tables."
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl
        "\n\
        \  - If you seat them at a table that is too small, they will leave. \n\
        \  - If you seat them at a table that is too big, they will leave. \n\
        \  - If you seat them at a table that is not ready, they will leave. \n\
        \  - If you seat them at a table that is just right, they will stay!"
      >>= fun () ->
      Lwt_unix.sleep 3. >>= fun () ->
      Lwt_io.printl
        "\n\
         You will be given a score based on how many parties you seat. You \
         will lose if you seat too many parties at the wrong table size. \n\
        \  You will win if you seat all the parties in the queue. \n\
        \ Good luck! \n"
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      setup_num_tables () >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl keys >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt.return (read_key ()) >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl "Thank you for\n   playing Dish Dash Dilemma!" >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl
        "Goodbye! (3110 Final Project FA2023: \n\
        \  by: C. Jin, S. Pan,\n\
        \   K. Sabile, S. Wang)" )
