open Lwt.Infix
open Restaurant
open Menus
open Rest
open Points
open Queue

(* Mutable counter to keep track of the number of mistakes made by the user. The
   game ends if the value held in this location is equal to 3. *)
let mistake_counter = ref 0

let keys : string =
  "Here are the valid commands for the game: \n\
  \ - \"next\" to seat the next party in queue \n\
  \ - \"help\" to see the valid key commands \n\
  \ - \"exit\" to end the game \n"

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
  if !mistake_counter = 3 then (
    print_endline
      ("Oh no! You tried to seat too many \n\
       \    parties at occupied tables. You finished with a score of "
      ^ string_of_int (Points.get_points ())
      ^ ". Better luck next time! \n\
        \ 3110 Final Project FA2023: \n\
        \  by: C. Jin, S. Pan,\n\
        \   K. Sabile, S. Wang)");
    exit 0)
  else if WaitlistQueue.length !Rest.queue = 0 then (
    print_endline
      ("Congratulations! You successfully seated all customers! You\n\
       \  finished with a score of "
      ^ string_of_int (Points.get_points ())
      ^ ".");
    print_endline
      "Thanks for playing Dish Dash Dilemma~ See you again soon! \n\
      \ 3110 Final Project FA2023: \n\
      \  by: C. Jin, S. Pan,\n\
      \   K. Sabile, S. Wang)";
    exit 0)
  else (
    print_string "Enter a command here: ";
    let input = read_line () in
    if input = "next" then (
      match WaitlistQueue.dequeue !Rest.queue with
      | -1, _ -> failwith "Impossible"
      | party_size, q ->
          let start_time = Unix.gettimeofday () in
          print_endline
            ("Next in line is a party of " ^ string_of_int party_size ^ ".");
          print_endline
            "Which table number do you want to seat the party at? (starts from \
             1 and increases across the row first, then goes down columns) ";
          let rec seat n =
            try Rest.seat_party party_size n with
            | Rest.TableOccupied ->
                Points.parties_points
                  ~-(15
                    / (2 + int_of_float (Unix.gettimeofday () -. start_time)))
                  party_size;
                print_endline
                  ("Oops, looks like that table is occupied; your score is now "
                  ^ string_of_int (Points.get_points ())
                  ^ ". Try again: ");
                mistake_counter := !mistake_counter + 1;
                seat (read_int ())
            | Rest.TableNotFound ->
                print_endline "That table doesn't exist! Try again:";
                seat (read_int ())
          in
          seat (read_int ());
          Rest.queue := q;
          Rest.display_filled_restaurant ();
          if q = WaitlistQueue.empty then (
            print_endline
              ("Congratulations! You successfully seated all customers! You\n\
               \  finished with a score of "
              ^ string_of_int (Points.get_points ())
              ^ ".");
            print_endline
              "Thanks for playing Dish Dash Dilemma~ See you again soon! \n\
              \ 3110 Final Project FA2023: \n\
              \  by: C. Jin, S. Pan,\n\
              \   K. Sabile, S. Wang)";
            exit 0)
          else (
            print_endline
              "Great job! Your updated restaurant is displayed above. ";
            Points.parties_points
              (30 / (2 + int_of_float (Unix.gettimeofday () -. start_time)))
              party_size;
            Points.show_points ();
            read_key ()))
    else if input = "help" then
      let _ = print_endline keys in
      read_key ()
    else if input = "exit" then (
      print_endline
        ("Thanks for playing Dish Dash Dilemma! Your score is "
        ^ string_of_int (Points.get_points ())
        ^ ". See you again soon! \n\
          \ 3110 Final Project FA2023: \n\
          \  by: C. Jin, S. Pan,\n\
          \   K. Sabile, S. Wang)");
      exit 0)
    else (
      print_endline
        "Invalid command. Type \"help\" to see the list of valid commands. ";
      read_key ()))

(** Reads the user input and checks for the enter key. If not, then exits. *)
let read_enter () =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | Some input -> if input = "" then Lwt.return () else exit 0
  | None -> exit 0

(** Creates and displays a restaurant with the user-specified number of tables. *)
let setup_num_tables () =
  print_endline
    "First, enter the number of tables for the width and the height of the \
     restaurant: ";
  let n = read_int () in
  Rest.initialize_queue n;
  Rest.make_restaurant n;
  print_endline "Your empty restaurant looks like this: ";
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
        "To start the game, press the enter key. Press any other key to exit."
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
      Lwt_unix.sleep 5. >>= fun () ->
      Lwt_io.printl
        "\n\
        \  - You will earn points for seating a party at a table that is not\n\
        \    already occupied. The faster you are, the more points you will \n\
        \    earn.\n\
        \  - If you try to seat a party at an occupied table, your score will\n\
        \    go down. You will also not earn any points if you wait too long \n\
        \    to seat them.\n\
        \  - Try to earn as many points as possible and seat all the parties\n\
        \    before time runs out!"
      >>= fun () ->
      Lwt_unix.sleep 10. >>= fun () ->
      Lwt_io.printl
        "\n\
         Your current score will be updated as the game progresses. You will \
         lose if you try to seat more than three parties at occupied tables. \n\
        \  You will win if you seat all the parties in the queue. \n\
        \ Good luck! \n"
      >>= fun () ->
      Lwt_unix.sleep 7. >>= fun () ->
      setup_num_tables () >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl keys >>= fun () ->
      Lwt_unix.sleep 5. >>= fun () -> Lwt.return (read_key ()) )
