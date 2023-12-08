open Lwt.Infix
open Restaurant
open Menus
open Rest

<<<<<<< HEAD
(* let restaurant_data = (int * Table.t * TableQueue.t) list *)
let restaurant_layout = ref (Array.make 0 (Array.make 0 (ref "")))

let keys : string =
  "Here are valid commands for the game: \n\
=======
(** A string containing all the valid commands for the game. *)
let keys : string =
  "\n\
  \ Here are valid commands for the game: \n\
>>>>>>> bd154809852b37544fa753ee6fe5b95e8002a6a2
  \ - enter bar to get the size of the next party on queue \n\
  \ - \"a\" to seat the next party in queue \n\
  \ - \"help\" to see the valid key commands \n\
  \ - \"exit\" to quit the game \n"

<<<<<<< HEAD
=======
(** Reads the user input for an integer. If not, then console alerts that this
    is an invalid input and continues reading for user input until valid. *)
>>>>>>> bd154809852b37544fa753ee6fe5b95e8002a6a2
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
  print_string "Insert a command here: ";
  (* TODO: implement "a" to seat ppl *)
  let input = read_line () in
<<<<<<< HEAD
  if input = "" then
    (* TODO: call actual next queue party here *)
    let party_size = 1 + Random.int 10 in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".")
    (* TODO: call read_key after re-printing key commands *)
=======
  if input = "" then (
    (* TODO: call actual next queue party here *)
    let party_size = 1 + Random.int 10 in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".");
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printl keys >>= fun () -> read_key ())
  else if input = "a" then (
    print_endline
      "Which table number do you want to seat the party at? (increases across \
       the row first, then goes down columns) ";
    let table_num = read_int () in
    Rest.seat_party 3 table_num;
    read_key ())
>>>>>>> bd154809852b37544fa753ee6fe5b95e8002a6a2
  else if input = "help" then
    let _ = print_endline keys in
    read_key ()
  else if input = "exit" then exit 0
  else (
    print_endline "Please press enter or type \"exit\" to quit. ";
    read_key ())

<<<<<<< HEAD
let generate_row row i1 i2 c =
  for i = i1 to i2 do
    row.(i) <- c
  done;
  if i1 <> 0 then (
    row.(0) <- ref "|";
    row.(i2) <- ref "|")

let create_table_end row width =
  row.(0) <- ref "|";
  for i = 1 to width - 2 do
    if i mod 4 = 0 || i mod 3 = 0 then row.(i) <- ref " "
    else row.(i) <- ref "-"
  done;
  row.(width - 1) <- ref "|"

let table_middle_row row width =
  row.(0) <- ref "|";
  for i = 1 to width - 2 do
    if i mod 4 = 0 || i mod 3 = 0 then row.(i) <- ref "|"
    else row.(i) <- ref " "
  done;
  row.(width - 1) <- ref "|"

(* fill_restaurant will modify the corresponding rows of table_id and place 'x's
   around that table to represent the people in the party. [num_people] = the
   number of x's to place around the table [table_id] = the table to place the
   people at *)
(* let fill_restaurant restaurant num_people table_id = failwith "naur" *)

let display_filled_restaurant num_tables =
  let width = (5 * num_tables) + (3 * (num_tables - 1)) + 8 in
  let height = (num_tables * 5) + 2 in
  restaurant_layout := Array.make height (Array.make 0 (ref ""));
  for n = 1 to height - 1 do
    !restaurant_layout.(n) <- Array.make width (ref "")
  done;
  (* restaurant_layout.(0) <- ref "\n ~Dish Dash Dilemma!"; *)
  generate_row !restaurant_layout.(0) 0 width (ref "-");
  let i = ref 1 in
  while !i < height - 2 do
    generate_row !restaurant_layout.(!i) 1 (width - 1) (ref " ");
    create_table_end !restaurant_layout.(!i + 1) width;
    table_middle_row !restaurant_layout.(!i + 2) width;
    create_table_end !restaurant_layout.(!i + 3) width;
    generate_row !restaurant_layout.(!i + 4) 1 (width - 2) (ref " ");
    i := !i + 5
  done;
  generate_row
    !restaurant_layout.(Array.length !restaurant_layout - 1)
    0 (width - 1) (ref "-");
  for i = 0 to height - 1 do
    for j = 1 to width - 1 do
      print_string !(!restaurant_layout.(i).(j))
    done;
    print_string "\n"
  done;
  (* where print queue starts *)
  for _ = 0 to Random.int 10 do
    print_string "* "
  done;
  print_endline "\n"

(* running the game *)
let () =
  print_endline "Welcome to Dish Dash Dilemma!\n ";
  print_endline
    "In this game, you will be the host of a restaurant. \n\
    \ You are in charge of seating customers and making sure they are happy. \n\
    \ Let's see how well you do!";
  print_endline
    " \nTo start the game, press enter. Press any other key to exit. \n ";
  let input = read_line () in
  if input = "" then () else exit 0;
  print_endline
    "First, enter the number of tables for the width and the height of your \
     restaurant: ";
  let n = read_int () in
  display_filled_restaurant n;
  print_endline keys;
  (* TODO: edit? this is based of game finishing when queue ends *)
  (* print_endline "\n\ \ The objective of the game is to manage the restaurant.
     \n\ \ You will be given a queue of parties waiting to be seated. \n\ \ You
     must seat them in the restaurant. \n\ \ If you seat them at a table that is
     too small, they will leave. \n\ \ If you seat them at a table that is too
     big, they will leave. \n\ \ If you seat them at a table that is not ready,
     they will leave. \n\ \ If you seat them at a table that is just right, they
     will stay! \n\ \ You will be given a score based on how many parties you
     seat. \n\ \ You will lose if you seat too many parties at the wrong table
     size. \n\ \ You will win if you seat all the parties in the queue. \n\ \
     Good luck! \n"; *)
  read_key ()

(* print_endline "Thank you for playing Dish Dash Dilemma!";; *)
=======
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
         You will be given a queue of parties waiting to be seated. \n\
        \  You must seat them in the restaurant."
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl
        "\n\
         If you seat them at a table that is too small, they will leave. \n\
        \  If you seat them at a table that is too big, they will leave. \n\
        \  If you seat them at a table that is not ready, they will leave. \n\
        \  If you seat them at a table that is just right, they will stay!"
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
      Lwt_unix.sleep 2. >>= fun () -> read_key () )

(* TODO: uncomment this for end of game *)
(* >>= fun () -> Lwt_unix.sleep 1. >>= fun () -> Lwt_io.printl "Thank you for
   playing Dish Dash Dilemma!" >>= fun () -> Lwt_unix.sleep 1. >>= fun () ->
   Lwt_io.printl "Goodbye! (3110 Final Project FA2023: \n\ \ by: C. Jin, S. Pan,
   K. Sabile, S. Wang)" *)
>>>>>>> bd154809852b37544fa753ee6fe5b95e8002a6a2
