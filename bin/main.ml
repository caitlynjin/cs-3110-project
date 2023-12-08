open Lwt.Infix
open Restaurant
open Menus
open Rest

let keys : string =
  "\n\
  \ Here are valid commands for the game: \n\
  \ - enter bar to get the size of the next party on queue \n\
  \ - \"a\" to seat the next party in queue \n\
  \ - \"help\" to see the valid key commands \n\
  \ - \"exit\" to quit the game \n"

let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

let rec read_key () =
  (* TODO: edit these instructions *)
  print_string "Insert a command here: ";
  (* TODO: implement "a" to seat ppl *)
  let input = read_line () in
  if input = "" then
    (* TODO: call actual next queue party here *)
    let party_size = 1 + Random.int 10 in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".")
    (* TODO: call read_key after re-printing key commands *)
  else if input = "help" then
    let _ = print_endline keys in
    read_key ()
  else if input = "exit" then exit 0
  else (
    print_endline "Please press enter or type \"exit\" to quit. ";
    read_key ())

let read_enter () =
  Lwt_io.read_line_opt Lwt_io.stdin >>= function
  | Some input -> if input = "" then Lwt.return () else exit 0
  | None -> exit 0

(* let empty_restaurant size = Array.make size (ref "") *)

(* let create_filled_restaurant2 num_tables = let width = (5 * num_tables) + (3
   * (num_tables - 1)) + 8 in let height = num_tables * 5 in let restaurant =
   empty_restaurant (height + 2) in restaurant.(0) <- generate_line width "-";
   let i = ref 1 in while !i < height do restaurant.(!i) <- ref ("|" ^
   !(generate_line (width - 2) " ") ^ "|"); create_table_end restaurant.(!i + 1)
   num_tables; restaurant.(!i + 2) <- ref ("| " ^ !(generate_line num_tables " |
   | ") ^ " |"); create_table_end restaurant.(!i + 3) num_tables; restaurant.(!i
   + 4) <- ref ("|" ^ !(generate_line (width - 2) " ") ^ "|"); i := !i + 5 done;
   restaurant.(Array.length restaurant - 1) <- generate_line width "-"; for i =
   0 to Array.length restaurant - 1 do print_string (!(restaurant.(i)) ^ "\n")
   done; print_string "\n \n"; () *)

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

(* running the game *)
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
      Lwt_io.printl "\n  The objective of the game is to manage the restaurant."
      >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl
        "\n\
         You will be given a queue of parties waiting to be seated. \n\
        \  You must seat them in the restaurant."
      >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl
        "\n\
         If you seat them at a table that is too small, they will leave. \n\
        \  If you seat them at a table that is too big, they will leave. \n\
        \  If you seat them at a table that is not ready, they will leave. \n\
        \  If you seat them at a table that is just right, they will stay!"
      >>= fun () ->
      Lwt_unix.sleep 1. >>= fun () ->
      Lwt_io.printl
        "\n\
         You will be given a score based on how many parties you seat. \n\
        \  You will lose if you seat too many parties at the wrong table size. \n\
        \  You will win if you seat all the parties in the queue. \n\
        \ Good luck! \n"
      >>= fun () ->
      Lwt_unix.sleep 2. >>= fun () -> setup_num_tables () );
  print_endline keys;
  read_key ()

(* TODO: uncomment this for end of game *)
(* >>= fun () -> Lwt_unix.sleep 1. >>= fun () -> Lwt_io.printl "Thank you for
   playing Dish Dash Dilemma!" >>= fun () -> Lwt_unix.sleep 1. >>= fun () ->
   Lwt_io.printl "Goodbye! (3110 Final Project FA2023: \n\ \ by: C. Jin, S. Pan,
   K. Sabile, S. Wang)" *)
