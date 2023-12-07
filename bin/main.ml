open Lwt.Infix
open Restaurant

(* let restaurant_data = (int * Table.t * TableQueue.t) list *)
let restaurant_layout = ref (Array.make 0 (Array.make 0 (ref "")))

let keys : string =
  "Here are valid commands for the game: \n\
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
  let height = num_tables * 5 in
  for n = 0 to height + 2 do
    !restaurant_layout.(n) <- Array.make width (ref "")
  done;
  (* restaurant_layout.(0) <- ref "\n ~Dish Dash Dilemma!"; *)
  generate_row !restaurant_layout.(0) 0 (width - 1) (ref "-");
  let i = ref 1 in
  while !i < height do
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

(* running the game *)
let () =
  Lwt_main.run
    ( Lwt_unix.sleep 2. >>= fun () ->
      Lwt_io.printl "Welcome to Dish Dash Dilemma!\n " >>= fun () ->
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
      Lwt_unix.sleep 2. >>= fun () -> Menus.set_up_restaurant () );
  print_endline
    "First, enter the number of tables for the width and the height of the \
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
  read_key ();

  print_endline "Thank you for playing Dish Dash Dilemma!"
