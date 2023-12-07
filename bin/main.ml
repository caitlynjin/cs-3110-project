open Lwt.Infix
open Restaurant
open Table

(* let restaurant_data = (int * Table.t * TableQueue.t) list *)
let restaurant_layout = ref (Array.make 0 (Array.make 0 (ref "")))
let table_list = ref []
let set_coord_symbol row col sym = row.(col) <- ref sym
let add_table_list id table = table_list := (id, table) :: !table_list
let get_table id = List.assoc id !table_list

(* changes the first nth seats of table id to sym *)
let change_seats_sym id n sym =
  for i = 0 to n - 1 do
    let x, y = List.nth (Table.coord_list (get_table id)) i in
    set_coord_symbol !restaurant_layout.(x) y sym
  done

let width = ref 0
let height = ref 0

(* prints out the table list such that "table #'s coordinates are (#,
   #)(#,#)..." *)
let _print_list () =
  let print_coord acc (h, w) =
    acc ^ "(" ^ string_of_int h ^ ", " ^ string_of_int w ^ ")"
  in
  let print_coord_list lst = List.fold_left print_coord "" lst in
  let print_entry (id, table) =
    print_string
      ("table " ^ string_of_int id ^ "'s coordinates are "
      ^ print_coord_list (Table.coord_list table)
      ^ "\n")
  in
  List.iter print_entry !table_list

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

let make_restaurant num_tables =
  (* sets width and height *)
  width := (5 * num_tables) + (3 * (num_tables - 1)) + 8;
  height := (num_tables * 5) + 2;

  (* creates the tables *)
  for n = 1 to num_tables * num_tables do
    add_table_list n (Table.make 4 4)
  done;

  let current_table = ref 1 in

  (* helper function to help generate row *)
  let generate_row row i1 i2 c =
    for i = i1 to i2 - 1 do
      row.(i) <- c
    done;
    if i1 <> 0 then (
      row.(0) <- ref "|";
      row.(i2) <- ref "|")
  in

  (* creates the end of tables and adds coordiantes to each table in the list *)
  let create_table_end row width h =
    (row.(0) <- ref "|";
     row.(1) <- ref " ";
     row.(2) <- ref " ";
     row.(3) <- ref " ";

     let i = ref 4 in
     while !i < width - 8 do
       row.(!i) <- ref " ";
       row.(!i + 1) <- ref "-";
       row.(!i + 2) <- ref "-";
       Table.add_list (get_table !current_table) h (!i + 2);
       row.(!i + 3) <- ref "-";
       row.(!i + 4) <- ref " ";
       row.(!i + 5) <- ref " ";
       row.(!i + 6) <- ref " ";
       row.(!i + 7) <- ref " ";
       i := !i + 8;
       current_table := !current_table + 1
     done;
     row.(width - 1) <- ref "|");
    current_table := !current_table - num_tables
  in

  (* creates the middle row of the table (the | | of each table) *)
  let table_middle_row row width h =
    ((* sets the left border of the row *)
     row.(0) <- ref "|";
     row.(1) <- ref " ";
     (* manually does the | | thing *)
     let i = ref 2 in
     while !i < width - 8 do
       row.(!i) <- ref " ";
       row.(!i + 1) <- ref " ";
       row.(!i + 2) <- ref "|";
       Table.add_list (get_table !current_table) h (!i + 2);
       row.(!i + 3) <- ref " ";
       row.(!i + 4) <- ref " ";
       row.(!i + 5) <- ref " ";
       row.(!i + 6) <- ref "|";
       Table.add_list (get_table !current_table) h (!i + 6);

       row.(!i + 7) <- ref " ";
       i := !i + 8;
       current_table := !current_table + 1
     done;
     (* sets the right border of the row *)
     row.(width - 3) <- ref " ";
     row.(width - 2) <- ref " ";
     row.(width - 1) <- ref "|");
    current_table := !current_table - num_tables
  in

  restaurant_layout := Array.make !height (Array.make !width (ref ""));

  for n = 0 to !height - 2 do
    !restaurant_layout.(n) <- Array.make !width (ref "")
  done;

  (* restaurant_layout.(0) <- ref "\n ~Dish Dash Dilemma!"; *)

  (* first sets the first row of the array to just "-" *)
  generate_row !restaurant_layout.(0) 0 !width (ref "-");
  let i = ref 1 in
  while !i < !height - 2 do
    generate_row !restaurant_layout.(!i) 1 (!width - 1) (ref " ");
    create_table_end !restaurant_layout.(!i + 1) !width (!i + 1);
    table_middle_row !restaurant_layout.(!i + 2) !width (!i + 2);
    create_table_end !restaurant_layout.(!i + 3) !width (!i + 3);
    generate_row !restaurant_layout.(!i + 4) 1 (!width - 1) (ref " ");
    i := !i + 5;
    current_table := !current_table + num_tables
  done;
  (* sets the next to last row to an empty row and the final row of the array to
     just "-" *)
  generate_row !restaurant_layout.(!height - 2) 1 (!width - 1) (ref " ");
  generate_row !restaurant_layout.(!height - 1) 0 !width (ref "-")

(* fill_restaurant will modify the corresponding rows of table_id and place 'x's
   around that table to represent the people in the party. [num_people] = the
   number of x's to place around the table [table_id] = the table to place the
   people at *)
let _fill_restaurant num_people table_id =
  if num_people > Table.capacity (get_table table_id) then
    failwith "too much people"
  else (
    Table.seat (get_table table_id) num_people;
    change_seats_sym table_id num_people "*")

let display_filled_restaurant () =
  (* prints everything out *)
  for i = 0 to !height - 1 do
    for j = 0 to !width - 1 do
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

let setup_num_tables () =
  print_endline
    "First, enter the number of tables for the width and the height of the \
     restaurant: ";
  make_restaurant (read_int ());
  display_filled_restaurant ();

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
  read_key ();

  print_endline "Thank you for playing Dish Dash Dilemma!"
