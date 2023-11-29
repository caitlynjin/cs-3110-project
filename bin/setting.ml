let restaurant = ref [| "" |]

let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

let rec read_key () =
  print_string "Press the space bar to get the next party in line: ";
  if read_line () = " " then
    let party_size = 1 + Random.int 10 in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".")
  else
    raise
      (Failure
         "Invalid input. Please enter a valid command. \n\
          COMMANDS KEY:\n\
         \    ' ' - Check the size of the next party in line.");
  read_key ()

(* Makes n 5 x 3 tables (for 4 people each). n is the number of tables in the
   row. *)
let create_row n =
  for i = 1 to 3 do
    print_string "|   ";
    for k = 1 to n do
      for j = 1 to 5 do
        if (i = 1 || i = 3) && (j = 1 || j = 5) then print_char ' '
        else if i = 1 || i = 3 then print_char '-'
        else if j = 1 || j = 5 then print_char '|'
        else print_char ' '
      done;
      if k < n then print_string "   " else print_string "   | \n"
    done
    (* print_string "\t |" *)
  done

(* Updates a row of n 5 x 3 tables (for 4 people each). n is the number of
   tables in the row. table_id is which table the people will be placed at.
   people is the number of people to be placed at the table. *)
let fill_row n table_id =
  (* num_filled: number of people to be placed *)
  (* let num_filled = people; *)
  for i = 1 to 3 do
    print_string "|   ";
    for k = 1 to n do
      let person = if k = table_id then "X" else " " in
      for j = 1 to 5 do
        if (i = 1 || i = 3) && (j = 1 || j = 5) then print_char ' '
        else if i = 1 || i = 3 then print_char '-'
        else if j = 1 || j = 5 then print_char '|'
        else print_char ' '
          (* if want to put food on the table, replace here^ *)
      done;
      if k < n then print_string (" " ^ person ^ " ")
      else print_string "   | \n"
    done
    (* print_string "\t |" *)
  done

(* left-wall- generates a person sitting at leftmost seat of the leftmost table
   if at the right row(middle of a table) or just a wall if not *)
let _left_wall table_id place_person row =
  if table_id = 1 && place_person && row mod 3 = 2 then "|  x" else "|   "

(* right-wall generates a person sitting at rightmost seat of the rightmost
   table if at the right row(middle of a table) or just a wall if not *)
let _right_wall n table_id place_person row =
  if table_id = n && place_person && row mod 3 = 2 then "x  |" else "   |"

(* NEED TO REPEAT- CALL FOR EVERY SPACE BETWEEN TABLES (so n+1 times)
generates a person sitting at (left or right) seat of table if at the right row(middle of a
   table) or just the appropriate amount of spaces if not *)
let _side_of_table current table_id place_person row =
  if table_id = current && place_person && row mod 3 = 2 then " x " else "   "

(* DO NOT NEED TO REPEAT, RETURNS WHOLE ROW
generates the top and bottom edges of n tables, not including the spacing and
   | that represents the restaurant walls *)
let _table_top_bottom n =
  let rec multiply_string x str =
    if x = 0 then "" else str ^ multiply_string (x - 1) str
  in
  multiply_string (n - 1) " ---    " ^ " --- "

(* generates the top and bottom spaces of n tables, not including the spacing and
   | that represents the restaurant walls *)
let _place_people_top_botton n table_id place_person row =
  let rec multiply_string x str =
    if x = 0 then "" else str ^ multiply_string (x - 1) str
  in
  if table_id = n && place_person && row mod 3 = 2 then
    multiply_string (n - 1) " ---    " ^ " --- "
  else multiply_string (n - 1) " ---    " ^ " --- "

  (* pieces together above fxns and places into restaurant, dict = dict with table_ids as keys and int of people to seat *)
  (* let generate_restaurant n dict =
    restaurant := [|_table_top_bottom n|] *)

(* generates the left and right edges of n tables, not including the spacing and
   | that represents the restaurant walls *)

(* (* generates a person sitting at right seat of table if at the right
   row(middle of a table) or just the appropriate amount of spaces if not *) let
   _right_of_table current table_id place_person row = if table_id = current &&
   place_person && row mod 3 = 2 then " x " else " " *)

(* Given the number of tables horizontally and vertically, prints a rectangle of
   that size with | and - with 5 x 3 size tables inside. [num_tables] represents
   the number of tables per row and and column of this square restaurant. *)

   (* SPACE FROM WALL TO TABLE: 4
      TABLE DIMENSIONS (5 x 3): 
        width: 1 space + 3'-' + 1 space
        height: '|' with 1 space above and below, generated in row ^ 
        inside table: 3 spaces
      SPACE FROM TABLE TO TABLE: 3 spaces *)
let _create_filled_restaurant num_tables =
  let width = (5 * num_tables) + (3 * (num_tables - 1)) + 8 in
  let border_string =
    String.cat (String.cat "|" (String.make (width - 2) ' ')) "|\n"
  in
  for _ = 1 to width do
    print_char '-'
  done;
  print_newline ();
  print_string border_string;

  for _ = 1 to num_tables do
    create_row num_tables;
    print_string border_string;
    print_string border_string
  done;
  let queue_length = 1 + Random.int 10 in
  print_string "| \t";
  for _ = 1 to queue_length do
    print_char '*'
  done;
  print_newline ();
  for _ = 1 to width do
    print_char '-'
  done;
  print_newline ()

let print_array arr =
  Array.iter (fun x -> print_string x) arr;
  print_newline ()

let () =
  print_string "Enter the number of tables: ";
  let num_tables = read_int () in

  (* create_filled_restaurant num_tables; *)
  print_string "Enter the table id: ";
  let num_people = read_int () in
  fill_row num_tables num_people;
  print_array !restaurant;
  read_key ()
