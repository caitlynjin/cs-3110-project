(* open Restaurant *)

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

(* Makes a 5 x 3 table (for 4 people each). n is the number of tables in the
   row. *)
(* let create_table n = for i = 1 to 3 do print_string "| "; for k = 1 to n do
   for j = 1 to 5 do if (i = 1 || i = 3) && (j = 1 || j = 5) then print_char ' '
   else if i = 1 || i = 3 then print_char '-' else if j = 1 || j = 5 then
   print_char '|' else print_char ' ' done; if k < n then print_string " " else
   print_string " | \n" done done *)

(* Given the number of tables horizontally and vertically, prints a rectangle of
   that size with | and - with 5 x 3 size tables inside. [num_tables] represents
   the number of tables per row and and column of this square restaurant. *)
(* let create_filled_restaurant1 num_tables = let width = (5 * num_tables) + (3
   * (num_tables - 1)) + 8 in let border_string = String.cat (String.cat "|"
   (String.make (width - 2) ' ')) "|\n" in for _ = 1 to width do print_char '-'
   done; print_newline (); print_string border_string;

   for _ = 1 to num_tables do create_table num_tables; print_string
   border_string; print_string border_string done; let queue_length = 1 +
   Random.int 10 in print_string "| \t"; for _ = 1 to queue_length do print_char
   '*' done; print_newline (); for _ = 1 to width do print_char '-' done;
   print_newline () *)

(* Initializes array representation of restaurant to an empty string array of
   length [size], where [size] represents the height of the restaurant. *)
let empty_restaurant size = Array.make size (ref "")

(* Concatenates the substring [s] to the empty string [length] times, returning
   a string that is [length] characters long. Requires [s] to be of length 1. *)
let generate_line length s =
  let generate length acc =
    for _ = 1 to length do
      acc := !acc ^ s
    done;
    acc
  in
  generate length (ref "")

(* Helper function to create rows for top and bottom edges of tables. *)
let create_table_end row width =
  row := "|   ";
  for _ = 1 to width do
    row := !row ^ " ---    "
  done;
  row := !row ^ "|"

(* Modifies string array produced by function [empty_restaurant] to contain [n]
   by [n] restaurant layout with [n * n] tables, and prints restaurant after
   updating array. For example, passing [4] to create_filled_restaurant will
   produce a restaurant of 4 x 4 = 16 tables. Because each table is a 5 * 3
   square, ach iteration of the while loop modifies five elements of the
   restaurant array. *)
let create_filled_restaurant2 n =
  let length = (5 * n) + (3 * (n - 1)) + 8 in
  let width = n * 5 in
  let restaurant = empty_restaurant (width + 2) in
  restaurant.(0) <- generate_line length "-";
  let i = ref 1 in
  while !i < width do
    restaurant.(!i) <- ref ("|" ^ !(generate_line (length - 2) " ") ^ "|");
    create_table_end restaurant.(!i + 1) n;
    restaurant.(!i + 2) <- ref ("| " ^ !(generate_line n "  |   | ") ^ "  |");
    create_table_end restaurant.(!i + 3) n;
    restaurant.(!i + 4) <- ref ("|" ^ !(generate_line (length - 2) " ") ^ "|");
    i := !i + 5
  done;
  restaurant.(Array.length restaurant - 1) <- generate_line length "-";
  for i = 0 to Array.length restaurant - 1 do
    print_string (!(restaurant.(i)) ^ "\n")
  done;
  ()

(* Main function to handle user input and output. Queries user for the number of
   tables they would like their restaurant to contain along its length and
   width. *)
let () =
  print_string "Enter the number of tables: ";
  let num_tables = read_int () in
  (* create_filled_restaurant1 n; *)
  create_filled_restaurant2 num_tables;
  read_key ()
