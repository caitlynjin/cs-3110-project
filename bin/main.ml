(* open Restaurant *)

let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

let rec read_key () =
  print_string
    "Press the return bar to get the next party in line (Type \"exit\" to \
     quit): ";
  let input = read_line () in
  if input = "" then
    let party_size = 1 + Random.int 10 in
    print_endline
      ("Next in line is a party of " ^ string_of_int party_size ^ ".")
  else if input = "exit" then exit 0
  else print_endline "Please press the return bar or type \"exit\" to quit. ";
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

let empty_restaurant size = Array.make size (ref "")

let generate_line length c =
  let generate length acc =
    for _ = 1 to length do
      acc := !acc ^ c
    done;
    acc
  in
  generate length (ref "")

let create_table_end row width =
  row := "|   ";
  for _ = 1 to width do
    row := !row ^ " ---    "
  done;
  row := !row ^ "|"
(* let table_middle_row row width = row := "| "; for _ = 1 to width do row :=
   !row ^ "| | " done; row := "|" *)

let create_filled_restaurant2 num_tables =
  let width = (5 * num_tables) + (3 * (num_tables - 1)) + 8 in
  let height = num_tables * 5 in
  let restaurant = empty_restaurant (height + 2) in
  restaurant.(0) <- generate_line width "-";
  let i = ref 1 in
  while !i < height do
    restaurant.(!i) <- ref ("|" ^ !(generate_line (width - 2) " ") ^ "|");
    create_table_end restaurant.(!i + 1) num_tables;
    restaurant.(!i + 2) <-
      ref ("| " ^ !(generate_line num_tables "  |   | ") ^ "  |");
    create_table_end restaurant.(!i + 3) num_tables;
    restaurant.(!i + 4) <- ref ("|" ^ !(generate_line (width - 2) " ") ^ "|");
    i := !i + 5
  done;
  restaurant.(Array.length restaurant - 1) <- generate_line width "-";
  for i = 0 to Array.length restaurant - 1 do
    print_string (!(restaurant.(i)) ^ "\n")
  done;
  ()

let () =
  print_string "Enter the number of tables: ";
  let n = read_int () in
  (* create_filled_restaurant1 n; *)
  create_filled_restaurant2 n;
  read_key ()
