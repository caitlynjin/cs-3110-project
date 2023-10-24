let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

(* Makes a 5 x 3 table (for 4 people each). n is the number of tables in the
   row. *)
let create_table n =
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

(* Given the number of tables horizontally and vertically, prints a rectangle of
   that size with | and - with 5 x 3 size tables inside. [num_tables] represents
   the number of tables per row and and column of this square restaurant. *)
let create_filled_restaurant num_tables =
  let width = (5 * num_tables) + (3 * (num_tables - 1)) + 8 in
  let border_string =
    String.cat (String.cat "|" (String.make (width - 2) ' ')) "|\n"
  in
  for i = 1 to width do
    print_char '-'
  done;
  print_newline ();
  print_string border_string;
  for j = 1 to num_tables do
    create_table num_tables;
    print_string border_string;
    print_string border_string
  done;
  for i = 1 to width do
    print_char '-'
  done;
  print_newline ()

let () =
  (* print_string "Enter the width: "; let width = read_int () in print_string
     "Enter the height: "; let height = read_int () in create_restaurant width
     height; *)
  print_string "Enter the number of tables: ";
  let num_tables = read_int () in
  create_filled_restaurant num_tables
