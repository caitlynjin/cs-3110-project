let rec read_int () =
  try int_of_string (read_line ())
  with Failure _ ->
    print_endline "Invalid input. Please enter an integer.";
    read_int ()

(* Given the width and height, prints a rectangle of that size with | and -. *)
let create_restaurant width height =
  for i = 1 to height do 
    for j = 1 to width do
      if (i = 1 || i = height) && (j = 1 || j = width) then print_char ' '
      else if i = 1 || i = height then print_char '-'
      else if j = 1 || j = width then print_char '|'
      else print_char ' '
    done;
    print_newline ()
  done

let () =
  print_string "Enter the width: ";
  let width = read_int () in
  print_string "Enter the height: ";
  let height = read_int () in
  create_restaurant width height
