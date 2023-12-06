let cuisine_list : string list =
  [ "Chinese"; "Italian"; "American"; "Indian"; "Japanese"; "Korean" ]

let cuisine = ref ""

(* menu type *)
type menu = {
  name : string;
  price : float;
  ingredients : string list;
}

(* turn list into string *)
let string_of_list lst =
  let rec string_of_list_helper lst acc =
    match lst with
    | [] -> acc
    | h :: t -> string_of_list_helper t (acc ^ ", " ^ h)
  in
  match lst with
  | [] -> ""
  | h :: t -> string_of_list_helper t h

let rec read_key () =
  print_endline
    ("Choose a type of cuisine for your restaurant, from these options: "
    ^ string_of_list cuisine_list);
  print_endline "Or, type 'random' to get a random cuisine.";
  let input = read_line () in
  let cuisine_announcement = "The cuisine style of your restaurant is " in
  if input = "Chinese" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "Italian" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "American" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "Indian" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "Japanese" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "Korean" then begin
    cuisine := input;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "random" then begin
    let random_cuisine =
      List.nth cuisine_list (Random.int (List.length cuisine_list))
    in
    cuisine := random_cuisine;
    print_endline (cuisine_announcement ^ !cuisine ^ ".")
  end
  else if input = "exit" then exit 0
  else print_endline "Please enter a cuisine style, or type \"exit\" to quit. ";
  read_key ()

let () = read_key ()
