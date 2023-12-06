(* SETTING UP THE RESTAURANT: prompting player for the name of the restaurant,
  cuisine, and menu. *)

(* TODO: CALL THESE BEFORE ASKING PLAYER FOR TABLE SIZE ?? *)

(* dishes type *)
type dish = {
  name : string;
  price : float;
  ingredients : string list; (* TODO: ADD THIS ? make_time: int *)
}

type menu = dish list

let cuisine = ref ""
let restaurant_menu = ref []

(* list of cuisines that player can choose from *)
let cuisine_list : string list =
  [ "Chinese"; "Italian"; "American"; "Indian"; "Japanese"; "Korean" ]

(* suggestions for dishes *)
let chinese_dishes_suggestions = "Dumplings, Fried Rice, Noodles, Soup, Tofu"
let japanese_dishes_suggestions = "Sushi, Ramen, Udon, Tempura, Sashimi"
let american_dishes_suggestions = "Burgers, Steaks, Sandwiches, Fries, Pizza"
let italian_dishes_suggestions = "Pasta, Pizza, Risotto, Lasagna, Gnocchi"
let indian_dishes_suggestions =
  "Curry, Naan, Samosas, Tandoori Chicken, Biryani"
let korean_dishes_suggestions = "Bibimbap, Bulgogi, Kimchi, Tteokbokki, Japchae"

(* pre-determined menus *)
let chinese_set_menu =
  [
    {
      name = "Dumplings";
      price = 5.99;
      ingredients = [ "Dough"; "Meat"; "Vegetables" ];
    };
    {
      name = "Fried Rice";
      price = 6.99;
      ingredients = [ "Rice"; "Eggs"; "Vegetables" ];
    };
    {
      name = "Noodles";
      price = 7.99;
      ingredients = [ "Noodles"; "Meat"; "Vegetables" ];
    };
    {
      name = "Soup";
      price = 4.99;
      ingredients = [ "Broth"; "Meat"; "Vegetables" ];
    };
    {
      name = "Tofu";
      price = 3.99;
      ingredients = [ "Tofu"; "Sauce"; "Vegetables" ];
    };
  ]

let japanese_set_menu =
  [
    {
      name = "Sushi";
      price = 9.99;
      ingredients = [ "Rice"; "Fish"; "Vegetables" ];
    };
    {
      name = "Ramen";
      price = 8.99;
      ingredients = [ "Noodles"; "Eggs"; "Vegetables" ];
    };
    {
      name = "Udon";
      price = 7.99;
      ingredients = [ "Noodles"; "Meat"; "Vegetables" ];
    };
    {
      name = "Tempura";
      price = 6.99;
      ingredients = [ "Batter"; "Meat"; "Vegetables" ];
    };
    {
      name = "Sashimi";
      price = 10.99;
      ingredients = [ "Fish"; "Sauce"; "Vegetables" ];
    };
  ]

let american_set_menu =
  [
    {
      name = "Burger";
      price = 9.99;
      ingredients = [ "Bun"; "Meat"; "Vegetables" ];
    };
    {
      name = "Steak";
      price = 12.99;
      ingredients = [ "Meat"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Sandwich";
      price = 7.99;
      ingredients = [ "Bread"; "Meat"; "Vegetables" ];
    };
    {
      name = "Fries";
      price = 4.99;
      ingredients = [ "Potatoes"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Pizza";
      price = 10.99;
      ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
    };
  ]

let korean_set_menu =
  [
    {
      name = "Bibimbap";
      price = 9.99;
      ingredients = [ "Rice"; "Meat"; "Vegetables" ];
    };
    {
      name = "Bulgogi";
      price = 12.99;
      ingredients = [ "Meat"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Kimchi";
      price = 7.99;
      ingredients = [ "Cabbage"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Tteokbokki";
      price = 4.99;
      ingredients = [ "Rice Cake"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Japchae";
      price = 10.99;
      ingredients = [ "Noodles"; "Sauce"; "Vegetables" ];
    };
  ]

let italian_set_menu =
  [
    {
      name = "Pasta";
      price = 9.99;
      ingredients = [ "Pasta"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Pizza";
      price = 12.99;
      ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Risotto";
      price = 7.99;
      ingredients = [ "Rice"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Lasagna";
      price = 4.99;
      ingredients = [ "Pasta"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Gnocchi";
      price = 10.99;
      ingredients = [ "Potatoes"; "Sauce"; "Vegetables" ];
    };
  ]

let indian_set_menu =
  [
    {
      name = "Curry";
      price = 9.99;
      ingredients = [ "Sauce"; "Meat"; "Vegetables" ];
    };
    {
      name = "Naan";
      price = 12.99;
      ingredients = [ "Bread"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Samosas";
      price = 7.99;
      ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Tandoori Chicken";
      price = 4.99;
      ingredients = [ "Chicken"; "Sauce"; "Vegetables" ];
    };
    {
      name = "Biryani";
      price = 10.99;
      ingredients = [ "Rice"; "Sauce"; "Vegetables" ];
    };
  ]

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

(* ADD STEP TO NAME THE RESTAURANT *)
let rec read_key () =
  print_endline
    ("Choose a type of cuisine for your restaurant, from these options: "
    ^ string_of_list cuisine_list);
  print_endline "Or, type 'random' to get a random cuisine.";
  let input = read_line () in
  let cuisine_announcement = "The cuisine style of your restaurant is " in
  if
    input = "Chinese" || input = "Italian" || input = "American"
    || input = "Indian" || input = "Japanese" || input = "Korean"
  then begin
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
  else print_endline "Please enter a valid cuisine style, or type \"exit\" to quit. ";
  (* TODO: if exits before choosing a cuisine, set a random one *)
  read_key ()

(* if player wants a pre-determined menu, set it based on the cuisine they have
   chosen. *)
let set_menu input =
  if input = "Chinese" then chinese_set_menu
  else if input = "Japanese" then japanese_set_menu
  else if input = "American" then american_set_menu
  else if input = "Italian" then italian_set_menu
  else if input = "Indian" then indian_set_menu
  else if input = "Korean" then korean_set_menu
  else failwith "Invalid cuisine"

(* adds dish to menu *)
let rec make_dish dName dPrice dIngredients =
  restaurant_menu :=
    { name = dName; price = dPrice; ingredients = dIngredients }
    :: !restaurant_menu

(* turns menu into a string *)
let rec menu_to_string menu =
  let dish_to_string dish =
    dish.name ^ ": $" ^ string_of_float dish.price ^ "\n ingredients: "
    ^ string_of_list dish.ingredients
    ^ "\n"
  in
  match menu with
  | [] -> ""
  | h :: t -> dish_to_string h ^ menu_to_string t

(* prompts player to make their own menu, or select from the pre-determined menus. *)
let rec make_menu () =
  let input = read_line () in
  if input = "standard" then begin
    restaurant_menu := set_menu !cuisine;
    print_endline
      ("The menu for your " ^ !cuisine ^ " restaurant is: \n"
      ^ string_of_list (List.map (fun x -> x.name) !restaurant_menu)
      ^ ".")
  end
  else if input = "done" then exit 0
  else
    let dish_name = input in
    print_string "What is the price of this dish? ";
    let dish_price = read_float () in
    print_endline "What are the ingredients of this dish? ";
    let dish_ingredients = read_line () in
    make_dish dish_name dish_price [ dish_ingredients ];
    print_endline
      "Please enter another dish you want to include on your menu, or type \
       'done' if you finished making your menu. ";
    make_menu ()

let () =
  print_endline "Let's set up our restaurant!";
  (* read_key () *)
  print_endline
    "Now, let's make the menu! What dishes do you want to serve?\n\
    \   \n\
    \  Or, type 'standard' to get a pre-determined menu. \n";
  (* read_menu (); *)
  print_endline "Your menu is: \n" ^ menu_to_string !restaurant_menu ^ ".";
  print_endline "Now, let's open the restaurant! \n"
