open Lwt.Infix

(** A model that sets up the restaurant, prompting the player for the name of
    the restaurant, cuisine, and menu. *)
module Menus = struct
  (* TODO: CALL THESE BEFORE ASKING PLAYER FOR TABLE SIZE *)

  (** The default name of the restaurant in case the player doesn't input one. *)
  let restaurant_name = ref "Default Restaurant Name"

  type dish = {
    name : string;
    price : float;
    ingredients : string list;
    make_time : int;
  }
  (** Type representing a dish, which includes a name, price, ingredients, and
      the time it takes to make the dish. *)

  (** The cuisine type for the restaurant, which is a string. *)
  let cuisine = ref ""

  (** The restaurants menu, which is a list of dishes. *)
  let restaurant_menu = ref []

  (** A list of cuisines that the user can choose from. *)
  let cuisine_list : string list =
    [
      "Chinese";
      "Italian";
      "American";
      "Indian";
      "Japanese";
      "Korean";
      "Spanish";
      "French";
      "Greece";
      "Thai";
    ]

  (** A string of suggestions for Chinese style dishes. *)
  let chinese_dishes_suggestions = "Dumplings, Fried Rice, Noodles, Soup, Tofu"

  (** A string of suggestions for Japanese style dishes. *)
  let japanese_dishes_suggestions = "Sushi, Ramen, Udon, Tempura, Sashimi"

  (** A string of suggestions for American style dishes. *)
  let american_dishes_suggestions = "Burgers, Steaks, Sandwiches, Fries, Pizza"

  (** A string of suggestions for Italian style dishes. *)
  let italian_dishes_suggestions = "Pasta, Pizza, Risotto, Lasagna, Gnocchi"

  (** A string of suggestions for Indian style dishes. *)
  let indian_dishes_suggestions =
    "Curry, Naan, Samosas, Tandoori Chicken, Biryani"

  (** A string of suggestions for Korean style dishes. *)
  let korean_dishes_suggestions =
    "Bibimbap, Bulgogi, Kimchi, Tteokbokki, Japchae"

  (** A string of suggestions for Spanish style dishes. *)
  let spanish_dishes_suggestions =
    "Paella, Gazpacho, Tortilla, Croquettes, Patatas Bravas"

  (** A string of suggestions for French style dishes. *)
  let french_dishes_suggestions =
    "Croissants, Baguettes, Ratatouille, Coq au Vin, Cassoulet"

  (** A string of suggestions for Greece style dishes. *)
  let greece_dishes_suggestions =
    "Moussaka, Souvlaki, Gyros, Spanakopita, Baklava"

  (** A string of suggestions for Thai style dishes. *)
  let thai_dishes_suggestions =
    "Pad Thai, Tom Yum, Som Tum, Khao Pad, Massaman Curry"

  (** The default menu for a Chinese style cuisines. *)
  let chinese_set_menu =
    [
      {
        name = "Dumplings";
        price = 5.99;
        ingredients = [ "Dough"; "Meat"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Fried Rice";
        price = 6.99;
        ingredients = [ "Rice"; "Eggs"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Noodles";
        price = 7.99;
        ingredients = [ "Noodles"; "Meat"; "Vegetables" ];
        make_time = 7;
      };
      {
        name = "Soup";
        price = 4.99;
        ingredients = [ "Broth"; "Meat"; "Vegetables" ];
        make_time = 9;
      };
      {
        name = "Tofu";
        price = 3.99;
        ingredients = [ "Tofu"; "Sauce"; "Vegetables" ];
        make_time = 4;
      };
    ]

  (** The default menu for a Japanese style cuisines. *)
  let japanese_set_menu =
    [
      {
        name = "Sushi";
        price = 9.99;
        ingredients = [ "Rice"; "Fish"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Ramen";
        price = 12.99;
        ingredients = [ "Noodles"; "Broth"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Udon";
        price = 7.99;
        ingredients = [ "Noodles"; "Broth"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Tempura";
        price = 4.99;
        ingredients = [ "Batter"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Sashimi";
        price = 10.99;
        ingredients = [ "Fish"; "Vegetables" ];
        make_time = 7;
      };
    ]

  (** The default menu for a American style cuisines. *)
  let american_set_menu =
    [
      {
        name = "Burger";
        price = 9.99;
        ingredients = [ "Bun"; "Meat"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Steak";
        price = 12.99;
        ingredients = [ "Meat"; "Sauce"; "Vegetables" ];
        make_time = 9;
      };
      {
        name = "Sandwich";
        price = 7.99;
        ingredients = [ "Bread"; "Meat"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Fries";
        price = 4.99;
        ingredients = [ "Potatoes"; "Sauce"; "Vegetables" ];
        make_time = 3;
      };
      {
        name = "Pizza";
        price = 10.99;
        ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
        make_time = 6;
      };
    ]

  (** The default menu for a Korean style cuisines. *)
  let korean_set_menu =
    [
      {
        name = "Bibimbap";
        price = 9.99;
        ingredients = [ "Rice"; "Meat"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Bulgogi";
        price = 12.99;
        ingredients = [ "Meat"; "Sauce"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Kimchi";
        price = 7.99;
        ingredients = [ "Cabbage"; "Sauce"; "Vegetables" ];
        make_time = 2;
      };
      {
        name = "Tteokbokki";
        price = 4.99;
        ingredients = [ "Rice Cake"; "Sauce"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Japchae";
        price = 10.99;
        ingredients = [ "Noodles"; "Sauce"; "Vegetables" ];
        make_time = 5;
      };
    ]

  (** The default menu for a Italian style cuisines. *)
  let italian_set_menu =
    [
      {
        name = "Pasta";
        price = 9.99;
        ingredients = [ "Pasta"; "Sauce"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Pizza";
        price = 12.99;
        ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
        make_time = 12;
      };
      {
        name = "Risotto";
        price = 7.99;
        ingredients = [ "Rice"; "Sauce"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Lasagna";
        price = 4.99;
        ingredients = [ "Pasta"; "Sauce"; "Vegetables" ];
        make_time = 15;
      };
      {
        name = "Gnocchi";
        price = 10.99;
        ingredients = [ "Potatoes"; "Sauce"; "Vegetables" ];
        make_time = 8;
      };
    ]

  (** The default menu for a Indian style cuisines. *)
  let indian_set_menu =
    [
      {
        name = "Curry";
        price = 9.99;
        ingredients = [ "Sauce"; "Meat"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Naan";
        price = 12.99;
        ingredients = [ "Bread"; "Sauce"; "Vegetables" ];
        make_time = 3;
      };
      {
        name = "Samosas";
        price = 7.99;
        ingredients = [ "Dough"; "Sauce"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Tandoori Chicken";
        price = 4.99;
        ingredients = [ "Chicken"; "Sauce"; "Vegetables" ];
        make_time = 7;
      };
      {
        name = "Biryani";
        price = 10.99;
        ingredients = [ "Rice"; "Sauce"; "Vegetables" ];
        make_time = 8;
      };
    ]

  (** The default menu for a Spanish style cuisines. *)
  let spanish_set_menu =
    [
      {
        name = "Paella";
        price = 9.99;
        ingredients = [ "Rice"; "Meat"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Gazpacho";
        price = 12.99;
        ingredients = [ "Tomatoes"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Tortilla";
        price = 7.99;
        ingredients = [ "Eggs"; "Potatoes"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Croquettes";
        price = 4.99;
        ingredients = [ "Batter"; "Meat"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Patatas Bravas";
        price = 10.99;
        ingredients = [ "Potatoes"; "Sauce"; "Vegetables" ];
        make_time = 7;
      };
    ]

  (** The default menu for a French style cuisines. *)
  let french_set_menu =
    [
      {
        name = "Croissants";
        price = 9.99;
        ingredients = [ "Dough"; "Butter"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Baguettes";
        price = 12.99;
        ingredients = [ "Dough"; "Butter"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Ratatouille";
        price = 7.99;
        ingredients = [ "Vegetables"; "Sauce"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Coq au Vin";
        price = 4.99;
        ingredients = [ "Chicken"; "Sauce"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Cassoulet";
        price = 10.99;
        ingredients = [ "Beans"; "Meat"; "Vegetables" ];
        make_time = 7;
      };
    ]

  (** The default menu for a Greece style cuisines. *)
  let greece_set_menu =
    [
      {
        name = "Moussaka";
        price = 9.99;
        ingredients = [ "Eggplant"; "Meat"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Souvlaki";
        price = 12.99;
        ingredients = [ "Meat"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Gyros";
        price = 7.99;
        ingredients = [ "Meat"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Spanakopita";
        price = 4.99;
        ingredients = [ "Spinach"; "Dough"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Baklava";
        price = 10.99;
        ingredients = [ "Nuts"; "Dough"; "Vegetables" ];
        make_time = 7;
      };
    ]

  (** The default menu for a Thai style cuisines. *)
  let thai_set_menu =
    [
      {
        name = "Pad Thai";
        price = 9.99;
        ingredients = [ "Noodles"; "Meat"; "Vegetables" ];
        make_time = 8;
      };
      {
        name = "Tom Yum";
        price = 12.99;
        ingredients = [ "Broth"; "Meat"; "Vegetables" ];
        make_time = 4;
      };
      {
        name = "Som Tum";
        price = 7.99;
        ingredients = [ "Papaya"; "Vegetables" ];
        make_time = 6;
      };
      {
        name = "Khao Pad";
        price = 4.99;
        ingredients = [ "Rice"; "Meat"; "Vegetables" ];
        make_time = 5;
      };
      {
        name = "Massaman Curry";
        price = 10.99;
        ingredients = [ "Curry"; "Meat"; "Vegetables" ];
        make_time = 7;
      };
    ]

  (** Converts a list into string. *)
  let string_of_list lst =
    let rec string_of_list_helper lst acc =
      match lst with
      | [] -> acc
      | h :: t -> string_of_list_helper t (acc ^ ", " ^ h)
    in
    match lst with
    | [] -> ""
    | h :: t -> string_of_list_helper t h

  (** Reads user input for the name of the restaurant. Modifies the name of the
      restaurant to the user input. *)
  let name_res () =
    print_endline "What would you like to name your restaurant?";
    let input = read_line () in
    restaurant_name := input;
    print_endline ("\n Your restaurant is called " ^ input ^ "! \n");
    Lwt_unix.sleep 2.

  (** Reads the user input for a float type. If not, prints in console that it
      is invalid and continues reading for user input until valid. *)
  let rec read_float () =
    try float_of_string (read_line ())
    with Failure _ ->
      print_endline "Invalid input. Please enter a number.";
      read_float ()

  (** Prompts the user to choose a cuistine type for the restaurant, and
      continues to read for a valid input unless the user inputs exit. *)
  let rec read_key () =
    Lwt_io.printl
      ("Next, choose a type of cuisine for your restaurant, from these \
        options: "
      ^ string_of_list cuisine_list)
    >>= fun () ->
    Lwt_io.printl "Or, type 'random' to get a random cuisine." >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    let input = read_line () in
    let cuisine_announcement = "\nThe cuisine style of your restaurant is " in
    let input = String.lowercase_ascii input in
    if
      input = "chinese" || input = "italian" || input = "american"
      || input = "indian" || input = "japanese" || input = "korean"
      || input = "spanish" || input = "french" || input = "greece"
      || input = "thai"
    then begin
      cuisine := input;
      print_endline (cuisine_announcement ^ !cuisine ^ ". \n");
      Lwt_unix.sleep 1.
    end
    else if input = "random" || input = "exit" then begin
      let random_cuisine =
        List.nth cuisine_list (Random.int (List.length cuisine_list))
      in
      cuisine := random_cuisine;
      print_endline (cuisine_announcement ^ !cuisine ^ ".");
      Lwt_unix.sleep 2.
    end
    else begin 
      print_endline
        "\nPlease enter a valid cuisine style, or type \"exit\" to quit. ";
      read_key ()
    end

  (** If a player wants ideas for what dishes to put, returns a list of possible
      dishes for their chosen cuisine as suggestions. *)
  let suggest_menus cuisine =
    let cuisine = String.lowercase_ascii cuisine in
    if cuisine = "chinese" then chinese_dishes_suggestions
    else if cuisine = "japanese" then japanese_dishes_suggestions
    else if cuisine = "american" then american_dishes_suggestions
    else if cuisine = "italian" then italian_dishes_suggestions
    else if cuisine = "indian" then indian_dishes_suggestions
    else if cuisine = "korean" then korean_dishes_suggestions
    else if cuisine = "spanish" then spanish_dishes_suggestions
    else if cuisine = "french" then french_dishes_suggestions
    else if cuisine = "greece" then greece_dishes_suggestions
    else if cuisine = "thai" then thai_dishes_suggestions
    else failwith "Invalid cuisine"

  (** If player wants a pre-determined menu, set it based on the cuisine they
      have chosen. Otherwise, fails with an invalid cuisine. *)
  let set_menu input =
    let input = String.lowercase_ascii input in
    if input = "chinese" then chinese_set_menu
    else if input = "japanese" then japanese_set_menu
    else if input = "american" then american_set_menu
    else if input = "italian" then italian_set_menu
    else if input = "indian" then indian_set_menu
    else if input = "korean" then korean_set_menu
    else if input = "spanish" then spanish_set_menu
    else if input = "french" then french_set_menu
    else if input = "greece" then greece_set_menu
    else if input = "thai" then thai_set_menu
    else failwith "Invalid cuisine"

  (** Adds the dish to the restaurant menu, which contains a name [dName], price
      [dPrice], ingredients [dIngredients], and make time [dTime]. *)
  let make_dish dName dPrice dIngredients dTime =
    restaurant_menu :=
      {
        name = dName;
        price = dPrice;
        ingredients = dIngredients;
        make_time = dTime;
      }
      :: !restaurant_menu

  (** Converts the menu [menu] into a string. *)
  let rec menu_to_string menu =
    let dish_to_string dish =
      dish.name ^ ": $" ^ string_of_float dish.price ^ "\n    ingredients: "
      ^ string_of_list dish.ingredients
      ^ "\n"
    in
    match menu with
    | [] -> ""
    | h :: t -> dish_to_string h ^ menu_to_string t

  (** Prompts the user to make their own menu, or selects from the
      pre-determined menus. *)
  let rec make_menu () =
    let input = read_line () in
    if input = "standard" then begin
      restaurant_menu := set_menu !cuisine
      (* print_endline ("The menu for your " ^ !cuisine ^ " restaurant is: \n" ^
         string_of_list (List.map (fun x -> x.name) !restaurant_menu) ^ ".") *);
      Lwt.return ()
    end
    else if input = "suggest" then begin
      print_endline
        ("Here are some suggestions for dishes to add to your menu: \n"
       ^ suggest_menus !cuisine
       ^ "\n\
         \ Please enter a dish you want to include on your menu, or type \
          'done' if you finished making your menu. ");
      make_menu ()
    end
    else if input = "done" then begin
      if !restaurant_menu = [] then begin
        restaurant_menu := set_menu !cuisine;
        print_endline
          ("The menu for your " ^ !cuisine ^ " restaurant is: \n"
          ^ string_of_list (List.map (fun x -> x.name) !restaurant_menu)
          ^ ".");
        Lwt.return ()
      end
      else Lwt.return ()
    end
    else
      let set_dishes =
        let dish_name = input in
        print_string "\n What is the price of this dish? ";
        let dish_price = read_float () in
        print_endline "\n What are the ingredients of this dish? ";
        let dish_ingredients = read_line () in
        print_endline "\n How long does it take to make this dish? ";
        let dish_time : int =
          let rec set_time () : int =
            try int_of_string (read_line ())
            with _ ->
              print_string "Please enter an integer: ";
              set_time ()
          in
          set_time ()
        in
        make_dish dish_name dish_price [ dish_ingredients ] dish_time;
        print_endline
          "\n\
           Please enter another dish you want to include on your menu, or type \
           'done' if you finished making your menu. ";
        make_menu ()
      in
      set_dishes

  (** A sequence of prompts for the user to set up the restaurant, including the
      restaurant name, type, and menu. *)
  let set_up_restaurant () =
    Lwt_io.printl "Let's set up our restaurant!" >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    name_res () >>= fun () ->
    read_key () >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    Lwt_io.printl "Now, let's make the menu! What dishes do you want to serve?"
    >>= fun () ->
    Lwt_io.printl
      "  Or, type 'suggest' to see some suggestions of dishes to add, or \
       'standard' to use a pre-determined menu. \n"
    >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    make_menu () >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    let menu_str = menu_to_string !restaurant_menu in
    Lwt_io.printl "\nYour full menu is:" >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    Lwt_io.printl menu_str >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    Lwt_io.printl "Now, let's open the restaurant! \n" >>= fun () ->
    Lwt_unix.sleep 2.
end
