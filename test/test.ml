open Restaurant.Table

module Rest = struct
  (* type t = string ref array array ref *)

  let restaurant_layout = ref (Array.make 0 (Array.make 0 (ref "")))
  let table_list = ref []
  let set_coord_symbol row col sym = row.(col) <- ref sym
  let add_table_list id table = table_list := (id, table) :: !table_list
  let get_table id = List.assoc id !table_list

  exception SizeError
  exception TableOccupied

  (* changes the first nth sseats of table id to sym *)
  let change_seats_sym id n sym =
    for i = 0 to n - 1 do
      let x, y = List.nth (Table.coord_list (get_table id)) i in
      set_coord_symbol !restaurant_layout.(x) y sym
    done

  let width = ref 0
  let height = ref 0

  (* creates a restaurant based on the number of tables *)
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

    (* creates the end of tables and adds coordiantes to each table in the
       list *)
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
    (* sets the next to last row to an empty row and the final row of the array
       to just "-" *)
    generate_row !restaurant_layout.(!height - 2) 1 (!width - 1) (ref " ");
    generate_row !restaurant_layout.(!height - 1) 0 !width (ref "-")

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

  (* prints out the table list such that "table #'s coordinates are (#,
     #)(#,#)..." *)
  let print_list () =
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

  (* seat_party will modify the corresponding rows of table_id and place 'x's
     around that table to represent the people in the party. [num_people] = the
     number of x's to place around the table [table_id] = the table to place the
     people at *)
  let seat_party num_people table_id =
    if Table.isReady (get_table table_id) then
      if num_people > Table.capacity (get_table table_id) then raise SizeError
      else (
        Table.seat (get_table table_id) num_people;
        change_seats_sym table_id num_people "*")
    else raise TableOccupied

  let get_table_list = !table_list
  let get_coord_value x y = !(!restaurant_layout.(x).(y))
end
