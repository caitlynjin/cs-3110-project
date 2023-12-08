open Table

(** A model that lays out the restaurant user interface. *)
module Rest = struct
  type t = string ref array array ref
  (** Type representing the 2D array of tables. *)

  (** The restaurant's layout, which is represented by a 2D array. *)
  let restaurant_layout = ref (Array.make 0 (Array.make 0 (ref "")))

  (** The list of tables represented by a 2D array of table id's the table
      itself. *)
  let table_list = ref []

  (** Sets the symbol [sym] to the coordinate at row [row] and column [col]. *)
  let set_coord_symbol row col sym = row.(col) <- ref sym

  (** Adds the table with the [id] to the list of tables [table_list]. *)
  let add_table_list id table = table_list := (id, table) :: !table_list

  (** Gets the table with the corresponding [id]. *)
  let get_table id = List.assoc_opt id !table_list

  exception SizeError
  exception TableOccupied
  exception TableNotFound

  (** Changes the first [n]th seats of the table [id] to the following symbol
      [sym]. *)
  let change_seats_sym id n sym =
    match get_table id with
    | Some table ->
        for i = 0 to n - 1 do
          let x, y = List.nth (Table.coord_list table) i in
          set_coord_symbol !restaurant_layout.(x) y sym
        done
    | None -> raise TableNotFound

  (** Resets the table [id] to its default state such that . This only changes
      the appearance of the coordinates of the table, does not modify any other
      aspects of the table *)
  let reset_table id =
    match get_table id with
    | Some table ->
        let b1, b2 = List.nth (Table.coord_list table) 0 in
        let t1, t2 = List.nth (Table.coord_list table) 3 in
        let l1, l2 = List.nth (Table.coord_list table) 2 in
        let r1, r2 = List.nth (Table.coord_list table) 1 in
        set_coord_symbol !restaurant_layout.(b1) b2 "-";
        set_coord_symbol !restaurant_layout.(t1) t2 "-";
        set_coord_symbol !restaurant_layout.(r1) r2 "|";
        set_coord_symbol !restaurant_layout.(l1) l2 "|"
    | None -> raise TableNotFound

  (** The width of the restaurant. *)
  let width = ref 0

  (** The height of the restaurant. *)
  let height = ref 0

  (** Creates a restaurant based on the set number of tables [num_tables]. *)
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
      row.(0) <- ref "|";
      row.(1) <- ref " ";
      row.(2) <- ref " ";
      row.(3) <- ref " ";

      match get_table !current_table with
      | Some table ->
          let i = ref 4 in
          while !i < width - 8 do
            row.(!i) <- ref " ";
            row.(!i + 1) <- ref "-";
            row.(!i + 2) <- ref "-";
            Table.add_list table h (!i + 2);
            row.(!i + 3) <- ref "-";
            row.(!i + 4) <- ref " ";
            row.(!i + 5) <- ref " ";
            row.(!i + 6) <- ref " ";
            row.(!i + 7) <- ref " ";
            i := !i + 8;
            current_table := !current_table + 1
          done;
          row.(width - 1) <- ref "|";
          current_table := !current_table - num_tables
      | None -> failwith "Impossible"
    in

    (* creates the middle row of the table (the | | of each table) *)
    let table_middle_row row width h =
      (* sets the left border of the row *)
      row.(0) <- ref "|";
      row.(1) <- ref " ";
      (* manually does the | | thing *)
      match get_table !current_table with
      | Some table ->
          let i = ref 2 in
          while !i < width - 8 do
            row.(!i) <- ref " ";
            row.(!i + 1) <- ref " ";
            row.(!i + 2) <- ref "|";
            Table.add_list table h (!i + 2);
            row.(!i + 3) <- ref " ";
            row.(!i + 4) <- ref " ";
            row.(!i + 5) <- ref " ";
            row.(!i + 6) <- ref "|";
            Table.add_list table h (!i + 6);

            row.(!i + 7) <- ref " ";
            i := !i + 8;
            current_table := !current_table + 1
          done;
          (* sets the right border of the row *)
          row.(width - 3) <- ref " ";
          row.(width - 2) <- ref " ";
          row.(width - 1) <- ref "|";
          current_table := !current_table - num_tables
      | None -> failwith "Impossible"
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

  (** Prints the restaurant including the people that may or may not be seated
      in the restaurant. *)
  let display_filled_restaurant () =
    (* prints everything out *)
    for i = 0 to !height - 1 do
      for j = 0 to !width - 1 do
        print_string !(!restaurant_layout.(i).(j))
      done;
      print_string "\n"
    done;
    (* where print queue starts *)
    print_string "Current Queue: ";
    for _ = 0 to 5 + Random.int 10 do
      print_string "* "
    done;
    print_endline "\n"

  (** Prints out the table list such that "table #'s coordinates are (#,
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

  (** Modifies the corresponding rows of table_id and place 'x's around that
      table to represent the people in the party. [num_people] = the number of
      x's to place around the table [table_id] = the table to place the people
      at. *)
  let seat_party num_people table_id =
    match get_table table_id with
    | Some table ->
        if Table.isReady table then
          if num_people > Table.capacity table then raise SizeError
          else (
            Table.seat table num_people;
            change_seats_sym table_id num_people "*")
        else raise TableOccupied
    | None -> raise TableNotFound

  (** Modifies the corresponding rows of table_id and place '-'s or '|'s
      correspondinly around that table where people ('*') once were to represent
      the people in the party leaving and the table being cleared. [table_id] =
      the table to clear the people from *)
  let finish_eating table_id =
    match get_table table_id with
    | Some table ->
        Table.finish table;
        reset_table table_id
    | None -> failwith "Impossible"

  let get_table_list = !table_list
  let get_coord_value x y = !(!restaurant_layout.(x).(y))
end
