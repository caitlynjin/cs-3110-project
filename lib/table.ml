(** A module type that abstracts properties for all types of tables in the
    restaurant. *)
module type Table = sig
  type t

  val make : int -> int -> t
  (** Makes a new table with the corresponding coordinates. *)

  val state : t -> string
  (** Returns the current state of the table. *)

  val isReady : t -> bool
  (** Returns whether the table is in the ready state. *)

  val party_size : t -> int
  (** Returns the size of the party. *)

  val capacity : t -> int
  (** Returns the person capacity of the table. *)

  val coord_list : t -> (int * int) list
  (** Returns the coordinate list of people seated at the table. *)
end

module Table = struct
  type state =
    | Occupied
    | Ready
    | Dirty

  type t = {
    mutable current_state : state;
    capacity : int;
    coord_list : (int * int) list ref;
    mutable party_size : int;
  }

  (** Makes a table with the default Ready state, a person capacity [c], a party
      size of zero, and an empty coordinate list for the seating of people. *)
  let make _ c =
    { current_state = Ready; capacity = c; party_size = 0; coord_list = ref [] }

  (** Makes a table with people seated. The table has a default state of
      Occupied, a party size of [p], a person capacity of [c], and a coordinate
      list for the seating of people [lst]. *)
  let make_with_coord p c lst =
    {
      current_state = Occupied;
      party_size = p;
      capacity = c;
      coord_list = ref lst;
    }

  (** The current state of the table [table]. *)
  let state table = table.current_state

  (** Cleans the table, converting the current state of the table to Ready. *)
  let clean table = table.current_state <- Ready

  (** Returns whether the table is in the Ready state. *)
  let isReady table = table.current_state = Ready

  (** Seats the people at the table [table], making the current state of the
      table Occupied and changing the party size to the corresponding size [p]. *)
  let seat table p =
    table.current_state <- Occupied;
    table.party_size <- p

  (** Converts the table [table] to the Dirty state, and removes the people at
      the table, which changes the party size to zero. *)
  let finish table =
    table.current_state <- Dirty;
    table.party_size <- 0

  (** Returns the party size of the table [table]. *)
  let party_size table = table.party_size

  (** Returns the person capacity of the table [table]. *)
  let capacity table = table.capacity

  (** Returns the coordinate list of the table [table], which is a list of
      integer pairs that represent the positions around the table. *)
  let coord_list table = !(table.coord_list)

  (** Adds a person to the coordinate ([x], [y]) at the table [table]. *)
  let add_list table x y = table.coord_list := (x, y) :: !(table.coord_list)
end

(* module ReadyTable : Table = struct type t = { state : string; capacity : int;
   coord_list : (int * int) list ref; }

   (** Makes a table with the Ready state, a person capacity [c], and an empty
   coordinate list for the seating of people. *) let make _ c = { state =
   "Ready"; capacity = c; coord_list = ref [] }

   (** The state of the table [table]. *) let state table = table.state

   (** Returns whether the table is ready. *) let isReady _ = true

   (** Returns the coordinate list of the people seated at the table, which is
   empty. *) let coord_list _ = []

   (** Returns the party size of the table, which is zero. *) let party_size _ =
   0

   (** Returns the person capacity of the table [table]. *) let capacity table =
   table.capacity end

   module OccupiedTable : Table = struct type t = { state : string; party_size :
   int; capacity : int; coord_list : (int * int) list ref; }

   (** Makes a table with the default Ready state, a person capacity [c], a
   party size of [p], and an empty coordinate list for the seating of people. *)
   let make p c = { state = "Occupied"; party_size = p; capacity = c; coord_list
   = ref [] }

   (** Makes a table with people seated. The table has a default state of
   Occupied, a party size of [p], a person capacity of [c], and a coordinate
   list for the seating of people [lst]. *) let make_with_coord p c lst = {
   state = "Occupied"; party_size = p; capacity = c; coord_list = ref lst }

   (** Returns the current state of the table [table]. *) let state table =
   table.state

   (** Returns whether the table is in the Ready state, which it is not. *) let
   isReady _ = false

   (** Returns the party size of the table [table]. *) let party_size table =
   table.party_size

   (** Returns the person capacity of the table [table]. *) let capacity table =
   table.capacity

   (** The coordinate list of the table [table], which is a list of integer
   pairs that represent the positions around the table. *) let coord_list table
   = !(table.coord_list)

   (** Adds a person to the coordinate ([x], [y]) at the table [table]. *) let
   add_list table x y = table.coord_list := (x, y) :: !(table.coord_list) end

   module DirtyTable : Table = struct type t = { state : string; party_size :
   int; capacity : int; coord_list : (int * int) list ref; }

   (** Makes a table with the default Ready state, a person capacity [c], a
   party size of [p], and an empty coordinate list for the seating of people. *)
   let make p c = { state = "Dirty"; party_size = p; capacity = c; coord_list =
   ref [] }

   (** Makes a table with people seated. The table has a default state of
   Occupied, a party size of [p], a person capacity of [c], and a coordinate
   list for the seating of people [lst]. *) let make_with_coord p c lst = {
   state = "Occupied"; party_size = p; capacity = c; coord_list = ref lst }

   (** Returns the current state of the table [table]. *) let state table =
   table.state

   (** Returns whether the table is in the Ready state, which it is not. *) let
   isReady _ = false

   (** Returns the size of the party at the table [table]. *) let party_size
   table = table.party_size

   (** Returns the person capacity of the table [table]. *) let capacity table =
   table.capacity

   (** Returns the coordinate list of the table [table], which is a list of
   integer pairs that represent the positions around the table. *) let
   coord_list table = !(table.coord_list)

   (** Adds a person to the coordinate ([x], [y]) at the table [table]. *) let
   add_list table x y = table.coord_list := (x, y) :: !(table.coord_list) end *)
