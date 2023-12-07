module type Table = sig
  type t

  val make : int -> int -> t
  val make_with_coord : int -> int -> (int * int) list -> t
  val state : t -> string
  val isReady : bool
  val party_size : t -> int
  val capacity : t -> int
  val coord_list : t -> (int * int) list
  val add_list : t -> int -> int -> unit
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

  let make _ c =
    { current_state = Ready; capacity = c; party_size = 0; coord_list = ref [] }

  let make_with_coord p c lst =
    {
      current_state = Ready;
      party_size = p;
      capacity = c;
      coord_list = ref lst;
    }

  let state table = table.current_state
  let clean table = table.current_state <- Ready

  let seat table p =
    table.current_state <- Occupied;
    table.party_size <- p

  let finish table =
    table.current_state <- Dirty;
    table.party_size <- 0

  let party_size table = table.party_size
  let capacity table = table.capacity
  let coord_list table = !(table.coord_list)
  let add_list table x y = table.coord_list := (x, y) :: !(table.coord_list)
end

module ReadyTable : Table = struct
  type t = {
    state : string;
    capacity : int;
    coord_list : (int * int) list ref;
  }

  let make _ c = { state = "Ready"; capacity = c; coord_list = ref [] }

  let make_with_coord _ c lst =
    { state = "Ready"; capacity = c; coord_list = ref lst }

  (* let set_list table lst = table.coord_list = lst *)
  let state table = table.state
  let isReady = true
  let coord_list table = !(table.coord_list)
  let party_size _ = 0
  let capacity table = table.capacity
  let add_list table x y = table.coord_list := (x, y) :: !(table.coord_list)
end

module OccupiedTable : Table = struct
  type t = {
    state : string;
    party_size : int;
    capacity : int;
    coord_list : (int * int) list ref;
  }

  let make p c =
    { state = "Occupied"; party_size = p; capacity = c; coord_list = ref [] }

  let make_with_coord p c lst =
    { state = "Occupied"; party_size = p; capacity = c; coord_list = ref lst }

  (* let set_list table lst = table.coord_list = lst *)
  let state table = table.state
  let isReady = false
  let party_size table = table.party_size
  let capacity table = table.capacity
  let coord_list table = !(table.coord_list)
  let add_list table x y = table.coord_list := (x, y) :: !(table.coord_list)
end

module DirtyTable : Table = struct
  type t = {
    state : string;
    party_size : int;
    capacity : int;
    coord_list : (int * int) list ref;
  }

  let make p c =
    { state = "Dirty"; party_size = p; capacity = c; coord_list = ref [] }

  let make_with_coord p c lst =
    { state = "Occupied"; party_size = p; capacity = c; coord_list = ref lst }

  (* let set_list table lst = table.coord_list = lst *)
  let state table = table.state
  let isReady = true
  let party_size table = table.party_size
  let capacity table = table.capacity
  let coord_list table = !(table.coord_list)
  let add_list table x y = table.coord_list := (x, y) :: !(table.coord_list)
end
