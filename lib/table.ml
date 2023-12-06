module Table = struct
  type state =
    | Occupied
    | Ready
    | Dirty

  type t = {
    mutable current_state : state;
    capacity : int;
    mutable party_size : int;
  }

  let make c = { current_state = Ready; capacity = c; party_size = 0 }
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
end
