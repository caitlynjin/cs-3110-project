module type Table = sig
  type t

  val make : int -> int -> t
  val state : string
  val isReady : bool
  val party_size : t -> int
  val capacity : t -> int
end

module ReadyTable : Table = struct
  type t = {
    state : string;
    capacity : int;
  }

  let make p c = { state = "Ready"; capacity = c }
  let state = "Ready"
  let isReady = true
  let party_size table = 0
  let capacity table = table.capacity
end

module OccupiedTable : Table = struct
  type t = {
    state : string;
    party_size : int;
    capacity : int;
  }

  let make p c = { state = "Occupied"; party_size = p; capacity = c }
  let state = "Occupied"
  let isReady = false
  let party_size table = table.party_size
  let capacity table = table.capacity
end

module DirtyTable : Table = struct
  type t = {
    state : string;
    party_size : int;
    capacity : int;
  }

  let make p c = { state = "Dirty"; party_size = p; capacity = c }
  let state = "Dirty"
  let isReady = true
  let party_size table = table.party_size
  let capacity table = table.capacity
end
