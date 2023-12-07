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

module ReadyTable : Table
module OccupiedTable : Table
module DirtyTable : Table
