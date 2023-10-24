module type Table = sig
  type t

  val make : int -> int -> t
  val state : string
  val isReady : bool
  val party_size : t -> int
  val capacity : t -> int
end
