module WaitlistQueue : sig
  type t = int list

  val empty : 'a list
  val length : t -> int
  val enqueue : int -> t -> int list
  val dequeue : t -> int option
  val to_list : t -> int list
end
