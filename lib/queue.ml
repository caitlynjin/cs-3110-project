(** A model for waitlist queues. *)
module WaitlistQueue = struct
  type t = int list
  (** Type representing the list of party sizes in the queue. *)

  (** An empty waitlist queue. *)
  let empty = []

  (** Gets the length of the queue, which is the number of parties in the line. *)
  let length (q : t) : int = List.length q

  (** Adds the new party's party size to the queue. *)
  let enqueue (party : int) (q : t) = q @ [ party ]

  (** Removes the first party in the queue and returns a pair with the party's
      size as the first element and the resulting queue as the second element. *)
  let dequeue (q : t) : int * t =
    match q with
    | [] -> (-1, [])
    | h :: t -> (h, t)

  (** Converts the waitlist queue into a list. *)
  let to_list (q : t) : int list = q
end
