(** A model for waitlist queues. *)
module WaitlistQueue = struct
  type t = int list

  (** An empty waitlist queue. *)
  let empty = []

  (** Gets the length of the queue, which is the number of parties in the line. *)
  let length (q : t) : int = List.length q

  (** Adds the new party's party size to the queue. *)
  let enqueue (party : int) (q : t) = q @ [ party ]

  (** Removes the first party in the queue and returns the party's size. *)
  let dequeue (q : t) : int option =
    match q with
    | [] -> None
    | h :: _ -> Some h

  (** Converts the waitlist queue into a list. *)
  let to_list (q : t) : int list = q
end
