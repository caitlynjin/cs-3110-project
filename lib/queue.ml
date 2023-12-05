module WaitlistQueue = struct
  type t = int list

  let empty = []
  let length (q : t) : int = List.length q
  let enqueue (party : int) (q : t) = q @ [ party ]

  let dequeue (q : t) : int option =
    match q with
    | [] -> None
    | h :: _ -> Some h

  let to_list (q : t) : int list = q
end
