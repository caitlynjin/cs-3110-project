open Lwt.Infix

module Points = struct
  (* TODO: PASS IN NAME FROM MENUS.ML *)
  let name = "placeholder"

  (* point system to determine the performance of the user during the game *)
  type points = {
    mutable queue_performance : int;
    mutable meals : int;
    mutable profits : int;
  }

  let game_pts = { queue_performance = 0; meals = 0; profits = 0 }

  (* TODO: CALL THIS FUNCTION WHENEVER A PARTY IS SAT *)
  (* calculates points based on how queue is managed *)
  let parties_points (party_size : int) (wait_time : int) =
    (* points calculated from how long party had to wait in queue, weighted by
       party size *)
    let wait_points = ref 0 in
    if wait_time > 30 then wait_points := party_size * 5
    else if wait_time > 10 then wait_points := party_size * 10
    else wait_points := party_size * 15;

    (* points calculated from party size *)
    (* let party_points = party_size * 10 in *)
    (* print_endline ("wait points: " ^ string_of_int !wait_points);
    print_endline ("queueperform: " ^ string_of_int game_pts.queue_performance); *)
    (* update points *)
    game_pts.queue_performance <- game_pts.queue_performance + !wait_points

  (* TODO: CALL THIS FUNCTION WHENEVER A PARTY IS DONE AT THEIR TABLE *)
  (* calculates points based on party experience at table*)
  let meals_points (party_size : int) (food_time : int) (num_dishes : int)
      (duration_at_table : int) =
    (* points calculated for how fast food are delivered to table (shorter is
       better) *)
    let food_points = ref 0 in
    if food_time > 20 then food_points := 5 * num_dishes
    else if food_time > 10 then food_points := 12 * num_dishes
    else food_points := 20 * num_dishes;

    (* points calculated for how long party stayed at table (longer is
       better) *)
    let duration = ref 0 in
    if duration_at_table > 30 then duration := 15 * party_size
    else if duration_at_table > 20 then duration := 10 * party_size
    else duration := 5 * party_size;

    (* update points *)
    game_pts.meals <- (game_pts.meals + !food_points + !duration)

  (* TODO: CALL THIS FUNCTION WHENEVER A PARTY IS DONE AT THEIR TABLE *)
  let profits_points (total : float) =
    (* points calculated from total profits *)
    let profit_points = total *. 25. in

    (* update points *)
    game_pts.profits <- game_pts.profits + int_of_float profit_points

  (* returns total points accumulated from managing the queue *)
  let get_queue () = game_pts.queue_performance

  (* returns total points accumulated from managing each table *)
  let get_meals () = game_pts.meals

  (* returns total points accumulated from profits *)
  let get_profits () = game_pts.profits

  (* returns total points accumulated *)
  let get_points () =
    game_pts.queue_performance + game_pts.meals + game_pts.profits

  let show_points () =
    Lwt_io.printl
      ("You have " ^ string_of_int (get_points ()) ^ " points so far!")

  (* to display at end of game- wrap up with final points *)
  let print_points () =
    Lwt_io.printl "Let's see how u did!" >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    (let queue_pts = string_of_int (get_queue ()) in
     Lwt_io.printl
       ("From managing the queue, you earned " ^ queue_pts ^ " points! \n"))
    >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    (let meals_pts = string_of_int (get_meals ()) in
     Lwt_io.printl
       ("From managing all the tables, you earned " ^ meals_pts ^ " points! \n"))
    >>= fun () ->
    Lwt_unix.sleep 1. >>= fun () ->
    (let money_pts = string_of_int (get_profits ()) in
     Lwt_io.printl
       ("From all the orders you took, you earned " ^ money_pts ^ " points! \n"))
    >>= fun () ->
    Lwt_unix.sleep 2. >>= fun () ->
    let congrat_msg =
      "Congratulations! Based on your point score, you have successfully run \
       Dish Dash Dilemma's restaurant, " ^ name
      ^ "! Now you (mostly) have what it takes to run your very own \
         restaurant!! \n"
    in
    Lwt_io.printl congrat_msg >>= fun () -> Lwt_unix.sleep 2.
end
