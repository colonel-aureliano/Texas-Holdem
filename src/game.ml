open Card
open Player

type game = {
  player_queue : player Queue.t;
  consecutive_calls : int;
  pot : int;
  current_deck : Card.t;
  cards_on_table : Card.t;
}

type command =
  | Call of int
  | Raise of int
  | Fold

let create_game players = raise (Failure "Unimplemented")

let player_move (cmd : command) (p : player) : player =
  match cmd with
  | Call x | Raise x -> deduct p x
  | _ -> raise IllegalMove

let execute_command (g : game) (cmd : command) =
  {
    g with
    player_queue =
      (let q = g.player_queue in
       match cmd with
       | Fold ->
           Queue.take q;
           q
       | _ ->
           Queue.add (Queue.take q |> player_move cmd) q;
           q);
    consecutive_calls =
      (match cmd with
      | Call _ -> g.consecutive_calls + 1
      | _ -> g.consecutive_calls);
  }

let drawing_card g = raise (Failure "Unimplemented")

(*BEGINNING OF HELPER FUNCTIONS FOR POT DISTRIBUTOR*)
let rec queue_to_list (q : player Queue.t) : player list =
  if Queue.is_empty q then [] else Queue.take q :: queue_to_list q

let reverse_args f x y = f y x

let rec list_to_queue (p : player list) : player Queue.t =
  let q = Queue.create () in
  List.iter (reverse_args Queue.add q) (List.rev p);
  q

let check_index_match
    (highest_index : int)
    (cur_index : int)
    (_ : player) : bool =
  highest_index = cur_index

let players_to_hands (p : player list) : Card.t list =
  List.map (fun x -> cards x) p

(*END OF HELPER FUNCTIONS FOR POT DISTRIBUTOR*)
let pot_distributer g =
  {
    g with
    player_queue =
      (let q = g.player_queue |> queue_to_list in
       List.filteri
         (check_index_match
            (q |> players_to_hands |> index_of_highest_hand))
         q
       |> list_to_queue);
  }

let poker_game g = raise (Failure "Unimplemented")

let betting_round (g : game) : game =
  if g.consecutive_calls = Queue.length g.player_queue then poker_game g
  else
    {
      (drawing_card g) with
      player_queue =
        (let q = g.player_queue in
         Queue.add (Queue.take q) q;
         q);
    }
