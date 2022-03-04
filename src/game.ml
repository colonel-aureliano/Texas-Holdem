open Card
open Player

exception MorePlayersNeeded

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

(* beginning of helper functions for create game *)
let list_to_queue players = 
  let order_after_blind = 
    match players with 
    | sm :: big :: [] -> players 
    | sm :: big :: tail ->  tail @ [sm] @ [big]
    | _ -> raise (MorePlayersNeeded) in 
  let rec helper order_after_blind queue = 
    match order_after_blind with
    | [] -> queue
    | h :: t -> helper t (Queue.add h queue; queue)
  in 
  let myqueue = Queue.create (); in 
  helper order_after_blind myqueue

let rec card_to_players players deck new_players = 
  match players with 
  | [] -> (new_players, deck)
  | h :: t ->   
      let cards, new_deck = n_random_card deck 3 in 
      let new_player = set_cards h cards in  
      card_to_players t new_deck (new_players @ [new_player])
(* end of helper functions for create game *)

let create_game (players : player list) = 
  let (new_players, new_deck) =  card_to_players players Card.new_deck [] in 
  let (table_card, final_deck) = n_random_card new_deck 3 in
  let curr_queue = list_to_queue new_players in 
  {
    player_queue = curr_queue;
    consecutive_calls = 0;
    pot = 3;
    current_deck = final_deck;
    cards_on_table = table_card;
  }


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
       |> List.map (reverse_args add g.pot)
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
