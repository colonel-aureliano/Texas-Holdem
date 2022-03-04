open Card
open Player

exception MorePlayersNeeded
exception NotImplemented

type game = {
  player_queue : player Queue.t;
  small_blind : player;
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

(** rearrange rotate the player queue until dealer is the last element*)
let rec rearrange (queue : player Queue.t) (sb : player) =
  if Queue.peek queue = sb then queue
  else
    rearrange
      (Queue.add (Queue.take queue) queue;
       queue)
      sb
(* let rec rearrange (queue : player Queue.t) (sb : player) = if
   Queue.peek_opt queue = sb then queue else rearrange (Queue.add
   (Queue.take queue) queue;) *)

(** convert player list to player queue*)
let list_to_queue players =
  let rec helper players queue =
    match players with
    | [] -> queue
    | h :: t ->
        helper t
          (Queue.add h queue;
           queue)
  in
  let myqueue = Queue.create () in
  helper players myqueue
(* let list_to_queue players = let rec helper players queue = match
   players with | [] -> queue | h :: t -> helper t (Queue.add h queue;)
   in let myqueue = Queue.create (); in helper players myqueue *)

(** shift after small or big blind*)
let shift_for_blind queue =
  Queue.add (Queue.take queue) queue;
  queue
(* let shift_for_blind queue = Queue.add (Queue.take queue) queue *)

let rec card_to_players queue deck num_dealed =
  if num_dealed = Queue.length queue then (queue, deck)
  else
    let player = Queue.take queue in
    let cards, new_deck = n_random_card deck 3 in
    let new_player = set_cards player cards in
    card_to_players
      (Queue.add new_player queue;
       queue)
      new_deck (num_dealed + 1)
(* let rec card_to_players queue deck num_dealed = if num_dealed =
   Queue.length queue then (queue, deck) else let player = Queue.take
   queue in let cards, new_deck = n_random_card deck 3 in let new_player
   = set_cards player cards in card_to_players (Queue.add new_player
   queue;) new_deck num_dealed+1 *)
(* end of helper functions for create game *)

let create_game players small_blind_amt = 
  let init_players = list_to_queue players in  
  let curr_small_blind = Queue.peek init_players in 
  let players_with_card, curr_deck = card_to_players init_players new_deck 3 in 
  let table_card, final_deck = n_random_card curr_deck 3 in 
  let sb_shift_players = shift_for_blind players_with_card in 
  let bb_shift_players = shift_for_blind sb_shift_players in 
  { 
    player_queue = bb_shift_players;
    small_blind = curr_small_blind;
    consecutive_calls  = 0;
    pot = 3 * small_blind_amt;
    current_deck = final_deck;
    cards_on_table = final_deck;
  }
(* let create_game (players : player list) = let init_queue =
   list_to_queue players in let ordered_queue = rearrange init_queue in
   let dealed_queue, curr_deck = card_to_players ordered_queue
   Card.new_deck [] in let table_card, final_deck = n_random_card
   curr_deck 3 in let sb_shift_queue = shift_for_blind dealed_queue in
   let bb_shift_queue = shift_for_blind sb_shift_queue in { player_queue
   = bb_shift_queue; consecutive_calls = 0; pot = 3; current_deck =
   final_deck; cards_on_table = table_card; } *)

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
           ignore (Queue.take q);
           q
       | _ ->
           Queue.add (Queue.take q |> player_move cmd) q;
           q);
    consecutive_calls =
      (match cmd with
      | Call _ -> g.consecutive_calls + 1
      | _ -> g.consecutive_calls);
  }

let drawing_card g =
  let new_card, new_set = n_random_card g.current_deck 1 in
  {
    player_queue = g.player_queue;
    consecutive_calls = g.consecutive_calls;
    pot = g.pot;
    current_deck = new_set;
    cards_on_table = new_card @ g.cards_on_table;
    small_blind = raise NotImplemented (*TODO: IMPLEMENT*);
  }

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

let get_command : command = raise (Failure "Unimplemented")

let betting_round (g : game) : game =
  if g.consecutive_calls = Queue.length g.player_queue then
    drawing_card { g with consecutive_calls = 0 }
  else execute_command g get_command

let rec poker_helper curr_round max_round game =
  if curr_round >= max_round then game
  else
    let game_after_bet = betting_round game in
    let ordered_game =
      {
        game_after_bet with
        player_queue =
          rearrange game_after_bet.player_queue
            game_after_bet.small_blind;
      }
    in
    poker_helper (curr_round + 1) max_round ordered_game

let poker_game game =
  let curr_round = 0 in
  let max_round = 2 in
  let new_game = poker_helper curr_round max_round game in 
  let game_after_last_bet = betting_round new_game in 
  pot_distributer game_after_last_bet

