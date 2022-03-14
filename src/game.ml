open Card
open Player

exception MorePlayersNeeded
exception RepeatedName of string


type game = {
  players : player Queue.t;
  active_players : player Queue.t;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player; 
  small_blind_amt : int; 
  current_bet : int; 
  consecutive_calls : int;
}

type command =
  | Call of int
  | Raise of int
  | Fold

(* BEGINING OF HELPER FUNCTIONS FOR CREATE GAME *)

(** rearrange rotate the player queue until dealer is the last element*)
let rec rearrange (queue : player Queue.t) (sb : player) =
  if Queue.peek queue = sb then queue
  else
    rearrange
      (Queue.add (Queue.take queue) queue;
       queue)
      sb

(** [list_to_queue players] converts the list of players to a queue *)
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

(** [shift_for_blind queue] pop the top element on the queue and push it
    to the back of it *)
let shift_for_blind queue =
  Queue.add (Queue.take queue) queue;
  queue

(** [card_to_players queue deck num_dealed] deal 3 cards randomly to players
    in the queue *)  
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

(** [dup_name player_names] check if there exits duplicated names 
    in player_names. Return false if no duplicate exits and raise
    ReapeadtedName error if found duplicates *)
let rec dup_name player_names = 
  match player_names with
  | [] -> false
  | hd :: tl -> 
    if List.exists ((=) hd) tl then raise (RepeatedName hd)
    else dup_name tl;;

(** [init_helper players_queue small_blind_amt] initialize game 
    based on players_queue and small_blind_amt. Returns the game
    of players queue with big blind in the last place and each
    player with 3 cards and the table has 3 cards *)
let init_helper players_queue small_blind_amt =  
  let players_with_card, curr_deck = card_to_players players_queue new_deck 3 in  
  let table_card, final_deck = n_random_card curr_deck 3 in 
  let sb_shift_players = shift_for_blind players_with_card in 
  let bb_shift_players = shift_for_blind sb_shift_players in 
  {
    players = players_queue;
    active_players = bb_shift_players;
    current_deck = final_deck;
    cards_on_table = table_card;
    pot = 3 * small_blind_amt;
    small_blind = Queue.peek players_queue; 
    small_blind_amt = small_blind_amt; 
    current_bet = 2 * small_blind_amt; 
    consecutive_calls = 0;
  }

(** [create_game players small_blind_amt] initializes game based 
    on players and small_blind_amt. The first player in the queue 
    will automatically be the small_blind *)   
let create_game players small_blind_amt = 
  let player_names = List.map (fun x -> name x) players in 
  let _ = dup_name player_names in 
  let players_queue = list_to_queue players in init_helper players_queue small_blind_amt

(** [play_again game] restarts the game with same set of players but
    shifting the small_blind to the next person *)
let play_again game = 
  let old_player_queue = game.players in 
  let new_player_queue = shift_for_blind old_player_queue in 
  let small_blind_amt = game.small_blind_amt in 
  init_helper new_player_queue small_blind_amt

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

