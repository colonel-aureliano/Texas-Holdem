open Card
open Player

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
  game_over : bool;
}

type command =
  | Call
  | Raise of int
  | Fold

(* BEGINING OF HELPER FUNCTIONS *)

(** [mutable_push] is Queue.push but returns the Queue instead of a unit*)
let mutable_push x q =
  Queue.push x q;
  q

(** [mutable_pop] is Queue.pop but returns the Queue instead of a unit*)
let mutable_pop q =
  let myq = Queue.copy q in
  ignore (Queue.pop myq);
  myq

(** [reverse_arg_order] swaps the argument orders for a function with 2
    arguments*)
let reverse_arg_order f x y = f y x

(** [list_to_queue players] converts the list of players to a queue *)
let rec list_to_queue players queue =
  match players with
  | [] -> queue
  | h :: t ->
      list_to_queue t
        (Queue.add h queue;
         queue)

(** [queue_to_list players] converts the queue of players to a list *)
let queue_to_list q = List.rev (Queue.fold (fun x y -> y :: x) [] q)

(** [players_to_hands] returns a list containing the hands of each
    player in [p] *)
let players_to_hands (p : player list) : Card.t list =
  List.map (fun x -> cards x) p

(** [player_shift] pop the top element on the queue, deduct amount x
    from its wealth and push it to the back of the queue *)
let player_shift queue amt =
  mutable_push (deduct (Queue.pop queue) amt) queue

(** rearrange rotate the players in the queue until the first element is
    sb *)
let rec rearrange queue sb =
  if Player.name (Queue.peek queue) = Player.name sb then queue
  else rearrange (player_shift queue 0) sb

(** [card_to_players queue deck num_dealed] deal 2 cards randomly to
    players in the queue *)
let rec card_to_players queue deck num_dealed =
  if num_dealed = Queue.length queue then (queue, deck)
  else
    let player = Queue.take queue in
    let cards, new_deck = n_random_card deck 2 in
    let new_player = set_cards player cards in
    card_to_players
      (Queue.add new_player queue;
       queue)
      new_deck (num_dealed + 1)

(** [init_helper players_queue small_blind_amt] initialize game based on
    players_queue and small_blind_amt. Returns the game of players queue
    with big blind in the last place and each player with 2 cards and
    the table has 0 cards *)
let init_helper players_queue small_blind_amt =
  let players_with_card, curr_deck =
    card_to_players players_queue new_deck 0
  in
  let original_queue = Queue.copy players_with_card in
  let queue_sb = player_shift players_with_card small_blind_amt in
  let queue_bb = player_shift queue_sb (2 * small_blind_amt) in
  {
    players = original_queue;
    active_players = queue_bb;
    current_deck = curr_deck;
    cards_on_table = [];
    pot = 3 * small_blind_amt;
    small_blind = Queue.peek original_queue;
    small_blind_amt;
    current_bet = 2 * small_blind_amt;
    consecutive_calls = 0;
    game_over = false;
  }

(** [draw_card] draws num of cards from the current deck and places it
    on the table*)
let draw_card g num =
  let next_card, updated_deck = n_random_card g.current_deck num in
  {
    g with
    current_deck = updated_deck;
    cards_on_table = g.cards_on_table @ next_card;
  }

(** [execute_player_spending] returns the game state after the current
    player has spent amount x of his wealth*)
let execute_player_spending g x =
  { g with active_players = player_shift g.active_players x }
(* let execute_player_spending g x = { g with active_players =
   mutable_push (deduct (Queue.pop g.active_players) x)
   g.active_players; } *)

(** [update_player_status] update player's wealth and cards in the
    queue; will be used when player folds *)
let update_player_status queue player =
  let rotated_q = rearrange queue player in
  let curr_player = Queue.pop rotated_q in
  let updated_wealth_player = set_wealth curr_player (wealth player) in
  let updated_cards_player = remove_cards updated_wealth_player in
  mutable_push updated_cards_player queue

let rec update_active_players_status queue active_players =
  if Queue.length active_players = 0 then queue
  else
    let curr_player = Queue.pop active_players in
    let new_queue = update_player_status queue curr_player in
    update_active_players_status new_queue active_players

(* END OF HELPER FUNCTIONS *)

(** [create_game players small_blind_amt] initializes game based on
    players and small_blind_amt. The first player in the queue will
    automatically be the small_blind *)
let create_game players small_blind_amt =
  let queue = Queue.create () in
  let players_queue = list_to_queue players queue in
  init_helper players_queue small_blind_amt

(** [play_again game] restarts the game with same set of players but
    shifting the small_blind to the next person *)
let play_again game =
  let old_players_q = game.players in
  let updated_status_q =
    update_active_players_status old_players_q game.active_players
  in
  let new_players_q = player_shift updated_status_q 0 in
  init_helper new_players_q game.small_blind_amt

(** [get_curr_player game] returns the player who is making the decision
    of pass/raise/fold *)
let get_curr_player game = Queue.peek game.active_players

(** [get_winner game] returns the player who won. Precondition: game had
    ended *)
let get_winner game = Queue.peek game.players

(** [get_all_players game] returns all the players in the game*)
let get_all_players game = game.players |> queue_to_list

let table game = game.cards_on_table

let rec print_player_list_names = function
  | [] -> ()
  | h :: t ->
      print_string (Player.name h ^ " ");
      print_player_list_names t

let rec print_card_list = function
  | [] -> ()
  | h :: t ->
      print_string (Card.to_string h ^ " and ");
      print_card_list t

(** [winner_player_with_pot_added] returns the winning player with the
    pot added to his wealth*)
let winner_player_with_pot_added g =
  if Queue.length g.active_players = 1 then
    add (Queue.pop g.active_players) g.pot
  else
    let player_list = g.active_players |> queue_to_list in
    let highest_hand_index =
      player_list
      |> List.map (fun x -> Player.cards x @ g.cards_on_table)
      |> index_of_highest_hand
    in
    List.nth player_list highest_hand_index
    |> reverse_arg_order add g.pot

(** [pot distrubutor g] distributes the pot to the winning player in
    game g*)
let pot_distributer g =
  {
    g with
    game_over = true;
    players =
      (let winner = winner_player_with_pot_added g in
       (* print_string (Player.name winner); print_endline " determined
          by winner_player_with_pot_added for Debugging \ winner
          determination.\n"; *)
       let arranged_players = rearrange g.players winner in
       rearrange
         (mutable_push winner (mutable_pop arranged_players))
         winner);
  }

(** [update_fold_state g] returns the game state after shifting the
    small_blind by a player and removing the player who folded*)
let update_fold_state (g : game) : game =
  let curr_player = Queue.peek g.active_players in
  let new_active_players = mutable_pop (Queue.copy g.active_players) in
  let new_sb =
    if curr_player = g.small_blind then Queue.peek new_active_players
    else g.small_blind
  in
  print_endline "next player after fold is ";
  print_string (Player.name (Queue.peek new_active_players));
  { g with active_players = new_active_players; small_blind = new_sb }

(** [execute_command g] returns the game state after executing the
    player's next move*)
let execute_command (g : game) (cmd : command) : game =
  match cmd with
  | Call ->
      let cur_player = get_curr_player g in
      let x = g.current_bet - Player.amount_placed cur_player in
      if Player.wealth cur_player < x then raise InsufficientFund
      else
        let updated_g =
          { (execute_player_spending g x) with pot = g.pot + x }
        in
        if
          updated_g.consecutive_calls
          = Queue.length g.active_players - 1
        then
          if List.length g.cards_on_table = 5 then
            pot_distributer updated_g
            (* up to here doesn't break after fold *)
          else
            let num_card =
              if List.length g.cards_on_table = 0 then 3 else 1
            in
            let rearranged_p =
              rearrange updated_g.active_players
                (if
                 List.mem updated_g.small_blind
                   (updated_g.active_players |> queue_to_list)
                then updated_g.small_blind
                else Queue.peek updated_g.active_players)
            in
            draw_card
              {
                updated_g with
                active_players = rearranged_p;
                consecutive_calls = 0;
              }
              num_card
        else
          { updated_g with consecutive_calls = g.consecutive_calls + 1 }
  | Raise x ->
      let cur_player = get_curr_player g in
      let y = x + g.current_bet - amount_placed cur_player in
      {
        (execute_player_spending g y) with
        current_bet = g.current_bet + x;
        pot = g.pot + y;
        consecutive_calls = 1;
      }
  | Fold ->
      let updated_g = update_fold_state g in
      let active_player_count = Queue.length updated_g.active_players in
      let consecutive_call_count = updated_g.consecutive_calls in
      let num_cards_on_table = List.length g.cards_on_table in

      if
        active_player_count = 1
        || active_player_count = consecutive_call_count
           && num_cards_on_table = 5
      then pot_distributer updated_g
      else if active_player_count = consecutive_call_count then
        draw_card
          { updated_g with consecutive_calls = 0 }
          (if num_cards_on_table = 0 then 3 else 1)
      else updated_g
