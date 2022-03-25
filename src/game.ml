open Card
open Player

type game = {
  active_players : player list;
  fold_collection : player list;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player;
  small_blind_amt : int;
  current_bet : int; (* highest bet on table *)
  minimum_raise : int; (* raise of current betting round *)
  consecutive_calls : int;
  new_round : bool; (* true when new cards are dealt, altered by main *)
  game_over : bool;
  winners : player list; (* empty until game has ended *)
  position : int; (* position of small blind in the current game *)
}

type command =
  | Call
  | Raise of int
  | Fold

(* BEGINING OF HELPER FUNCTIONS *)

(** [push x q] is [q; x]*)
let push x q = q @ [ x ]

(** [pop q] is q with first element removed. Raises: Failure if q is
    empty *)
let pop = function
  | h :: t -> t
  | _ -> failwith "Empty"

(** [peek q] is the first element of q. Raises: Failure if q is empty *)
let peek ls = List.hd ls

(** [players_to_hands] returns a list containing the hands of each
    player in [p] *)
let players_to_hands (p : player list) : Card.t list =
  List.map (fun x -> cards x) p

(** [player_shift] pop the top element of the queue, deduct amount x
    from its wealth and push it to the back of the queue. Raises:
    InsufficientFund. *)
let player_shift queue amt =
  let p = peek queue in
  push (deduct p amt) queue |> pop

(** rearrange rotate the players in the queue until the first element is
    has the name of sb *)
let rec rearrange queue sb =
  if name (peek queue) = name sb then queue
  else rearrange (player_shift queue 0) sb

(** [rearrangepos queue pos] rotate the players in the queue until the
    first element is has the position pos. Precondition: such player
    exists. *)
let rec rearrangepos queue pos =
  if position (peek queue) = pos then queue
  else rearrangepos (player_shift queue 0) pos

(** [card_to_players queue deck num_dealed] deal 2 cards randomly to
    players in the queue *)
let rec card_to_players queue deck num_dealed =
  if num_dealed = List.length queue then (queue, deck)
  else
    let player = peek queue in
    let queue = pop queue in
    let cards, new_deck = n_random_card deck 2 in
    let new_player = set_cards player cards in
    card_to_players (push new_player queue) new_deck (num_dealed + 1)

(** [init_helper players_queue small_blind_amt first_player_pos]
    initialize game. Small blind is the player at the first_player_pos,
    and is also placed to the top of the queue. Returns the game of
    players queue with big blind in the last place and each player with
    2 cards and the table has 0 cards *)
let init_helper players_queue small_blind_amt first_player_pos =
  let players_with_card, curr_deck =
    card_to_players players_queue new_deck 0
  in
  let original_queue =
    rearrangepos players_with_card first_player_pos
  in
  let sb = peek original_queue in
  let queue_sb = player_shift original_queue small_blind_amt in
  let queue_bb = player_shift queue_sb (2 * small_blind_amt) in
  {
    active_players = queue_bb;
    current_deck = curr_deck;
    cards_on_table = [];
    pot = 3 * small_blind_amt;
    small_blind = sb;
    small_blind_amt;
    current_bet = 2 * small_blind_amt;
    minimum_raise = 2 * small_blind_amt;
    consecutive_calls = 0;
    new_round = false;
    game_over = false;
    fold_collection = [];
    position = first_player_pos;
    winners = [];
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
    player has spent amount x of his wealth. Then the current player is
    moved to the back.*)
let execute_player_spending g x =
  { g with active_players = player_shift g.active_players x }

(* END OF HELPER FUNCTIONS *)

(** [create_game players small_blind_amt] initializes game based on
    players and small_blind_amt. The first player in the queue will
    automatically be the small_blind *)
let create_game players small_blind_amt =
  init_helper players small_blind_amt 1

(** [play_again game] restarts the game with same set of players but
    shifting the small_blind to the next person *)
let play_again game =
  let players_queue =
    List.(
      game.active_players @ game.fold_collection
      |> map reset_player
      |> List.sort (fun x y -> position x - position y))
  in
  let pos =
    let n = (game.position + 1) mod List.length players_queue in
    if n = 0 then List.length players_queue else n
  in
  init_helper players_queue game.small_blind_amt pos

let reshuffling_period game = game

(** [get_curr_player game] returns the player who is making the decision
    of pass/raise/fold *)
let get_curr_player game = peek game.active_players

(** [get_winner game] returns the player who won. Precondition: game had
    ended *)
let get_winners game = game.winners

let ranks =
  [
    "High Card";
    "One Pair";
    "Two Pairs";
    "Three of a Kind";
    "Straight";
    "Flush";
    "Full House";
    "Four of a Kind";
    "Striaght Flush";
    "Royal Flush";
  ]

(** [get_winning_hand] returns the rank of winninng hand. Preconditino:
    game has ended *)
let get_winning_hand game =
  (game.winners |> List.hd |> cards) @ game.cards_on_table
  |> rank_of_hand |> List.nth ranks

(** [get_all_players game] returns all the players in the game. Sorted
    by position. *)
let get_all_players game =
  game.active_players @ game.fold_collection
  |> List.sort (fun x y -> position x - position y)

let table game = game.cards_on_table

(** [winner_player_with_pot_added] is the winning players with the pot
    added to their wealths *)
let winners_with_pot_added g : player list =
  if List.length g.active_players = 1 then
    [ add (peek g.active_players) g.pot ]
  else
    let player_list = g.active_players in
    let highest_hand_indeces =
      try
        [
          player_list
          |> List.map (fun x -> Player.cards x @ g.cards_on_table)
          |> index_of_highest_hand;
        ]
      with Tie ls -> ls
    in
    let n = List.length highest_hand_indeces in
    List.map
      (fun x -> add (List.nth player_list x) (g.pot / n))
      highest_hand_indeces

(** [pot distrubutor g] distributes the pot to the winning player in
    game g. Puts winners in game.winners and update active players. *)
let pot_distributer g =
  let g =
    { g with game_over = true; winners = winners_with_pot_added g }
  in
  let names = List.map (fun x -> name x) g.winners in
  {
    g with
    active_players =
      (g.active_players
      |> List.filter (fun x -> List.mem (name x) names |> not))
      @ g.winners;
  }

(** [new_betting_round] updates the game state to enter the next betting
    round. Precondition: the current betting round has ended. *)
let new_betting_round (g : game) : game =
  if List.length g.cards_on_table = 5 then pot_distributer g
  else
    let num_card = if List.length g.cards_on_table = 0 then 3 else 1 in
    let rearranged_p = rearrange g.active_players g.small_blind in
    draw_card
      {
        g with
        active_players = rearranged_p;
        consecutive_calls = 0;
        minimum_raise = 0;
        new_round = true;
      }
      num_card

exception RaiseFailure

(** [execute_command g] returns the game state after executing the
    player's next move*)
let execute_command (g : game) (cmd : command) : game * int =
  match cmd with
  | Call ->
      let cur_player = get_curr_player g in
      let x = g.current_bet - Player.amount_placed cur_player in
      let updated_g =
        {
          (execute_player_spending g x) with
          pot = g.pot + x;
          consecutive_calls = g.consecutive_calls + 1;
        }
      in
      if updated_g.consecutive_calls = List.length g.active_players then
        (new_betting_round updated_g, x)
      else (updated_g, x)
  | Raise x ->
      if x < g.minimum_raise then raise RaiseFailure
      else
        let cur_player = get_curr_player g in
        let y = x + g.current_bet - amount_placed cur_player in
        ( {
            (execute_player_spending g y) with
            current_bet = g.current_bet + x;
            minimum_raise = x;
            pot = g.pot + y;
            consecutive_calls = 1;
          },
          y )
  | Fold ->
      let folder = get_curr_player g in
      let updated_g =
        {
          g with
          fold_collection = peek g.active_players :: g.fold_collection;
          active_players = pop g.active_players;
        }
      in
      if List.length updated_g.active_players = 1 then
        (pot_distributer updated_g, 0)
      else if
        updated_g.consecutive_calls
        = List.length updated_g.active_players
      then (new_betting_round updated_g, 0)
      else
        ( {
            updated_g with
            small_blind =
              (if name folder = name g.small_blind then
               peek updated_g.active_players
              else g.small_blind);
          },
          0 )

let get_legal_moves (g : game) : string list =
  let cur_player = peek g.active_players in
  let amount_left_after_call =
    wealth cur_player - g.current_bet + amount_placed cur_player
  in
  let moves = [ "Fold" ] in
  if amount_left_after_call >= 0 then
    let moves = moves @ [ "Call" ] in
    if g.minimum_raise <= amount_left_after_call then
      moves
      @ [
          "Raise between $"
          ^ string_of_int g.minimum_raise
          ^ " and $"
          ^ string_of_int amount_left_after_call;
        ]
    else moves
  else moves

let player_to_string (p : player) : string =
  "      \"name\": " ^ "\"" ^ Player.name p ^ "\"" ^ ",\n"
  ^ "      \"wealth\": "
  ^ string_of_int (Player.wealth p)
  ^ ",\n" ^ "      \"amount_placed\": "
  ^ string_of_int (Player.amount_placed p)
  ^ ",\n" ^ "      \"cards\": " ^ "\""
  ^ Card.to_string (Player.cards p)
  ^ "\"" ^ ",\n" ^ "      \"position\": "
  ^ string_of_int (Player.position p)

let save_game (g : game) (name : string) : bool =
  let file = "game_files/" ^ name ^ ".json" in
  try
    let _ =
      let oc = open_out file in
      Printf.fprintf oc "%s\n" "{";
      Printf.fprintf oc "%s\n" "  \"active_players\": [";
      Printf.fprintf oc "%s\n" "    {";
      let active_players =
        String.concat "\n    },\n    {\n"
          (List.map player_to_string g.active_players)
      in
      Printf.fprintf oc "%s\n" active_players;
      Printf.fprintf oc "%s\n" "    }";
      Printf.fprintf oc "%s\n" "  ],";
      Printf.fprintf oc "%s\n" "  \"fold_collection\": [";
      Printf.fprintf oc "%s\n" "    {";
      let fold_collection =
        String.concat "\n    },\n    {\n"
          (List.map player_to_string g.fold_collection)
      in
      Printf.fprintf oc "%s\n" fold_collection;
      Printf.fprintf oc "%s\n" "    }";
      Printf.fprintf oc "%s\n" "  ],";
      let current_deck = Card.to_string g.current_deck in
      Printf.fprintf oc "  %s,\n"
        ("\"current_deck\": " ^ "\"" ^ current_deck ^ "\"");
      let cards_on_table = Card.to_string g.cards_on_table in
      Printf.fprintf oc "  %s,\n"
        ("\"cards_on_table\": " ^ "\"" ^ cards_on_table ^ "\"");
      let pot = string_of_int g.pot in
      Printf.fprintf oc "  %s,\n" ("\"pot\": " ^ pot);
      Printf.fprintf oc "%s\n" "  \"small_blind\": ";
      Printf.fprintf oc "%s\n" "    {";
      let small_blind = player_to_string g.small_blind in
      Printf.fprintf oc "%s\n" small_blind;
      Printf.fprintf oc "%s\n" "    },";
      let small_blind_amt = string_of_int g.small_blind_amt in
      Printf.fprintf oc "  %s,\n"
        ("\"small_blind_amt\": " ^ small_blind_amt);
      let current_bet = string_of_int g.current_bet in
      Printf.fprintf oc "  %s,\n" ("\"current_bet\": " ^ current_bet);
      let minimum_raise = string_of_int g.minimum_raise in
      Printf.fprintf oc "  %s,\n" ("\"minimum_raise\": " ^ minimum_raise);
      let consecutive_calls = string_of_int g.consecutive_calls in
      Printf.fprintf oc "  %s,\n"
        ("\"consecutive_calls\": " ^ consecutive_calls);
      let new_round = string_of_bool g.new_round in
      Printf.fprintf oc "  %s,\n" ("\"new_round\": " ^ new_round);
      let game_over = string_of_bool g.game_over in
      Printf.fprintf oc "  %s,\n" ("\"game_over\": " ^ game_over);
      Printf.fprintf oc "%s\n" "  \"winners\": [";
      Printf.fprintf oc "%s\n" "    {";
      let winners =
        String.concat "\n    },\n    {\n"
          (List.map player_to_string g.winners)
      in
      Printf.fprintf oc "%s\n" winners;
      Printf.fprintf oc "%s\n" "    }";
      Printf.fprintf oc "%s\n" "  ],";
      let position = string_of_int g.position in
      Printf.fprintf oc "  %s\n" ("\"position\": " ^ position);
      Printf.fprintf oc "%s\n" "}";
      close_out oc
    in
    true
  with _ -> false

let read_game (j : Yojson.Basic.t) : game = failwith ""