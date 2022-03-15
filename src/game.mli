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
  betting_round : int;
  game_over : bool;
}
(** The abstract type of values representing game. *)

type command =
  | Call
  | Raise of int
  | Fold

val create_game : player list -> int -> game
(** [create_game] initialize the game with player list passed in from
    the interface player, deal hands for each player, and deal the cards
    on the table. It also automatically move for small and big blind, so
    the next player to move is the one after big blind *)

val play_again : game -> game
(** [play again] reinitialize the game with the same players from the
    previous game but shift the small blind to the next person. Also
    deals the cards similar to create game *)

val get_curr_player : game -> player
(** [get_curr_player] returns the player who is making the decision of
    pass/raise/fold *)

val betting_round : game -> command -> game
(** [betting_round g] returns the game state after executing the
    player's next move*)
