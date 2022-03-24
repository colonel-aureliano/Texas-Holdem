open Player

type game = {
  active_players : player Queue.t;
  fold_collection : player list;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player;
  small_blind_amt : int;
  current_bet : int;
  current_raise : int;
  consecutive_calls : int;
  new_round : bool;
  game_over : bool;
  winners : player list;
  position : int;
}
(** The abstract type of values representing game. *)

exception RaiseFailure (* raise is less than previous raise. *)

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

val execute_command : game -> command -> game
(** [betting_round g] returns the game state after executing the
    player's next move*)

val get_curr_player : game -> player
(** [get_curr_player game] returns the player who is making the decision
    of pass/raise/fold *)

val get_winners : game -> player list
(** [get_winner game] returns the players who won. Precondition: game
    had ended *)

val get_winning_hand : game -> string
(** [get_winning_hand] returns the rank of winninng hand. Preconditino:
    game has ended *)

val get_all_players : game -> player list
(** [get_all_players game] returns all the players in the game*)