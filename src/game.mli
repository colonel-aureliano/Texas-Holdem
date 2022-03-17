open Player

type game = {
  active_players : player Queue.t;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player;
  small_blind_amt : int;
  current_bet : int;
  consecutive_calls : int;
  game_over : bool;
  fold_collection : player Queue.t;
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

val execute_command : game -> command -> game
(** [betting_round g] returns the game state after executing the
    player's next move*)

val get_curr_player : game -> player
(** [get_curr_player game] returns the player who is making the decision
    of pass/raise/fold *)

val get_winner : game -> player
(** [get_winner game] returns the player who won. Precondition: game had
    ended *)

val get_all_players : game -> player list
(** [get_all_players game] returns all the players in the game*)

val winner_player_with_pot_added : game -> player
(** only for degugging, delete after done*)

val queue_to_list : 'a Queue.t -> 'a list
(** only for degugging, delete after done*)

val mutable_pop : 'a Queue.t -> 'a Queue.t
(** only for degugging, delete after done*)

val card_to_players :
  player Queue.t -> Card.t -> int -> player Queue.t * Card.t
(** only for degugging, delete after done*)

val player_shift : player Queue.t -> int -> player Queue.t
val init_helper : player Queue.t -> int -> game