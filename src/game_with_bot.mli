(** Representing the status of the poker game

    This module represents the state of current game, including 
    active_players, remaining deck, cards on the table, and other
    essential information. It handles the creations of game, 
    reshuffling of game, reseting the game, adding fund, adding
    new players, executing command, saving & loading files and 
    other functions related getting the state of the game *)

open Player_with_bot

(** The abstract type of values representing game. *)
type game = {
  active_players : player list;
  fold_collection : player list;
  current_deck : Card.t;
  cards_on_table : Card.t;
  pot : int;
  small_blind : player;
  small_blind_amt : int;
  current_bet : int;
  minimum_raise : int;
  consecutive_calls : int;
  new_round : bool;
  winners : player list;
  position : int;
}

(** Raised when the amount raised by current player  is less than 
   previous raise. *)
exception RaiseFailure 

(** Raised when the player with name do not exist in the current game *)
exception PlayerNotFound

(** Raised when current player's name duplicates with existing player *)
exception DuplicateName

(** Raised when less than 2 players is left in the game during the 
    reshuffling parse of the game *)
exception NotEnoughPlayers

(** Raised when the JSON game file attempting to be loaded do not match
    correct format of the game *)
exception BadFormat

(** The abstract type of values representing user command. *)
type command =
  | Call
  | Raise of int
  | Fold 

(** [create_game players pos] initialize the game with [players] passed 
    in from the interface player, and deal hands for each player. First 
    player is at [pos] of the list and will be the small blind. It also 
    automatically move for small and big blind, so the next player to 
    move is the one after big blind. *)
val create_game : player list -> int -> game

(** [play_again g] reinitialize the game with the same players from the
    [g] but shift the small blind to the next person. Other parts are 
    the same as create_game. 
    Precondition: reshuffling period (active_players is sorted by position)*)
val play_again : game -> game

(** [reshuffling_period g] moves all players in [g] to [g]'s active_players 
    and sort. Prepares for adding funds, adding new players, and removing 
    players.
    Precondition: game is over *)
val reshuffling_period : game -> game

(** [add_fund g n amt] adds [amt] to wealth of player with name [n]. 
    Precondition: reshuffling period. 
    Raises: [PlayerNotFound] if player with [n] do not exist, [failwith 
        "negative"] when [amt] is less than 0*)
val add_fund : game -> string -> int -> game

(** [add_player g n w] adds player of name [n] with wealth [w] to
    active_players ofg [g]. 
    Precondition: reshuffling period. 
    Raise: [DuplicateName] if player of [n] already exist in [g], [failwith 
        "negative"] when [w] is negative. *)
val add_player : game -> string -> int -> game

(** [remove_player g n] removes player with name [n] from the [g].
    Precondition: reshuffling period. 
    Raises: [PlayerNotFound] if player with [n] do not exist, 
        [NotEnoughPlayers] if number of players left in the game 
        is less than 2 *)
val remove_player : game -> string -> game

(** [execute_command g cmc] results new game state after updating [g] with 
    the player's command [cmc] *)
val execute_command : game -> command -> game * int

(** [table g] gets the cards on table of game [g]*)
val table: game -> Card.t 

(** [get_curr_player g] returns the player who is currently making the 
    decision of pass/raise/fold in [g]. Read only. *)
val get_curr_player : game -> player

(** [get_winners g] returns the players who won [g]. 
    Precondition: game [g] has ended. Read only. *)
val get_winners : game -> player list

(** [get_winning_hand g] returns the rank of winninng hand. 
    Precondition: game [g] has ended. Read only. *)
val get_winning_hand : game -> string

(** [get_all_players g] returns all the players in the [g], sorted
    by their positions. Read only. *)
val get_all_players : game -> player list

(** [get_legal_moves g] returns the legal moves a player can make
    this turn of [g]. Read only. *)
val get_legal_moves : game -> string list

(** [save_game g n] saves the current game [g] to a [n].json,
    returns true if successful, false otherwise. *)
val save_game : game -> string -> bool

(** [read_game j] returns the game stored in [j]. Raises [BadFormat] if
    [j] is badly formatted. *)
val read_game : Yojson.Basic.t -> game

