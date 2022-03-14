open Player

type game
(** The abstract type of values representing game. *)

type command =
  | Call of int
  | Raise of int
  | Fold

val create_game : player list -> int -> game
(** [create_game] initialize the game with player list passed in from
    the interface player, deal hands for each player, and deal the 
    cards on the table. It also automatically move for small and big
    blind, so the next player to move is the one after big blind *)

val play_again : game -> game
(** [play again] reinitialize the game with the same players from the 
previous game but shift the small blind to the next person. Also deals
the cards similar to create game *)

val get_curr_player : game -> player 
(** [get_curr_player] returns the player who is making the decision of
pass/raise/fold *)

val is_game_over : game -> bool
(** [is_game_over] returns a boolean that determines whether the game
is finished *)

val execute_command : game -> command -> game

val betting_round : game -> game
(** [betting_round] 从一次开牌到下一次开牌*)
(*if number of consecutive calls = # players left -1, then go to next
  round else loop the player queue*)

val drawing_card : game -> game
(** 发牌的动作*)

val pot_distributer : game -> game

val poker_game : game -> game
(** 从第一轮的betting开始 + 发n次牌 *)