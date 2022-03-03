open Player
type game 
(** The abstract type of values representing game. *)

type command =
| Call 
| Raise
| Fold


val create_game : player list -> game 
(** [create_game] initialize the game with player list passed
in from the interface player牌 + 桌上的三张*)

val execute_command: game -> command -> game

val betting_round : game -> game (*if number of consecutive calls = # players left -1, then go to next round else loop the player queue*)
(** [betting_round] 从一次开牌到下一次开牌*)

val drawing_card : game -> game  
(** 发牌的动作*)

val pot_distributer : game -> game 


val poker_game : game -> game 
(** 从第一轮的betting开始 + 发n次牌 *)