(** Representing the player of the poker game

    This module represents the player of current game, including any
    status that is essential to game. It handles creating player,
    reseting their hand & amt placed on the table, setting and
    manipulating their wealth, as well as functions that returns the
    status of the player *)

(** The abstract type of values representing bot's level of intelligence *)
type bot_level = Easy | Medium | Hard | None

type player
(** The abstract type of values representing a player (bot is also a
    player) *)

exception InsufficientFund
(** Raised when player is raising an amount except his wealth *)

val create_player : string -> int -> int -> bool * bot_level -> player
(** [create_player n w p b] returns a player with name as [n], wealth as
    [w], position as [p], and bot as [b] *)

val create_player_full :
  string -> int -> Card.t -> int -> int -> bool * bot_level -> player
(** [create_player_full n w c amt p b] returns a player with name as
    [n], wealth as [w], cards as [c], amount_placed_on_table as [amt],
    position as [p], and bot as [b]*)

val name : player -> string
(** [name p] returns the name of player [p]*)

val wealth : player -> int
(** [wealth p] returns the wealth of player [p]*)

val is_bot : player -> bool * bot_level
(** [is_bot p] returns (bool, bot_level) of player [p]. If bool is true,
    then p is a bot of bot_level. If bool is false, then p is a regular
    player with bot_level of None*)

val amount_placed : player -> int
(** [amount_placed p] returns the amount that [p] has placed on the
    table*)

val cards : player -> Card.t
(** [cards p] returns the cards in hand of [p] *)

val position : player -> int
(** [position p] returns the position of [p] *)

val set_cards : player -> Card.t -> player
(** [set_cards p cards] returns the player after updating [p]'s cards
    with [cards]. *)

val reset_player : player -> player
(** [reset_player p] removes all cards and set amount on table as 0 of
    [p]. *)

val set_wealth : player -> int -> player
(** [set_wealth p amt] returns player after updating [p]'s wealth to
    [amt] *)

val deduct : player -> int -> player
(** [deduct p amt] returns player after deducting [p]'s wealth by [amt].
    Raises [InsufficientFund] if [p]'s wealth is less than [amt].*)

val add : player -> int -> player
(** [add p amt] returns player after adding [amt] to [p]'s wealth. *)
