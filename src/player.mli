type player

exception InsufficientFund
exception EmptyPlayer of player

val create_player : string -> int -> Card.t -> player
(** [create_player name wealth cards] returns a player with name as
    [name], wealth as [wealth], cards as [cards]. *)

val name : player -> string
val wealth : player -> int
val amount_placed : player -> int
val cards : player -> Card.t

val set_cards : player -> Card.t -> player
(** [set_cards player cards] returns [player] possessed with [cards]. *)

val remove_cards : player -> player
(** [remove_cards player] returns [player] with no cards. *)

val deduct : player -> int -> player
(** [deduct player amount] returns [player] with [amount] deducted from
    their wealth. Raises [InsufficientFund] if player's wealth is less
    than [amount].*)

val add : player -> int -> player
(** [add player amount] returns [player] with [amount] added to their
    wealth. *)
