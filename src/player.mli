type player

exception FailedDeduction

val create_player : string -> int -> Card.t -> player
(** [create_player name wealth cards] returns a player with name as
    [name], wealth as [wealth], cards as [cards]. *)

val fold : player -> player
(** [fold player] returns [player] with no cards. *)

val raise : player -> int -> player
(** [raise player amount] returns [player] with [amount] deducted from
    their wealth. Raises [FailedDeduction] if player's wealth is less
    than [amount].*)

val call : player -> int -> player
(** [call player amount] returns [player] with [amount] deducted from
    their wealth. Raises [FailedDeduction] if player's wealth is less
    than [amount].*)