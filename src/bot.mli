type bot_level =
  | Easy
  | Medium
(* TODO: delete this after Elva pushes *)

val next_move :
  bot_level -> Card.t -> Card.t -> Card.t -> int -> int -> Game.command
(** [next_move difficult hand table deck wealth min_raise] returns the
    next move (Call, Raise x, Fold) of the [difficulty] bot given cards
    [hand] and [table] and states [wealth], [min_raise] and [deck]*)
