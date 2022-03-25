val next_move_easy : Card.t -> Card.t -> int -> int -> Game.command
(** [next_move_easy hand table wealth min_raise] returns the next move
    (Call, Raise x, Fold) of the easy difficulty bot given cards [hand]
    and [table] and states [wealth] and [min_raise]*)
