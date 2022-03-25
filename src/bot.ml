open Card

let easy_decision_rule
    (threshold : int)
    (strength : int)
    (wealth : int)
    (min_raise : int) : Game.command =
  if threshold < strength then
    Raise (min_raise + ((strength - threshold) * (wealth / 20)))
  else if threshold < strength + 2 then Call
  else Fold

let next_move_easy
    (hand : Card.t)
    (table : Card.t)
    (wealth : int)
    (min_raise : int) : Game.command =
  let ran_int = Random.int 10 in
  if List.length (hand @ table) = 2 then
    easy_decision_rule ran_int wealth min_raise
      (2 * starting_hand_estimated_strength hand)
  else
    easy_decision_rule ran_int
      (rank_of_hand (hand @ table))
      wealth min_raise
