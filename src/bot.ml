open Card

type bot_level = Easy

let easy_decision_rule
    (threshold : int)
    (strength : int)
    (wealth : int)
    (min_raise : int) : Game.command =
  if threshold < strength then
    Raise (min_raise + ((strength - threshold) * (wealth / 20)))
  else if threshold < strength + 2 then Call
  else Fold

(* (** [next_move_easy hand table wealth min_raise] returns the next
   move (Call, Raise x, Fold) of the easy difficulty bot given cards
   [hand] and [table] and states [wealth] and [min_raise]. It only looks
   at the current hand to make decisions.*) let next_move_easy (hand :
   Card.t) (table : Card.t) (wealth : int) (min_raise : int) :
   Game.command = let ran_int = Random.int 10 in if List.length (hand @
   table) = 2 then easy_decision_rule ran_int wealth min_raise (2 *
   starting_hand_estimated_strength hand) else easy_decision_rule
   ran_int (rank_of_hand (hand @ table)) wealth min_raise *)

(** [run_rollouts deck num_rollouts cur_cards] simulates [num_rollouts]
    number of rollouts and returns a list containing the hands under
    those rollouts *)
let rec run_rollouts
    (deck : Card.t)
    (num_rollouts : int)
    (cur_cards : Card.t) : Card.t list =
  match num_rollouts with
  | 0 -> []
  | _ ->
      let drawn_cards, _ =
        Card.n_random_card deck (7 - List.length cur_cards)
      in
      (drawn_cards @ cur_cards)
      :: run_rollouts deck (num_rollouts - 1) cur_cards

(** [next_move_easy hand table wealth min_raise] returns the next move
    (Call, Raise x, Fold) of the medium difficulty bot given cards
    [hand] and [table] and states [wealth] and [min_raise]. It only
    looks at the projection of its own hand's strength.*)
let next_move_easy
    (hand : Card.t)
    (table : Card.t)
    (deck : Card.t)
    (wealth : int)
    (min_raise : int) : Game.command =
  let ran_int = Random.int 10 in
  let num_rollouts = 100 in
  let rollout_hands = run_rollouts deck num_rollouts (hand @ table) in
  let average_hand_strength =
    List.fold_left
      (fun x y -> x + y)
      0
      (List.map rank_of_hand rollout_hands)
    / num_rollouts
  in
  easy_decision_rule ran_int average_hand_strength wealth min_raise

let next_move
    bot_level
    (hand : Card.t)
    (table : Card.t)
    (deck : Card.t)
    (wealth : int)
    (min_raise : int) : Game.command =
  match bot_level with
  | Easy -> next_move_easy hand table deck wealth min_raise
