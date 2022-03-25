open Card

type bot_level =
  | Easy
  | Medium

let decision_rule
    (my_strength : int)
    (opp_strength : int)
    (wealth : int)
    (min_raise : int) : Game.command =
  if min_raise > wealth then Fold
  else
    let threshold = Random.int 3 in
    if threshold <= my_strength - opp_strength then
      Raise
        (min
           (min_raise + ((my_strength - opp_strength) * (wealth / 20)))
           wealth)
    else if threshold < my_strength - opp_strength + 2 then Call
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
    number of rollouts till 7 cards and returns a list containing the
    hands under those rollouts *)
let rec run_rollouts
    (deck : Card.t)
    (num_rollouts : int)
    (cur_cards : Card.t)
    (num_cards_to_rollout : int) : Card.t list =
  match num_rollouts with
  | 0 -> []
  | _ ->
      let drawn_cards, _ =
        Card.n_random_card deck num_cards_to_rollout
      in
      (drawn_cards @ cur_cards)
      :: run_rollouts deck (num_rollouts - 1) cur_cards
           num_cards_to_rollout

let list_sum lst = List.fold_left (fun x y -> x + y) 0 lst

(** [avg_hand_strength deck num_rollouts hand] runs [num_rollouts]
    number of simulations until completion (7 cards) then returns the
    average hand strength of the 7 cards *)
let rec avg_hand_strength deck num_rollouts hand acc =
  match acc with
  | -1 -> []
  | _ ->
      let rollouts = run_rollouts deck num_rollouts hand acc in
      let avg =
        list_sum (List.map rank_of_hand rollouts) / num_rollouts
      in
      avg :: avg_hand_strength deck num_rollouts hand (acc - 1)

(** [next_move_easy hand table wealth min_raise] returns the next move
    (Call, Raise x, Fold) of the medium difficulty bot given cards
    [hand] and [table] and states [wealth] and [min_raise]. It only
    looks at the projection of its own hand's strength.*)
let next_move_medium
    (hand : Card.t)
    (table : Card.t)
    (deck : Card.t)
    (wealth : int)
    (min_raise : int) : Game.command =
  if List.length table = 0 then
    decision_rule
      (starting_hand_estimated_strength hand)
      0 wealth min_raise
  else
    let num_rollouts = 100 in
    let my_avg_strength =
      (avg_hand_strength deck num_rollouts (hand @ table)
         (5 - List.length table)
      |> list_sum)
      / (5 - List.length table)
    in
    let opp_avg_strength =
      (avg_hand_strength deck num_rollouts table (7 - List.length table)
      |> list_sum)
      / (7 - List.length table)
    in
    decision_rule my_avg_strength opp_avg_strength wealth min_raise

let next_move_easy (wealth : int) (min_raise : int) =
  decision_rule (Random.int 3) 0 wealth min_raise

let next_move
    bot_level
    (hand : Card.t)
    (table : Card.t)
    (deck : Card.t)
    (wealth : int)
    (min_raise : int) : Game.command =
  match bot_level with
  | Easy -> next_move_easy wealth min_raise
  | Medium -> next_move_medium hand table deck wealth min_raise
