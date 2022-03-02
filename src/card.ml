type card =
  | S of int
  | H of int
  | C of int
  | D of int

type t = card list

let rec create_new_deck lst =
  match lst with
  | [] -> create_new_deck [ S 1 ]
  | h :: t -> (
      match h with
      | S x ->
          if x = 13 then create_new_deck (H 1 :: lst)
          else create_new_deck (S (x + 1) :: lst)
      | H x ->
          if x = 13 then create_new_deck (C 1 :: lst)
          else create_new_deck (H (x + 1) :: lst)
      | C x ->
          if x = 13 then create_new_deck (D 1 :: lst)
          else create_new_deck (C (x + 1) :: lst)
      | D x -> if x = 13 then lst else create_new_deck (D (x + 1) :: lst)
      )

let new_deck = create_new_deck []

let random_card (t : t) =
  let _ = Random.self_init () in
  let c = List.nth t (Random.int (List.length t - 1)) in
  let new_deck = List.filter (fun x -> x <> c) t in
  (c, new_deck)

let rec n_random_card (t : t) (n : int) =
  match n with
  | 0 -> ([], t)
  | _ ->
      let card, new_deck = random_card t in
      let remaining_cards, new_deck = n_random_card new_deck (n - 1) in
      (card :: remaining_cards, new_deck)

let extract_value (c : card) =
  match c with
  | S x | H x | C x | D x -> x

let single_compare (card1 : card) (card2 : card) =
  let x1 = extract_value card1 in
  let x2 = extract_value card2 in
  match (x1, x2) with
  | 1, 1 -> 0
  | 1, _ -> 1
  | _, 1 -> -1
  | _ -> if x1 > x2 then 1 else if x1 < x2 then -1 else 0

let high_card (hand1 : t) (hand2 : t) =
  let hand1 = List.rev (List.sort single_compare hand1) in
  let hand2 = List.rev (List.sort single_compare hand2) in
  let h1 = List.hd hand1 in
  let h2 = List.hd hand2 in
  single_compare h1 h2

let rec determine_pair (hand : t) =
  let hand = List.rev (List.stable_sort single_compare hand) in
  let hand_temp = List.map extract_value hand in
  match hand_temp with
  | x1 :: x2 :: _ ->
      if x1 = x2 then
        extract_value (List.hd hand) :: determine_pair (List.tl hand)
      else determine_pair (List.tl hand)
  | _ -> []

let has_pair (hand : t) = determine_pair hand <> []

let has_two_pair (hand : t) =
  List.length (List.sort_uniq compare (determine_pair hand)) > 1

let rec has_three_of_a_kind (hand : t) =
  let l = determine_pair hand in
  match l with
  | [] -> false
  | h :: t -> List.mem h t || has_three_of_a_kind (List.tl hand)

let has_straight (hand : t) = failwith "unimplemented"
let has_flush (hand : t) = failwith "unimplemented"
let has_full_house (hand : t) = failwith "unimplemented"

let rec has_four_of_a_kind (hand : t) =
  let l = determine_pair hand in
  match l with
  | [] -> false
  | h1 :: h2 :: t ->
      (h1 = h2 && List.mem h1 (List.tl t))
      || has_three_of_a_kind (List.tl hand)
  | _ -> false

let has_straight_flush (hand : t) = failwith "unimplemented"
let has_royal_flush (hand : t) = failwith "unimplemented"
