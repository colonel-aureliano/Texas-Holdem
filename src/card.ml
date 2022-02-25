type card =
  | S of int
  | H of int
  | C of int
  | D of int

type t = card list

exception NotSameSuit

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
  let c = List.nth t (Random.int 52) in
  let new_deck = List.filter (fun x -> x <> c) t in
  (c, new_deck)

let compare (card1 : card) (card2 : card) =
  match (card1, card2) with
  | S x1, S x2 | H x1, H x2 | C x1, C x2 | D x1, D x2 -> (
      match (x1, x2) with
      | 1, 1 -> 0
      | 1, _ -> 1
      | _, 1 -> -1
      | _ -> if x1 > x2 then 1 else if x1 < x2 then -1 else 0)
  | _ -> raise NotSameSuit
