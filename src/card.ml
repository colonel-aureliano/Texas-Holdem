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
  let c = List.nth t (Random.int (List.length t)) in
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

let high_card (hand : t) =
  let hand = List.rev (List.sort single_compare hand) in
  let hand = List.map extract_value hand in
  if List.hd hand = 1 then 14 else List.hd hand

(** Returns an int list of non-unique elements found in [hand]. The
    actual number of cards of value v in [hand] is the number of
    occurrences of v in [determine_pair hand] + 1. Examples:
    [hand \[ C 5; D 2; H 2; C 2; S 10 \]] is [2;2]*)
let rec determine_pair (hand : t) =
  let hand = List.rev (List.stable_sort single_compare hand) in
  let intlist = List.map extract_value hand in
  match intlist with
  | x1 :: x2 :: _ ->
      if x1 = x2 then
        extract_value (List.hd hand) :: determine_pair (List.tl hand)
      else determine_pair (List.tl hand)
  | _ -> []

let has_pair (hand : t) = determine_pair hand <> []

let has_two_pair (hand : t) =
  List.length (List.sort_uniq compare (determine_pair hand)) > 1

let rec has_three_of_a_kind_helper l =
  match l with
  | [] -> false
  | h :: t -> List.mem h t || has_three_of_a_kind_helper t

let has_three_of_a_kind (hand : t) =
  let l = determine_pair hand in
  has_three_of_a_kind_helper l

let rec has_straight_helper l =
  if List.length l < 5 then false
  else
    let a = List.hd l in
    let b = a + 1 in
    let c = b + 1 in
    let d = c + 1 in
    let e = d + 1 in
    List.nth l 1 = b
    && List.nth l 2 = c
    && List.nth l 3 = d
    && List.nth l 4 = e
    || has_straight_helper (List.tl l)

let has_straight (hand : t) =
  let hand = List.stable_sort single_compare hand in
  let intlist = List.map extract_value hand in
  let intlist = List.map (fun x -> if x = 1 then 14 else x) intlist in
  has_straight_helper intlist

let has_flush_helper (hand : t) =
  let s =
    List.fold_left
      (fun count x ->
        match x with
        | S _ -> count + 1
        | _ -> count)
      0 hand
  in
  if s > 4 then (true, 'S')
  else
    let h =
      List.fold_left
        (fun count x ->
          match x with
          | H _ -> count + 1
          | _ -> count)
        0 hand
    in
    if h > 4 then (true, 'H')
    else
      let c =
        List.fold_left
          (fun count x ->
            match x with
            | C _ -> count + 1
            | _ -> count)
          0 hand
      in
      if c > 4 then (true, 'C')
      else
        let d =
          List.fold_left
            (fun count x ->
              match x with
              | D _ -> count + 1
              | _ -> count)
            0 hand
        in
        if d > 4 then (true, 'D') else (false, ' ')

let has_flush (hand : t) = fst (has_flush_helper hand)

let rec has_full_house_helper l b =
  match l with
  | h :: t ->
      if List.mem h t then
        b
        ||
        let rest = List.filter (fun x -> x != h) t in
        List.length rest > 0
      else has_full_house_helper t true
  | _ -> false

let has_full_house (hand : t) =
  let l = determine_pair hand in
  has_full_house_helper l false

let rec has_four_of_a_kind (hand : t) =
  let l = determine_pair hand in
  if List.length l < 3 then false
  else
    match l with
    | h1 :: h2 :: t ->
        (h1 = h2 && List.mem h1 (List.tl t))
        || has_three_of_a_kind (List.tl hand)
    | _ -> false

let has_straight_flush (hand : t) =
  let suit = snd (has_flush_helper hand) in
  match suit with
  | 'S' ->
      hand
      |> List.filter (fun x ->
             match x with
             | S x -> true
             | _ -> false)
      |> has_straight
  | 'H' ->
      hand
      |> List.filter (fun x ->
             match x with
             | H x -> true
             | _ -> false)
      |> has_straight
  | 'C' ->
      hand
      |> List.filter (fun x ->
             match x with
             | C x -> true
             | _ -> false)
      |> has_straight
  | 'D' ->
      hand
      |> List.filter (fun x ->
             match x with
             | D x -> true
             | _ -> false)
      |> has_straight
  | _ -> false

let sort_and_group hand =
  let s =
    List.filter
      (fun x ->
        match x with
        | S x -> true
        | _ -> false)
      hand
  in
  let s = List.rev (List.stable_sort single_compare s) in
  let h =
    List.filter
      (fun x ->
        match x with
        | H x -> true
        | _ -> false)
      hand
  in
  let h = List.rev (List.stable_sort single_compare h) in
  let c =
    List.filter
      (fun x ->
        match x with
        | C x -> true
        | _ -> false)
      hand
  in
  let c = List.rev (List.stable_sort single_compare c) in
  let d =
    List.filter
      (fun x ->
        match x with
        | D x -> true
        | _ -> false)
      hand
  in
  let d = List.rev (List.stable_sort single_compare d) in
  s @ h @ c @ d

let rec has_royal_flush_helper hand =
  match hand with
  | S 1 :: S 13 :: S 12 :: S 11 :: S 10 :: _ -> true
  | H 1 :: H 13 :: H 12 :: H 11 :: H 10 :: _ -> true
  | C 1 :: C 13 :: C 12 :: C 11 :: C 10 :: _ -> true
  | D 1 :: D 13 :: D 12 :: D 11 :: D 10 :: _ -> true
  | _ ->
      if List.length hand < 5 then false
      else has_royal_flush_helper (List.tl hand)

let has_royal_flush (hand : t) =
  let hand = sort_and_group hand in
  has_royal_flush_helper hand

let rec rank_hands (lst : t list) =
  match lst with
  | [] -> []
  | h :: t ->
      let rank =
        if has_royal_flush h then 9
        else if has_straight_flush h then 8
        else if has_four_of_a_kind h then 7
        else if has_full_house h then 6
        else if has_flush h then 5
        else if has_straight h then 4
        else if has_three_of_a_kind h then 3
        else if has_two_pair h then 2
        else if has_pair h then 1
        else 0
      in
      (rank, h) :: rank_hands t

let rec high_card_extract (lst : t list) =
  (* extracts the high card for each element of lst *)
  match lst with
  | [] -> []
  | h :: t -> (high_card h, h) :: high_card_extract t

let rec high_card_determine_tie lst tied_at =
  match lst with
  | [] -> []
  | h :: t ->
      if fst h = tied_at then snd h :: high_card_determine_tie t tied_at
      else high_card_determine_tie t tied_at

let break_tie (lst : t list) (rank : int) =
  (* elements in [lst] are tied at [rank] *)
  failwith "break tie unimplemented"

exception Tied of t list

let highest_hand_helper (lst : t list) =
  if List.length lst = 1 then List.hd lst
  else
    let lst = rank_hands lst in
    let lst =
      List.rev
        (List.sort
           (fun x y ->
             if fst x > fst y then 1
             else if fst x = fst y then 0
             else -1)
           lst)
    in
    if fst (List.hd lst) != fst (List.nth lst 1) then snd (List.hd lst)
    else if fst (List.hd lst) = 0 then
      let lst = high_card_extract (snd (List.split lst)) in
      let lst =
        List.rev
          (List.sort
             (fun x y ->
               if fst x > fst y then 1
               else if fst x = fst y then 0
               else -1)
             lst)
      in
      if fst (List.hd lst) != fst (List.nth lst 1) then
        snd (List.hd lst)
      else
        raise (Tied (high_card_determine_tie lst (fst (List.hd lst))))
    else
      let _ = print_int (fst (List.hd lst)) in
      let lst = List.filter (fun x -> fst x = fst (List.hd lst)) lst in
      List.hd (break_tie (snd (List.split lst)) (fst (List.hd lst)))

let rec find_index_of_element e list index =
  match list with
  | [] -> -1
  | h :: t ->
      if h = e then index else find_index_of_element e t (index + 1)

let index_of_highest_hand (lst : t list) =
  let hand = highest_hand_helper lst in
  find_index_of_element hand lst 0
