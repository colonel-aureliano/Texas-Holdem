type card =
  | S of int
  | H of int
  | C of int
  | D of int
      (** The suits are [S]pades, [H]earts, [C]lubs [D]iamonds.
          Examples:

          - [S 13] represents King of Spades
          - [D 1] represents Ace of Diamonds
          - [H 7] represents 7 of Hearts *)

type t = card list
(** Represents one deck of cards. *)

val new_deck : t
(** Returns a new deck of cards. There are 52 cards in it. *)

val random_card : t -> card * t
(** [random_card deck] returns (c, new_deck) where c is a random card
    from [deck] and new_deck is [deck] with c removed. *)

val extract_value : card -> int

val single_compare : card -> card -> int
(** [compare card1 card2] returns 1 if [card1] is larger than [card2]
    (using poker comparison rules), -1 if [card1] is less than [card2],
    0 if [card1] = [card2]. Examples:

    - [compare (S 3) (S 5)] is [-1]
    - [compare (C 1) (H 12)] is [1] *)

val high_card : t -> t -> int
(** [group_compare_highest_card hand1 hand2] returns 1 if the highest
    card of [hand1] is larger than the highest card of [hand2] (using
    poker comparison rules), -1 if it's less than, 0 if they're equal. *)

val determine_pair : t -> (card * card) list