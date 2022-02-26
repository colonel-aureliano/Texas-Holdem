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
(** [random_card deck] returns [(c, new_deck)] where [c] is a random
    card from [deck] and [new_deck] is [deck] with [c] removed. *)

val n_random_card : t -> int -> t * t
(** [n_random_card deck amount] returns [(cards, new_deck)] where
    [cards] is a list of distinct random cards from [deck] and
    [new_deck] is [deck] with elements of [cards] removed. Requires:
    [amount] >= 0. *)

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

val has_pair : t -> bool
(** [has_pair hand] returns true if there is at least one pair in
    [hand], false otherwise. *)

val has_two_pair : t -> bool
(** [has_two_pair hand] returns true if there is at least two distinct
    pairs in [hand], false otherwise. Examples:

    - [has_two_pair \[C 5; D 5; S 4; H 4; S 2; S 1; H 1\]] is true
    - [has_two_pair \[C 5; D 5; S 5; H 5; S 2; S 1; H 1\]] is true
    - [has_two_pair \[C 5; D 5; S 5; H 5; S 2; S 1; H 0\]] is false*)

val has_three_of_a_kind : t -> bool
(** [has_three_of_a_kind hand] returns true if there is at least three
    cards of a kind in [hand], false otherwise.*)

val has_four_of_a_kind : t -> bool
(** [has_four_of_a_kind hand] returns true if there is at least four
    cards of a kind in [hand], false otherwise.*)