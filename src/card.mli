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

exception NotSameSuit
(** Raised when two cards of different suits are compared against each
    other. *)

val new_deck : t
(** Returns a new deck of cards. There are 52 cards in it. *)

val random_card : t -> card * card list
(** [random_card deck] returns (c, new_deck) where c is a random card
    from [deck] and new_deck is [deck] with c removed. *)

val compare : card -> card -> int
(** [compare card1 card2] returns 1 if [card1] is larger than [card2]
    (using poker comparison rules), -1 if [card1] is less than [card2],
    0 if [card1] = [card2]. Examples:

    - [compare (S 3) (S 5)] is [-1]
    - [compare (C 1) (C 12)] is [1]

    Raises: [NotSameSuit] if [card1] is not the same suit as [card2]. *)