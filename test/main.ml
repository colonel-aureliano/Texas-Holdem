open OUnit2
open Texas_holdem
open Card

(* helper function *)
let rec cards_to_string (hand : card list) =
  match hand with
  | [] -> ""
  | h :: t -> (
      match h with
      | S x -> "S " ^ string_of_int x ^ "\n" ^ cards_to_string t
      | C x -> "C " ^ string_of_int x ^ "\n" ^ cards_to_string t
      | H x -> "H " ^ string_of_int x ^ "\n" ^ cards_to_string t
      | D x -> "D " ^ string_of_int x ^ "\n" ^ cards_to_string t)

let n_random_card_test
    (name : string)
    (input1 : card list)
    (input2 : int)
    (expected_output : int) =
  let output = n_random_card input1 input2 in
  (*let _ = print_string (cards_to_string (fst output)) in*)
  name >:: fun _ ->
  assert_equal expected_output (List.length (snd output))

let index_of_highest_hand_test
    (name : string)
    (input : card list list)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (index_of_highest_hand input)

let index_of_highest_hand_fail_test
    (name : string)
    (input : card list list)
    (expected_output : exn) =
  name >:: fun _ ->
  assert_raises expected_output (fun _ -> index_of_highest_hand input)

let hand1 = [ C 5; D 2; H 2; S 6; C 3; C 2; S 10 ] (* three of a kind *)

let hand2 = [ C 5; D 10; H 2; S 6; C 1; C 2; S 10 ] (* two pair *)

let hand3 = [ C 5; C 11; H 8; S 6; C 3; C 2; C 1 ] (* flush *)

let hand4 = [ C 5; D 5; H 5; S 5; C 2; C 1; S 0 ] (* four of a kind *)

let hand5 = [ C 9; C 10; C 11; C 12; C 13; S 8; D 3 ]
(* straight flush *)

let hand6 = [ C 1; D 1; H 1; S 11; C 11; H 9; S 8 ] (* full house *)

let hand7 = [ D 10; D 13; H 1; D 12; C 11; D 11; D 1 ] (* royal flush *)

let hand8 = [ D 10; S 3; H 1; D 12; C 8; H 2; D 4 ]
(* nothing, high = 1 *)

let hand9 = [ S 10; D 4; H 1; D 12; C 5; D 8; S 7 ]
(* nothing, high = 1*)

let card_tests =
  [
    n_random_card_test "n_random_card_test" new_deck 5 47;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand1 hand2 hand3"
      [ hand1; hand3; hand2 ] 1;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand3 hand4 hand5"
      [ hand3; hand4; hand5 ] 2;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand5 hand6 hand7"
      [ hand5; hand6; hand7 ] 2;
    index_of_highest_hand_fail_test
      "index_of_highest_hand_fail_test hand8 hand9" [ hand8; hand9 ]
      (Tied [ hand8; hand9 ]);
  ]

let single_compare_test
    (name : string)
    (input1 : card)
    (input2 : card)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (single_compare input1 input2)

let high_card_test
    (name : string)
    (input : card list)
    (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output (high_card input)

let f_test
    (name : string)
    (f : card list -> bool)
    (input : card list)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (f input)

let card_impl_tests =
  [
    single_compare_test "single_compare_test greater" (C 1) (H 2) 1;
    single_compare_test "single_compare_test lesser" (D 5) (S 13) ~-1;
    single_compare_test "single_compare_test equal 1" (H 13) (H 13) 0;
    single_compare_test "single_compare_test equal 2" (H 13) (C 13) 0;
    high_card_test "high_card_test 1" hand1 10;
    high_card_test "high_card_test 2" hand2 14;
    high_card_test "high_card_test 3" hand3 14;
    f_test "has_pair_test hand1" has_pair hand1 true;
    f_test "has_pair_test hand2" has_pair hand2 true;
    f_test "has_pair_test hand3" has_pair hand3 false;
    f_test "has_pair_test hand4" has_pair hand4 true;
    f_test "has_two_pair_test hand1" has_two_pair hand1 false;
    f_test "has_two_pair_test hand2" has_two_pair hand2 true;
    f_test "has_two_pair_test hand3" has_two_pair hand3 false;
    f_test "has_two_pair_test hand4" has_two_pair hand4 false;
    f_test "has_three_of_a_kind_test hand1" has_three_of_a_kind hand1
      true;
    f_test "has_three_of_a_kind_test hand2" has_three_of_a_kind hand2
      false;
    f_test "has_three_of_a_kind_test hand3" has_three_of_a_kind hand3
      false;
    f_test "has_three_of_a_kind_test hand4" has_three_of_a_kind hand4
      true;
    f_test "has_four_of_a_kind_test hand1" has_four_of_a_kind hand1
      false;
    f_test "has_four_of_a_kind_test hand2" has_four_of_a_kind hand2
      false;
    f_test "has_four_of_a_kind_test hand3" has_four_of_a_kind hand3
      false;
    f_test "has_four_of_a_kind_test hand4" has_four_of_a_kind hand4 true;
    f_test "has_four_of_a_kind_test hand5" has_four_of_a_kind hand5
      false;
    f_test "has_four_of_a_kind_test hand6" has_four_of_a_kind hand6
      false;
    f_test "has_four_of_a_kind_test hand7" has_four_of_a_kind hand7
      false;
    f_test "has_straight_test hand5" has_straight hand5 true;
    f_test "has_straight_test hand4" has_straight hand4 false;
    f_test "has_straight_test hand3" has_straight hand3 false;
    f_test "has_straight_test hand2" has_straight hand2 false;
    f_test "has_straight_test hand1" has_straight hand1 false;
    f_test "has_flush_test hand3" has_flush hand3 true;
    f_test "has_flush_test hand2" has_flush hand2 false;
    f_test "has_flush_test hand1" has_flush hand1 false;
    f_test "has_full_house_test hand6" has_full_house hand6 true;
    f_test "has_full_house_test hand3" has_full_house hand3 false;
    f_test "has_straight_flush_test hand5" has_straight_flush hand5 true;
    f_test "has_straight_flush_test hand4" has_straight_flush hand4
      false;
    f_test "has_straight_flush_test hand6" has_straight_flush hand6
      false;
    f_test "has_royal_flush_test hand7" has_royal_flush hand7 true;
    f_test "has_royal_flush_test hand3" has_royal_flush hand3 false;
  ]

let suite =
  "test suite for texas_holdem"
  >::: List.flatten [ card_tests; card_impl_tests ]

let _ = run_test_tt_main suite
