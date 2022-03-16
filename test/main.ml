open OUnit2
open Texas_holdem
open Card

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

let hand1_match = [ S 10; D 10; H 10; D 12; C 5; D 8; S 7 ]
(* three of a kind, higher than hand1 *)

let hand2 = [ C 5; D 10; H 2; S 6; C 1; C 2; S 10 ] (* two pair *)

let hand2_match_0 = [ C 5; D 11; H 2; S 6; C 1; C 2; S 11 ]
(* two pair, higher than hand2 *)

let hand2_match_1 = [ C 5; D 10; H 3; S 6; C 1; C 3; S 10 ]
(* two pair, higher than hand2 *)

let hand3 = [ C 5; C 11; H 8; S 6; C 3; C 2; C 1 ] (* flush *)

let hand3_match = [ C 5; C 13; H 8; S 6; C 3; C 2; C 4 ]
(* flush, less than hand3 *)

let hand4 = [ C 5; D 5; H 5; S 5; C 2; C 1; S 0 ] (* four of a kind *)

let hand4_match = [ C 6; D 6; H 6; S 6; C 2; C 1; S 0 ]
(* four of a kind, higher than hand4 *)

let hand5 = [ C 9; C 10; C 11; C 12; C 13; S 8; D 3 ]
(* straight flush *)

let hand5_match = [ D 8; D 9; D 10; D 11; D 12; D 2; D 3 ]
(* straight flush, less than hand5 *)

let hand6 = [ C 1; D 1; H 1; S 11; C 11; H 9; S 8 ] (* full house *)

let hand6_match = [ C 2; D 2; H 2; S 11; C 11; H 9; S 8 ]
(* full house, less than hand6 *)

let hand7 = [ D 10; D 13; D 1; D 12; D 11; C 11; H 1 ] (* royal flush *)

let hand7_match = [ C 10; C 13; C 1; C 12; C 11; D 11; D 1 ]
(* royal flush *)

let hand8 = [ D 10; S 3; H 1; D 12; C 8; H 2; D 4 ]
(* nothing, high = 1 *)

let hand8_match = [ S 10; D 4; H 1; D 12; C 5; D 8; S 7 ]
(* nothing, high = 1*)

let hand9 = [ H 1; S 1; H 10; S 9; H 3; D 6; D 4 ] (* a pair *)

let hand9_match_1 = [ H 11; S 11; H 10; S 9; H 3; D 6; D 4 ]
(* a pair, less than hand9 *)

let hand9_match_2 = [ D 1; C 1; H 10; S 9; H 3; D 6; D 4 ]
(* a pair, the same as hand9 *)

let hand10 = [ H 10; S 9; C 8; S 7; H 2; H 11; S 12 ] (* a straight *)

let hand10_match = [ H 10; S 9; C 8; S 7; H 11; S 12; H 13 ]
(* a straight, higher than hand10 *)

let hand_straight_0 = [ C 2; S 3; C 4; D 4; H 5; C 6; D 1 ]
let hand_straight_1 = [ H 2; S 3; D 4; C 4; H 5; D 10; D 1 ]

let card_tests =
  [
    index_of_highest_hand_test
      "index_of_highest_hand_test hand_straight_0 hand_straight_1"
      [ hand_straight_0; hand_straight_1 ]
      0;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand1 hand2 hand3"
      [ hand1; hand3; hand2 ] 1;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand3 hand4 hand5"
      [ hand3; hand4; hand5 ] 2;
    index_of_highest_hand_test
      "index_of_highest_hand_test hand5 hand6 hand7"
      [ hand5; hand6; hand7 ] 2;
    index_of_highest_hand_test "index_of_highest_hand_test hand1-8"
      [ hand1; hand2; hand3; hand4; hand5; hand6; hand7; hand8 ]
      6;
    (* Below are tests for tie or tie breakers. *)
    (* test of high card *)
    index_of_highest_hand_fail_test "0 tie_test hand8 hand9"
      [ hand8; hand8_match ]
      (Tie [ 0; 1 ]);
    (* test of pair *)
    index_of_highest_hand_test "1 tie_test hand9 hand9_match_1"
      [ hand9; hand9_match_1 ]
      0;
    index_of_highest_hand_fail_test
      "1 tie_test hand9 hand9_match_1 hand9_match_2"
      [ hand9; hand9_match_1; hand9_match_2 ]
      (Tie [ 2; 0 ]);
    (* test of two pair *)
    index_of_highest_hand_test "2 tie_test hand2 hand2_match_0"
      [ hand2; hand2_match_0 ]
      1;
    index_of_highest_hand_test "2 tie_test hand2 hand2_match_1"
      [ hand2; hand2_match_1 ]
      1;
    index_of_highest_hand_test
      "2 tie_test hand2 hand2_match_0 hand2_match_1"
      [ hand2; hand2_match_0; hand2_match_1 ]
      1;
    (* test of three of a kind *)
    index_of_highest_hand_test "3 tie_test hand1 hand1_match"
      [ hand1; hand1_match ] 1;
    index_of_highest_hand_fail_test
      "3 tie_test hand1 hand1_match hand1_match"
      [ hand1; hand1_match; hand1_match ]
      (Tie [ 1; 1 ]);
    (* test of straight *)
    index_of_highest_hand_test "4 tie_test hand10 hand10_match"
      [ hand10_match; hand10; hand1 ]
      0;
    index_of_highest_hand_fail_test "4 tie_test hand10 hand10"
      [ hand10; hand10 ]
      (Tie [ 0; 0 ]);
    (* test of flush *)
    index_of_highest_hand_test "5 tie_test hand3 hand3_match"
      [ hand3; hand3_match ] 0;
    index_of_highest_hand_fail_test "5 tie_test hand3 hand3"
      [ hand3; hand3 ]
      (Tie [ 0; 0 ]);
    (* test of four of a kind *)
    index_of_highest_hand_test "7 tie_test hand4 hand4_match"
      [ hand4; hand4_match ] 1;
    index_of_highest_hand_fail_test "7 tie_test hand1 hand4 hand4"
      [ hand1; hand4; hand4 ]
      (Tie [ 1; 1 ]);
    (* test of full house *)
    index_of_highest_hand_test "6 tie_test hand6 hand6_match_0 "
      [ hand6; hand6_match ] 0;
    (* test of straight flush *)
    index_of_highest_hand_test "8 tie_test hand5 hand5_match"
      [ hand5; hand5_match ] 0;
    index_of_highest_hand_fail_test "8 tie_test hand5 hand5"
      [ hand5; hand5 ]
      (Tie [ 0; 0 ]);
    (* test of royal flush *)
    index_of_highest_hand_fail_test "9 tie_test hand7 hand7_match"
      [ hand7; hand7_match ]
      (Tie [ 1; 0 ]);
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
    f_test "has_straight" has_straight hand_straight_1 true;
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

let h1 = [ S 3; S 6; C 11; H 6; S 5; H 10; S 7 ]
let h2 = [ S 3; S 6; C 11; H 6; S 5; S 3; D 5 ]

let card_debug_tests =
  [ index_of_highest_hand_test "h1 h2" [ h1; h2 ] 1 ]

let suite =
  "test suite for texas_holdem"
  >::: List.flatten [ card_tests; card_impl_tests; card_debug_tests ]

let _ = run_test_tt_main suite
