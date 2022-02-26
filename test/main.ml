open OUnit2
open Texas_holdem
open Card

let single_compare_test
    (name : string)
    (input1 : card)
    (input2 : card)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (single_compare input1 input2)

let high_card_test
    (name : string)
    (input1 : t)
    (input2 : t)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (high_card input1 input2)

let f_test
    (name : string)
    (f : t -> bool)
    (input : t)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (f input)

let hand1 = [ C 5; D 2; H 2; S 6; C 3; C 2; S 10 ] (* three of a kind *)

let hand2 = [ C 5; D 10; H 2; S 6; C 1; C 2; S 10 ] (* two pair *)

let hand3 = [ C 5; D 11; H 8; S 6; C 3; C 2; S 1 ] (* high card *)

let hand4 = [ C 5; D 5; H 5; S 5; C 2; C 1; S 0 ] (* four of a kind *)

let card_tests =
  [
    single_compare_test "single_compare_test greater" (C 1) (H 2) 1;
    single_compare_test "single_compare_test lesser" (D 5) (S 13) ~-1;
    single_compare_test "single_compare_test equal 1" (H 13) (H 13) 0;
    single_compare_test "single_compare_test equal 2" (H 13) (C 13) 0;
    high_card_test "high_card_test 1" hand3 hand1 1;
    high_card_test "high_card_test 2" hand2 hand3 0;
    high_card_test "high_card_test 3" hand1 hand2 ~-1;
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
  ]

let suite =
  "test suite for texas_holdem" >::: List.flatten [ card_tests ]

let _ = run_test_tt_main suite
