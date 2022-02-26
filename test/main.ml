open OUnit2
open Texas_holdem
open Card

let single_compare_test
    (name : string)
    (input1 : Card.card)
    (input2 : Card.card)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (Card.single_compare input1 input2)

let high_card_test
    (name : string)
    (input1 : Card.t)
    (input2 : Card.t)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (Card.high_card input1 input2)

let determine_pair_test
    (name : string)
    (input : Card.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (List.map extract_value
       (fst (List.split (Card.determine_pair input))))

let card_tests =
  [
    ("new_deck test" >:: fun _ -> assert_equal 52 (List.length new_deck));
    ( "random_card" >:: fun _ ->
      assert_equal 51 (List.length (snd (random_card new_deck))) );
    single_compare_test "single_compare_test greater" (Card.C 1)
      (Card.H 2) 1;
    single_compare_test "single_compare_test lesser" (Card.D 5)
      (Card.S 13) ~-1;
    single_compare_test "single_compare_test equal 1" (Card.H 13)
      (Card.H 13) 0;
    single_compare_test "single_compare_test equal 2" (Card.H 13)
      (Card.C 13) 0;
    high_card_test "high_card_test 1"
      [ Card.C 5; Card.D 10; Card.H 1; Card.S 5; Card.C 13 ]
      [ Card.C 12; Card.D 10; Card.H 4; Card.S 9; Card.C 13 ]
      1;
    high_card_test "high_card_test 2"
      [ Card.C 5; Card.D 10; Card.H 13; Card.S 5; Card.C 13 ]
      [ Card.S 12; Card.D 10; Card.C 4; Card.H 9; Card.C 13 ]
      0;
    high_card_test "high_card_test 3"
      [ Card.C 5; Card.D 10; Card.H 2; Card.S 5; Card.C 2 ]
      [ Card.S 12; Card.D 10; Card.C 4; Card.H 9; Card.C 13 ]
      ~-1;
    determine_pair_test "determine_pair_test 1"
      [
        Card.C 5;
        Card.D 10;
        Card.H 2;
        Card.S 6;
        Card.C 3;
        Card.C 2;
        Card.S 10;
      ]
      [ 10; 2 ];
    determine_pair_test "determine_pair_test 2"
      [
        Card.C 5;
        Card.D 11;
        Card.H 8;
        Card.S 6;
        Card.C 3;
        Card.C 2;
        Card.S 1;
      ]
      [];
  ]

let suite =
  "test suite for texas_holdem" >::: List.flatten [ card_tests ]

let _ = run_test_tt_main suite
