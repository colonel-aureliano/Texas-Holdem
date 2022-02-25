open OUnit2
open Texas_holdem
open Card

let card_tests =
  [
    ("new_deck test" >:: fun _ -> assert_equal 52 (List.length new_deck));
    ( "random_card test" >:: fun _ ->
      assert_equal 51 (List.length (snd (random_card new_deck))) );
    ( "random_card test" >:: fun _ ->
      print_int
        (match fst (random_card new_deck) with
        | S x | H x | C x | D x -> x) );
    ( "compare test" >:: fun _ ->
      assert_equal 1 (compare (Card.C 1) (Card.C 2)) );
  ]

let suite =
  "test suite for texas_holdem" >::: List.flatten [ card_tests ]

let _ = run_test_tt_main suite
