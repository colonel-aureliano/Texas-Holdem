open OUnit2
open Texas_holdem
open Card
open Game

let winner_player_with_pot_added_test
    (name : string)
    (input : game)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.name (Game.winner_player_with_pot_added input))

let get_small_blind_test
    (name : string)
    (input : game)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (Player.name input.small_blind)

let get_curr_player_test
    (name : string)
    (input : game)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.name (Game.get_curr_player input))

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

let hand4_debug = [ C 1; D 1; S 9; H 3; C 9; D 9; H 11 ]
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

(* tests for game*)

let test1 = [ H 3; C 6; H 8; H 5; S 2; D 10; S 7 ]
let test2 = [ H 3; H 2; H 5; H 8; C 7; S 2; C 6 ]
let test3 = [ H 3; H 4; H 5; H 8; D 12; S 2; C 6 ]
let hand_straight_0 = [ C 2; S 3; C 4; D 4; H 5; C 6; D 1 ]
let hand_straight_1 = [ H 2; S 3; D 4; C 4; H 5; D 10; D 1 ]
let buggy_hand_0 = [ S 2; H 7; C 12; H 12; C 9; D 1; H 5 ]
let buggy_hand_1 = [ S 2; H 7; C 12; H 12; C 9; D 2; S 5 ]

let card_tests =
  [
    index_of_highest_hand_test "testing buggy hands"
      [ buggy_hand_0; buggy_hand_1 ]
      1;
    index_of_highest_hand_test "testing game outputs"
      [ test1; test2; test3 ] 2;
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
    index_of_highest_hand_test "a small pair and a high"
      [ hand8; hand9 ] 1;
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
    f_test "has_four_of_a_kind_test hand4_debug" has_four_of_a_kind
      hand4_debug false;
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

let h1 = [ H 6; H 5; H 4; S 3; S 2; S 1; S 5 ]
let h2 = [ H 10; H 5; C 4; S 3; S 2; S 1; D 9 ]

let card_debug_tests =
  [ index_of_highest_hand_test "h1 h2" [ h1; h2 ] 0 ]

let player_a = Player.create_player "b" 20 [ D 1; H 5 ]
let player_b = Player.create_player "a" 20 [ D 2; S 5 ]
let player_c = Player.create_player "c" 30 []
let player_d = Player.create_player "d" 30 []

let rec list_to_queue players queue =
  match players with
  | [] -> queue
  | h :: t ->
      list_to_queue t
        (Queue.add h queue;
         queue)

let queue = Queue.create ()
let players_queue = list_to_queue [ player_a; player_b ] queue
let queue1 = Queue.create ()

let players_queue1 =
  list_to_queue [ player_a; player_b; player_c; player_d ] queue1

let queue2 = Queue.create ()

let players_queue2 =
  list_to_queue [ player_b; player_c; player_d; player_a ] queue2

let queue3 = Queue.create ()

let players_queue3 =
  list_to_queue [ player_c; player_d; player_a; player_b ] queue3

let players_queue_with_b_only = list_to_queue [ player_b ] queue
let players_queue_with_a_only = list_to_queue [ player_a ] queue

let g =
  {
    players = Queue.copy players_queue;
    active_players = players_queue;
    current_deck = [];
    cards_on_table = [ S 2; H 7; C 12; H 12; C 9 ];
    pot = 5;
    small_blind = player_a;
    small_blind_amt = 5;
    current_bet = 10;
    (* betting_round = 0; *)
    consecutive_calls = 0;
    game_over = false;
  }

let player_queue_equal q1 q2 : bool =
  List.equal
    (fun x y -> Player.name x = Player.name y)
    (q1 |> Game.queue_to_list)
    (q2 |> Game.queue_to_list)

let game_equal (g1 : game) (g2 : game) : bool =
  player_queue_equal g1.players g2.players
  && player_queue_equal g1.active_players g2.active_players
  && List.equal (fun x y -> x = y) g1.current_deck g2.current_deck
  && List.equal (fun x y -> x = y) g1.cards_on_table g2.cards_on_table
  && g1.pot = g2.pot
  && Player.name g1.small_blind = Player.name g2.small_blind
  && g1.small_blind_amt = g2.small_blind_amt
  && g1.current_bet = g2.current_bet
  && g1.consecutive_calls = g2.consecutive_calls
  && g1.game_over = g2.game_over

let update_fold_state_test
    (name : string)
    (input : game)
    (pass : bool)
    (expected_output : game) =
  name >:: fun _ ->
  assert_equal pass
    (game_equal expected_output (Game.update_fold_state input))

let create_game_test
    (name : string)
    (input : Player.player list)
    (pass : bool)
    (expected_output : game) =
  name >:: fun _ ->
  assert_equal pass
    (player_queue_equal expected_output.players
       (Game.create_game input 1).players
    && player_queue_equal expected_output.active_players
         (Game.create_game input 1).active_players)

let init_helper_test
    (name : string)
    (input : Player.player Queue.t)
    (pass : bool)
    (expected_output : game) =
  name >:: fun _ ->
  assert_equal pass
    (player_queue_equal expected_output.players
       (Game.init_helper input 1).players
    && player_queue_equal expected_output.active_players
         (Game.init_helper input 1).active_players)

let card_to_players_test
    (name : string)
    (input : Player.player Queue.t)
    (pass : bool)
    (expected_output : Player.player Queue.t) =
  name >:: fun _ ->
  let queue, _ = Game.card_to_players input new_deck 0 in
  assert_equal pass (player_queue_equal expected_output queue)

let player_shift_test
    (name : string)
    (input : Player.player Queue.t)
    (pass : bool)
    (expected_output : Player.player Queue.t) =
  name >:: fun _ ->
  assert_equal pass
    (player_queue_equal expected_output (Game.player_shift input 0))

let player_double_shift_test
    (name : string)
    (input : Player.player Queue.t)
    (pass : bool)
    (expected_output : Player.player Queue.t) =
  name >:: fun _ ->
  let queue1 = Game.player_shift input 0 in
  let queue2 = Game.player_shift queue1 0 in
  assert_equal pass (player_queue_equal expected_output queue2)

let player_list = [ player_a; player_b ]
let g_by_init = create_game player_list 5

let get_small_blind_tests =
  [
    get_small_blind_test "use game init" g_by_init "b";
    get_small_blind_test "use game created by hard coding" g "b";
  ]

let get_curr_player_tests =
  [
    get_curr_player_test "use game init" g_by_init "b";
    get_curr_player_test "use game created by hard coding" g "b";
  ]

let winner_tests =
  [ (*winner_player_with_pot_added_test "buggy hands" g "a"*) ]

let create_game_tests =
  [
    (*create_game_test "2-players create game" player_list true g;*)
    init_helper_test "2 player game init" players_queue true g;
  ]

let update_fold_state_tests =
  [
    update_fold_state_test "blind no change" g false
      { g with small_blind = player_a };
    update_fold_state_test "active_players no change" g false
      {
        g with
        small_blind = player_b;
        active_players = Queue.copy g.active_players;
      };
    update_fold_state_test "2-player, a folds as sb" g true
      {
        g with
        small_blind = player_b;
        active_players = players_queue_with_b_only;
      };
  ]

let card_to_players_tests =
  [
    card_to_players_test "card distribution" players_queue true
      players_queue;
  ]

let player_shift_tests =
  [
    player_shift_test "player shift wrong" players_queue false
      players_queue1;
    player_shift_test "player shift correct" players_queue1 true
      players_queue2;
    player_double_shift_test "shift twice" players_queue1 true
      players_queue3;
  ]

let suite =
  "test suite for texas_holdem"
  >::: List.flatten
         [
           card_tests;
           card_impl_tests;
           card_debug_tests;
           winner_tests;
           get_small_blind_tests;
           (* update_fold_state_tests; *)
           create_game_tests;
           card_to_players_tests;
           player_shift_tests;
         ]

let _ = run_test_tt_main suite
