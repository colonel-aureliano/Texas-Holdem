open Player
type game = {
    player_queue :  player list;
    consecutive_calls : int;
    pot : int;
    current_deck : Card.t;
    cards_on_table : Card.t
} 

type command =
| Call 
| Raise
| Fold

let create_game players = 
  raise (Failure "Unimplemented")

let execute_command g cmd = 
  raise (Failure "Unimplemented")

let betting_round g = 
  raise (Failure "Unimplemented")

let drawing_card g = 
  raise (Failure "Unimplemented")

let pot_distributer g = 
  raise (Failure "Unimplemented")

let poker_game g = 
  raise (Failure "Unimplemented")
    