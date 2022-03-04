open Player

type game = {
  player_queue : player Queue.t;
  consecutive_calls : int;
  pot : int;
  current_deck : Card.t;
  cards_on_table : Card.t;
}

type command =
  | Call of int
  | Raise of int
  | Fold

let create_game players = raise (Failure "Unimplemented")

let player_move (cmd : command) (p : player) : player =
  match cmd with
  | Call x | Raise x -> deduct p x
  | _ -> raise IllegalMove

let execute_command (g : game) (cmd : command) =
  {
    g with
    player_queue =
      (let q = g.player_queue in
       match cmd with
       | Fold ->
           Queue.take q;
           q
       | _ ->
           Queue.add (Queue.take q |> player_move cmd) q;
           q);
    consecutive_calls =
      (match cmd with
      | Call _ -> g.consecutive_calls + 1
      | _ -> g.consecutive_calls);
  }

let drawing_card g = raise (Failure "Unimplemented")
let pot_distributer g = raise (Failure "Unimplemented")
let poker_game g = raise (Failure "Unimplemented")

let betting_round (g : game) : game =
  if g.consecutive_calls = Queue.length g.player_queue then poker_game g
  else
    {
      (drawing_card g) with
      player_queue =
        (let q = g.player_queue in
         Queue.add (Queue.take q) q;
         q);
    }
