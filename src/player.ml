type player = {
  name : string;
  wealth : int;
  cards : Card.t;
}

exception IllegalMove

let create_player
    (input_name : string)
    (input_wealth : int)
    (input_cards : Card.t) =
  { name = input_name; wealth = input_wealth; cards = input_cards }

let name player = player.name
let wealth player = player.wealth
let cards player = player.cards
let set_cards player cards = { player with cards }
let remove_cards player = { player with cards = [] }

let deduct player amount =
  if player.wealth < amount then raise IllegalMove
  else { player with wealth = player.wealth - amount }

let add player amount = { player with wealth = player.wealth + amount }