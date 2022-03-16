type player = {
  name : string;
  wealth : int;
  cards : Card.t;
  amount_placed_on_table : int;
}

exception InsufficientFund
exception EmptyPlayer of player

let create_player
    (input_name : string)
    (input_wealth : int)
    (input_cards : Card.t) =
  {
    name = input_name;
    wealth = input_wealth;
    cards = input_cards;
    amount_placed_on_table = 0;
  }

let name player = player.name
let wealth player = player.wealth
let amount_placed player = player.amount_placed_on_table
let cards player = player.cards
let set_cards player cards = { player with cards }

let set_wealth player amount = 
  {
    player with 
    wealth = amount;
  }
    
let remove_cards player = { player with cards = [] }

let deduct player amount =
  if player.wealth < amount then raise InsufficientFund
  else
    {
      player with
      wealth = player.wealth - amount;
      amount_placed_on_table = player.amount_placed_on_table + amount;
    }

let add player amount = { player with wealth = player.wealth + amount }
let equal p1 p2 = p1.name = p2.name