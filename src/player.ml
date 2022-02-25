type player = {
  name : string;
  wealth : int;
  cards : Card.t;
}

let create_player
    (input_name : string)
    (input_wealth : int)
    (input_cards : Card.t) =
  { name = input_name; wealth = input_wealth; cards = input_cards }

let name player = player.name
let wealth player = player.wealth
let cards player = player.cards
let fold player = { player with cards = [] }
let raise player amount = failwith "unimplemented"
let call player amount = failwith "unimplemented"
