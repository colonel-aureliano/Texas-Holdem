open Texas_holdem
open Game
open Player
open Card

(** [prompt] asks user to enter a parseable command*)
let parse x : command =
  match
    String.(
      read_line () |> trim |> lowercase_ascii |> split_on_char ' ')
  with
  | [ "fold" ] -> Fold
  | [ "call"; n ] -> Call (int_of_string n)
  | [ "raise"; n ] -> Raise (int_of_string n)
  | _ -> failwith "Invalid Move"

(** Prompts for command until get a valid command*)
let rec get_command game : game =
  print_endline "\nEnter your move: ";
  print_string "> ";
  try
    let command = parse 0 in
    betting_round game command
  with Failure _ ->
    print_endline "Invalid Move";
    get_command game

(** [end_game] shows the result of the game and asks whether to play
    again *)
let end_game game = print_endline "This game is over."

(** [play] loops through plyaers, displaying relevant information and
    asks for command*)
let rec play game =
  if game.game_over = true then end_game game
  else
    let p = get_curr_player game in
    "\nThe next player is " ^ name p ^ "." |> print_endline;
    print_endline "Press Enter to confirm.";
    print_string (read_line ());
    "Table: " ^ to_string game.cards_on_table |> print_endline;
    "\nHello, " ^ name p ^ "!" |> print_endline;
    "Your Hand: " ^ to_string (cards p) |> print_endline;
    "Your wealth is $" ^ string_of_int (wealth p) ^ "." |> print_endline;
    if p = game.small_blind then print_endline "You are small blind."
    else
      "Current highest bet is $" ^ string_of_int game.current_bet ^ "."
      |> print_endline;
    get_command game |> play

(** [create_players n i ls] adds [n] players to [ls]. Prompts each
    player to enter in their names and initial wealth. Default player
    name: player[i]. Default wealth: 50. Default cards: a full deck. *)
let rec create_players n i (ls : player list) =
  if i > n then ls
  else
    let _ =
      "\n*** Player" ^ string_of_int i ^ " ***" |> print_endline;
      print_endline "\nEnter your name: ";
      print_string "> "
    in
    let name =
      match read_line () with
      | "" ->
          print_endline "Warning: name cannot be empty.";
          "player" ^ string_of_int i
      | name -> name
    in
    "Hello " ^ name ^ "!" |> print_endline;
    print_endline "\nEnter your wealth: ";
    print_string "> ";
    let wealth =
      try read_line () |> int_of_string
      with Failure _ ->
        print_endline "Warning: wealth must be an integer.";
        50
    in
    "Your initial wealth is $" ^ string_of_int wealth ^ "."
    |> print_endline;
    let player = create_player name wealth new_deck in
    create_players n (i + 1) (player :: ls)

(** [setup] sets up the initial state of the game. *)
let setup () =
  print_endline "Please enter the number of players.\n";
  print_string "> ";
  let n =
    try read_line () |> int_of_string
    with Failure _ ->
      print_endline
        "Warning: number of players is now set to 2 by default. \n";
      2
  in
  let players = create_players n 1 [] in
  print_endline "\nPlease enter the amount of small blind.\n";
  print_string "> ";
  let sb =
    try read_line () |> int_of_string
    with Failure _ ->
      print_endline
        "Warning: small blind is now set to 50 by default. \n";
      50
  in
  "The small blind is $" ^ string_of_int sb ^ "." |> print_endline;
  print_endline "\nsetup completed";
  create_game players sb |> play

(** Exectue game enegine *)
let () =
  print_endline "\n\nWelcome to poker.\n";
  setup ()
