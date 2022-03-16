open Texas_holdem
open Game
open Player
open Card

exception Exit

(** [prompt] asks user to enter a parseable command. input of "exit"
    exits the game. Empty input is Call. *)
let parse x : command =
  print_string "> ";
  match
    String.(
      read_line () |> trim |> lowercase_ascii |> split_on_char ' ')
  with
  | [ "fold" ] -> Fold
  | [ "call" ] | [ "raise"; "0" ] | [ "" ] -> Call
  | [ "raise"; n ] ->
      let n = int_of_string n in
      if n > 0 then Raise n else failwith "Illegal Command"
  | [ "exit" ] -> raise Exit
  | _ -> failwith "Illegal Command"

(** Prompts for command until get a valid command*)
let rec get_command game : game =
  print_endline "\nEnter your move: ";
  try
    let command = parse 0 in
    execute_command game command
  with
  | Failure _ ->
      print_endline "Illegal Command";
      get_command game
  | InsufficientFund ->
      print_endline "Insufficient Fund";
      get_command game

(** [player_result] prints the naeme and wealth of all players *)
let rec player_result = function
  | [] -> print_string ""
  | h :: t ->
      player_result t;
      name h ^ ": $" ^ string_of_int (wealth h) |> print_endline

(** [end_game] shows the result of the game and asks whether to play
    again *)
let rec end_game game =
  print_endline "\n\nThis game is over.";
  let winner = get_winner game in
  "The winner is: " ^ name winner |> print_endline;
  print_endline "\nPlayer Status";
  let players = get_all_players game |> List.rev in
  player_result players;
  print_endline "\nWould you like to start another game? (Y/N)";
  print_string "> ";
  match String.(read_line () |> trim |> lowercase_ascii) with
  | "y" ->
      print_endline "new game started";
      play (play_again game)
  | _ -> print_endline "\nbye"

(** [play] loops through plyaers, displaying relevant information and
    asks for command*)
and play game =
  if game.game_over = true then end_game game
  else
    let p = get_curr_player game in
    "\nThe next player is " ^ name p ^ "." |> print_endline;
    print_endline "Press Enter to confirm.";
    print_string (read_line ());
    "\n\n\n\nTable: " ^ to_string game.cards_on_table |> print_endline;
    "\nHello, " ^ name p ^ "!" |> print_endline;
    "Your Hand: " ^ to_string (cards p) |> print_endline;
    "\nYour wealth is $" ^ string_of_int (wealth p) ^ "."
    |> print_endline;
    "The pot has $" ^ string_of_int game.pot ^ "." |> print_endline;
    if p = game.small_blind then print_endline "You are small blind."
    else print_string "";
    "Highest bet on the table is $"
    ^ string_of_int game.current_bet
    ^ "."
    |> print_endline;
    "Your current bet is $" ^ string_of_int (amount_placed p) ^ "."
    |> print_endline;
    try
      let game = get_command game in
      print_endline "successful";
      play game
    with Exit -> print_endline "exit code 0"

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
        100
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
        "Warning: small blind is now set to $5 by default. \n";
      5
  in
  "The small blind is $" ^ string_of_int sb ^ "." |> print_endline;
  print_endline "\n\nsetup completed";
  print_endline "the small blind and big blind are placed by dealer\n";
  create_game players sb |> play

(** Exectue game enegine *)
let () =
  print_endline "\n\nWelcome to poker.\n";
  setup ()
