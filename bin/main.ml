open Texas_holdem
open Game
open Player
open Card

exception Exit

(** [prompt] asks user to enter a parseable command. input of "exit"
    exits the game. Empty input and Raise 0 are parsed as Call. *)
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
  print_endline
    ("\nLegal moves are: " ^ get_legal_moves game
   ^ "\nEnter your move: ");
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
  let winners = List.map (fun x -> name x) (get_winners game) in
  "The winner is: " ^ String.concat ", " winners |> print_endline;
  print_endline "\nPlayer Status";
  let players = get_all_players game |> List.rev in
  player_result players;
  print_endline "\nWould you like to start another game? (Y/N)";
  print_string "> ";
  match String.(read_line () |> trim |> lowercase_ascii) with
  | "y" ->
      let game = play_again game in
      print_endline "\n\nnew game started";
      "small blind : " ^ name game.small_blind |> print_endline;
      print_endline "the blinds are placed by dealer\n";
      play game
  | _ -> print_endline "\nbye\n"

(** [play] loops through plyaers, displaying relevant information and
    asks for command*)
and play game =
  if game.game_over = true then end_game game
  else if game.new_round = true then begin
    print_endline "\n\n\n\nNew cards have been dealt. ";
    "Table: " ^ to_string game.cards_on_table |> print_endline;
    play { game with new_round = false }
  end
  else
    let p = get_curr_player game in
    "\n\n\n\nThe next player is " ^ name p ^ "." |> print_endline;
    print_endline "Press Enter to confirm.";
    print_string (read_line ());
    "\nTable: " ^ to_string game.cards_on_table |> print_endline;
    "\nHello, " ^ name p ^ "!" |> print_endline;
    "Your Hand: " ^ to_string (cards p) |> print_endline;
    "\nYour wealth is $" ^ string_of_int (wealth p) ^ "."
    |> print_endline;
    "The pot has $" ^ string_of_int game.pot ^ "." |> print_endline;
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
    with
    | Exit -> print_endline "exit code 0"
    | Tie _ -> print_endline "Tie in Game. Feature to be implemented."

(** [create_players n i ls namels] adds [n] players to [ls]. Prompts
    each player to enter in their names and initial wealth. Default
    player name: player[i]. Default wealth: 100. Default cards: a full
    deck. Checks that the new name cannot be in namels *)
let rec create_players n i (ls : player list) (namels : string list) =
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
      | name ->
          if List.mem name namels then (
            print_endline "Warning: duplicate name.";
            "player" ^ string_of_int i)
          else name
    in
    "Hello " ^ name ^ "!" |> print_endline;
    print_endline "\nEnter your wealth: ";
    print_string "> ";
    let wealth =
      try
        let raw = read_line () |> int_of_string in
        if raw < 0 then failwith "negative" else raw
      with Failure _ ->
        print_endline "Warning: wealth must be a nonnegative integer.";
        100
    in
    "Your initial wealth is $" ^ string_of_int wealth ^ "."
    |> print_endline;
    let player = create_player name wealth new_deck i in
    create_players n (i + 1) (player :: ls) (name :: namels)

(** [setup] sets up the initial state of the game. *)
let setup () =
  print_endline "Please enter the number of players.\n";
  print_string "> ";
  let n =
    try
      let raw = read_line () |> int_of_string in
      if raw <= 0 then failwith "nonpositive" else raw
    with Failure _ ->
      print_endline
        "Warning: number of players is now set to 2 by default. \n";
      2
  in
  let players = create_players n 1 [] [] in
  print_endline "\nPlease enter the amount of small blind.\n";
  print_string "> ";
  let sb =
    try
      let raw = read_line () |> int_of_string in
      if raw < 0 then failwith "negative" else raw
    with Failure _ ->
      print_endline
        "Warning: small blind is now set to $5 by default. \n";
      5
  in
  "The small blind is $" ^ string_of_int sb ^ "." |> print_endline;
  let game = create_game (List.rev players) sb in
  print_endline "\n\nsetup completed";
  "small blind : " ^ name game.small_blind |> print_endline;
  print_endline "the blinds are placed by dealer\n";
  play game

(** Exectue game enegine *)
let () =
  print_endline "\n\nWelcome to poker.\n";
  setup ()
