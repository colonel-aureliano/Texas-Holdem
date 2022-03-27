open Texas_holdem
open Game_with_bot
open Player_with_bot
open Card
open Bot

exception Exit of int
(** 0: exit; 1: save game *)

(** =============================== *)

let get_player_wealth = 
  try
    let raw = read_line () |> int_of_string in
    if raw < 0 then failwith "negative" else raw
  with Failure _ ->
    print_endline "Warning: wealth must be a nonnegative integer.";
    100


let legal_move game : unit = 
  print_endline
    ("\nLegal moves: "
    ^ (get_legal_moves game |> String.concat ", ")
    ^ "\nEnter your move: ")

let rec get_bot_level i ls =
  print_endline "Which level of AI Bot do you want against?";
  (** add possible level *)
  print_string "> ";
  try 
    let level = 
      match read_line () with 
      | "Easy" -> Easy
      | "Medium" -> Medium
      | "Hard" -> Hard
      | _ -> failwith "Illegal Command"
    in
    let wealth = get_player_wealth in
    let player = create_player "AI Bot" wealth (i+1) (true, level) in 
    player :: ls
  with 
  | _ -> 
    print_endline "Illegal Command";
    get_bot_level i ls
  
  let parse_bot_cmd str = 
    match str with
    | [ "fold" ] -> Fold
    | [ "call" ] | [ "raise"; "0" ] | [ "" ] -> Call
    | [ "raise"; n ] ->
        let n = int_of_string n in 
        Raise n
    | _ -> failwith "Illegal Command"
(** =============================== *)

(** [load_file] prompts user to enter game file and converts it to type
    game. x is dummy variable. Condition: requested json file exists in
    game_files, file in right format of a game file. Raises: Exit *)
let rec load_file () : game =
  print_endline "Enter the game file: ";
  print_string "> ";
  let filename = read_line () in
  if filename = "exit" then raise (Exit 0)
  else
    match
      Yojson.Basic.from_file ("game_files/" ^ filename ^ ".json")
      |> read_game
    with
    | exception Sys_error _ ->
        print_endline "file not found\n";
        load_file ()
    | exception BadFormat ->
        print_endline "bad json format\n";
        load_file ()
    | game -> game

let rec player_result_helper = function
  | [] -> print_string ""
  | h :: t ->
      player_result_helper t;
      name h ^ ": $" ^ string_of_int (wealth h) |> print_endline

(** [player_result] prints the naeme and wealth of all players *)
let rec player_result ls = List.rev ls |> player_result_helper

(** [parse] asks user to enter a parseable command. input of "exit"
    exits the game. Empty input and Raise 0 are parsed as Call. *)
let parse game : command =
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
  | [ "exit" ] -> Exit 0 |> raise
  | [ "save"; name ] ->
      if save_game game name then
        print_endline "\nGame saved to game_files folder."
      else print_endline "\nFailed to save game.";
      Exit 1 |> raise
  | _ -> failwith "Illegal Command"

(** [reshuffling_parse] asks user to enter command during reshuffling
    period. *)
let rec reshuffle_parse game : game =
  print_string "\n> ";
  try
    match
      String.(
        read_line () |> trim |> lowercase_ascii |> split_on_char ' ')
    with
    | [ "add"; "fund"; name; amount ] ->
        let game = add_fund game name (int_of_string amount) in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "remove"; "player"; name ] ->
        let game = remove_player game name in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "add"; "player"; name; wealth ] ->
        let game = add_player game name (int_of_string wealth) in
        print_endline "Action succeeded";
        reshuffle_parse game
    | [ "start" ] ->
        let game = play_again game in
        print_endline "\n\nnew game started";
        "small blind : " ^ name game.small_blind |> print_endline;
        print_endline "the blinds are placed by dealer\n";
        game
    | [ "status" ] ->
        player_result game.active_players;
        reshuffle_parse game
    | [ "exit" ] -> Exit 0 |> raise
    | _ -> failwith "Illegal Command"
  with
  | PlayerNotFound ->
      print_endline "Action failed: player not found";
      reshuffle_parse game
  | NotEnoughPlayers ->
      print_endline "Action failed: not enough players ";
      reshuffle_parse game
  | DuplicateName ->
      print_endline "Action failed: duplicate name";
      reshuffle_parse game
  | Failure _ ->
      print_endline
        "Action failed: amount must be a nonnegative integer";
      reshuffle_parse game

(** Prompts for command until get a valid command*)
let rec get_command game : game * int =
  legal_move game;
  try
    let command = parse game in
    execute_command game command
  with
  | Failure _ ->
      print_endline "Illegal Command";
      get_command game
  | InsufficientFund ->
      print_endline "Insufficient Fund";
      get_command game
  | RaiseFailure ->
      print_endline "Insufficient Raise";
      get_command game

(** [end_game] shows the result of the game and asks whether to play
    again *)
let rec end_game game =
  print_endline "\n\nThis game is over.";
  let winners = List.map (fun x -> name x) (get_winners game) in
  let s = if List.length winners = 1 then "Winner: " else "Winners: " in
  s ^ String.concat ", " winners |> print_endline;
  "Winning hand has " ^ get_winning_hand game ^ "." |> print_endline;
  print_endline "\nPlayer Status";
  let players = get_all_players game in
  player_result players;
  print_endline "\nWould you like to start another game? (Y/N)";
  print_string "> ";
  match String.(read_line () |> trim |> lowercase_ascii) with
  | "y" | "" -> reshuffling_period game |> reshuffle
  | _ -> print_endline "\nbye\n"

and reshuffle game =
  print_endline "\n\n\n\nReshuffling Period";
  print_endline
    "Commands Menu: \n\
    \ Add Fund [Name] [Amount] \n\
    \ Add Player [Name] [Wealth] \n\
    \ Remove Player [Name] \n\
    \ Status \n\
    \ Exit \n\
    \ Start";
  reshuffle_parse game |> play

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
    let (bot, mode) = is_bot p in 
    if bot then 
    (
      let cmd_str = 
        next_move mode (cards p) (table game) game.current_deck 
          (wealth p) (game.minimum_raise)
      in let cmd = parse_bot_cmd cmd_str in
      let _ = execute_command game cmd in
      play game
    )
    else 
      try
        let game, amount = get_command game in
        "$" ^ string_of_int amount ^ " to the pot." |> print_endline;
        play game
      with Exit n ->
        "\nexit code " ^ string_of_int n ^ "\n" |> print_endline

(** [create_players n i ls namels] adds [n] players to [ls]. Prompts
    each player to enter in their names and initial wealth. Default
    player name: player[i]. Default wealth: 100. Default cards: a full
    deck. Checks that the new name cannot be in namels *)
let rec create_players n i (ls : player list) (namels : string list) =
  if i > n then 
  ( 
    print_endline "Want to play with AI Bot? (y/n)";
    print_string "> ";
    match read_line () with 
    | "y" -> get_bot_level i ls
    | "n" -> ls 
    | _ -> 
      print_endline "Warning: No AI bot in this game by defualt. \n";
      ls
  )
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
    "\nHello " ^ name ^ "!" |> print_endline;
    print_endline "Enter your wealth: ";
    print_string "> ";
    let wealth = get_player_wealth in
    "Your initial wealth is $" ^ string_of_int wealth ^ "."
    |> print_endline;
    let player = create_player name wealth i (false, None) in
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
  game

(** Exectue game enegine *)
let () =
  print_endline "\n\nWelcome to poker.\n";
  print_endline
    "Setup Commands: \n\
    \ New: start a new game \n\
    \ Load : load an existing JSON game file";
  print_string "> ";
  try
    let game =
      match read_line () |> String.trim |> String.lowercase_ascii with
      | "new" -> setup ()
      | "load" -> load_file ()
      | _ -> raise (Exit 0)
    in
    print_endline
      "\n\n\
       Play Commands Menu: \n\
      \ Fold \n\
      \ Call \n\
      \ Raise [amount] \n\
      \ Save [name] \n\
      \ Exit";
    play game
  with Exit n -> print_endline "exit code 0"