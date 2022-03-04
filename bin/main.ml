open Texas_holdem
open Game
open Player
open Card

let play game = print_endline "setup completed"

(** [create_players n i ls] adds [n] players to [ls]. Prompts each
    player to enter in their names and initial wealth. Default player
    name: player[i]. Default wealth: 50. Default cards: a full deck. *)
let rec create_players n i (ls : player list) =
  if i > n then ls
  else
    let _ =
      print_endline "\nEnter your name: ";
      print_string "> "
    in
    let name =
      match read_line () with
      | exception End_of_file -> "player" ^ string_of_int i
      | name -> name
    in
    print_endline "\nEnter your wealth: ";
    print_string "> ";
    let wealth =
      match read_line () with
      | exception End_of_file -> 50
      | w -> int_of_string w
    in
    let player = create_player name wealth new_deck in
    create_players n (i + 1) (player :: ls)

(** [setup] sets up the initial state of the game. *)
let setup () =
  print_endline "\n\nWelcome to poker.\n";
  print_endline "Please enter the name of players.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | n -> create_players (int_of_string n) 1 [] |> create_game |> play

(** Exectue game enegine *)
let () = setup ()