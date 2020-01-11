(* Citation: All ASCII animal art comes from "Ascii Art Archive" 
   (https://www.asciiart.eu/) *)

open State
open ANSITerminal

type outcome_char = Succ_Char of Character.t | Err_Char of string
type outcome_state = Succ_State of State.t | Err_State of string

(** [valid_char_json char] is the character, represented in type outcome_char,
    corresponding to the string [char]. If [char] corresponds to a valid
    json character file, the result is [Succ_Char]. Otherwise, the result is
    [Err_Char]. 
*)
let valid_char_json char =
  let formatted = char |> String.trim |> String.lowercase_ascii in
  try let json = Yojson.Basic.from_file (formatted ^ ".json") in
    Succ_Char (Character.from_json json)
  with
  | Not_found -> Err_Char "Something wrong with the json file"
  | Yojson.Basic.Util.Type_error (s, _) -> 
    Err_Char ("Type error in json file: " ^ s)
  | _ -> 
    Err_Char "Please enter a valid character name else the program will quit."

(** [res_to_st res] is the outcome state corresponding to the result [res]. *)
let res_to_st res = 
  match res with 
  | Legal st -> Succ_State st
  | Illegal err -> Err_State err

(* We used the example at the bottom of this OCaml file manipulation tutorial
   as a rough outline for this function: 
   https://ocaml.org/learn/tutorials/file_manipulation.html *)
let print_art char_name = 
  let file = (String.lowercase_ascii char_name) ^ ".txt" in
  let input = open_in file in
  let rec print_pic () = 
    match input_line input with
    | line -> 
      print_endline line;         
      print_pic ();
    | exception End_of_file ->                     
      close_in input;  
  in print_pic ();;

(** [parse_command command] is [command] as an action (an action verb and 
    a list of words in the action_phrase).
    Side effects: prints to the console if [command] is not a valid command or 
      if it is empty. 
*)
let rec parse_command command = let open Action in
  try parse_phrase command
  with 
  | Malformed -> 
    print_endline "Command not valid, try again."; 
    parse_command (read_line ())
  | Empty -> 
    print_endline "Your command is empty, try again."; 
    parse_command (read_line ())

(** [printable_list lst] is all the elements in [lst] reformatted with vertical
    bars separating each element. 
*)
let printable_list lst = 
  "[ " ^ (String.concat " | " lst) ^ " ]"

(** [print_market_options st] prints ASCII art representing the vendor based 
    on the current round of state [st], the items the vendor is carrying, the 
    items' effects, the items' costs, and prompts the user to buy or cancel. *)
let print_market_options st = 
  let open ANSITerminal in
  let char = st |> get_char_stat |> who in
  let vendor = if State.get_round st = 2 
    then Vendor.from_vend_json (Yojson.Basic.from_file "vendor1.json") 
    else Vendor.from_vend_json (Yojson.Basic.from_file "vendor2.json") in
  let stock = vendor |> Vendor.get_vend_item_names in
  let coins = char |> Character.get_coins |> string_of_int in
  (print_string [white] ("---------------------------------------" ^
                         "-----------------------------------------\n"));
  let vendor_name = if State.get_round st = 2 then "turtle" else "bear" in
  print_art vendor_name;
  (print_string [white;Bold] ("\nYou come across a kind " ^ vendor_name ^ 
                              " who offers to sell you some items.\n\n"));
  (print_string [white;Bold] ("Stock: \n"));

  (* [print_vend_items name_list] prints the item names of [name_list], the
      effect the item has, and the item's cost *)
  let rec print_vend_items name_list = 
    match name_list with
    |[] -> (print_string [white] (""));
    |h::t -> 
      (print_string [white;Bold] (h ^ "\n"));
      let item = Vendor.find_vend_item h vendor in
      let effect = Character.get_item_effect item |> string_of_int in
      let effect_msg = 
        match item |> Character.get_item_type with
        | "atk" -> "strengthens next attack by " ^ effect ^ "\n"
        | "hp" -> "adds " ^ effect ^ " health\n"
        | "mp" -> "adds " ^ effect ^ " mana\n"
        | _ -> ""
      in (print_string [green] effect_msg);
      (print_string [green] ("costs " ^ 
                             (item |> Character.get_item_cost |> string_of_int) 
                             ^ " coins\n\n"));
      print_vend_items t;
  in print_vend_items stock; 
  (print_string [white] ("---------------------------------------" ^
                         "-----------------------------------------\n"));
  (print_string [white] ("You have " ^ coins ^ " coins.\n"));
  (print_string [white] ("To buy an item, type \"buy\" and the item name.\n");
   (print_string [white] ("To move on without buying anything,\n"); 
    (print_string [white] ("type \"cancel\" \n"))));;

(** [print_items name_list] prints the item names of [name_list] of the player 
    and the effects the items have. *)
let rec print_items name_list st = 
  match name_list with
  |[] -> (print_string [white] (""));
  |h::t -> 
    (print_string [white;Bold] (h ^ "\n"));
    let item = Character.find_item h (st |> get_char_stat |> who) in
    let effect = Character.get_item_effect item |> string_of_int in
    let effect_msg = 
      match item |> Character.get_item_type with
      | "atk" -> "Strengthens next attack by " ^ effect ^ "\n"
      | "hp" -> "Adds " ^ effect ^ " health\n"
      | "mp" -> "Adds " ^ effect ^ " mana\n"
      | _ -> ""
    in (print_string [green] effect_msg);
    (print_string [white] ("---------------------------------------" ^
                           "---------------------------------------------\n"));
    print_items t st;;

(** [print_attacks name_lst] prints the attack names of [name_list] and the 
    damage and mana cost of each attack. *)
let rec print_attacks name_list st = 
  match name_list with
  |[] -> (print_string [white] (""));
  |h::t -> 
    (print_string [white;Bold] (h ^ "\n"));
    let attack = Character.find_attack h (st |> get_char_stat |> who) in
    let damage = Character.get_attack_damage attack |> string_of_int in
    let mana = Character.get_attack_mana attack |> string_of_int in
    (print_string [green] ("Damage: " ^ damage ^ ", Mana: " ^ mana ^ "\n"));
    (print_string [white] ("---------------------------------------" ^
                           "---------------------------------------------\n"));
    print_attacks t st;;

(** [bar char field] is a tuple of strings of spaces representing [field] as a bar. 
    [field] is either "health" or "mana". The first string represents how much 
    of the bar is left (out of 20 spaces), the second string represents how much 
    is lost (out of 20 spaces)corresponding to fraction of remaining health or 
    mana. *)
let bar char field = 
  let num_slots = 
    if field = "health" 
    then int_of_float ((float_of_int (Character.get_hp char))
                       /. (float_of_int (Character.get_original_hp char)) *. 20.) 
    else int_of_float ((float_of_int (Character.get_mp char))
                       /. (float_of_int (Character.get_original_mp char)) *. 20.) in
  (* requires numslots >=0 *)
  let rec slots_to_spaces numslots acc = 
    match numslots with
    | 0 -> acc
    | x -> slots_to_spaces (x-1) (" "^acc) in 
  slots_to_spaces num_slots "", slots_to_spaces (20-num_slots) ""

(** [print_enemy_stats st] prints the current stats of the enemy of [st]. *)
let print_enemy_stats st = 
  let attr_enem = [red;Bold] in
  (print_string [red;Bold;Underlined] "Enemy");
  (print_string attr_enem
     "\nHealth: |");
  let enem_health = bar (st |> State.get_enemy_stat |> who ) "health" in
  (print_string [red;Inverse] (fst enem_health));
  (print_string attr_enem (snd enem_health));
  (print_string attr_enem 
     ("| " ^
      (st |> State.get_enemy_stat |> who |> Character.get_hp 
       |> string_of_int) ^ "/" ^ 
      (st |> State.get_enemy_stat |> who |> Character.get_original_hp 
       |> string_of_int));
   (print_string [white] ("\n-------------------------------------" ^
                          "-----------------------------------------------\n")))
;;
(** [print_play_stats st] prints the current stats of the player of [st]. *)
let print_play_stats bg st = 
  let attr_char= [green;Bold] in
  (print_string [white] ("---------------------------------------" ^
                         "---------------------------------------------\n"));
  if bg then () else ignore(read_line ());
  (print_string [green;Bold;Underlined] "\nPlayer");
  (print_string attr_char "\nHealth: |");
  let char_health = bar (st |> State.get_char_stat |> who ) "health" in
  (print_string [green;Inverse] (fst char_health));
  (print_string attr_char (snd char_health));
  (print_string attr_char 
     ("| " ^
      (st |> State.get_char_stat |> who |> Character.get_hp 
       |> string_of_int) ^ "/" ^
      (st |> State.get_char_stat |> who |> Character.get_original_hp 
       |> string_of_int) ^ 
      "\nMana:   |"));
  let char_mana = bar (st |> State.get_char_stat |> who ) "mana" in
  (print_string [green;Inverse] (fst char_mana));
  (print_string attr_char (snd char_mana));
  (print_string attr_char 
     ("| " ^
      (st |> State.get_char_stat |> who |> Character.get_mp 
       |> string_of_int) ^ "/" ^
      (st |> State.get_char_stat |> who |> Character.get_original_mp 
       |> string_of_int)))

(** [print_stats bg st] prints the following to the console:
    - whose turn it is
    - the player's health, mana, available attacks, and powerups
    - the enemy's health, mana, available attacks, and powerups
*)
let print_stats bg st = let open ANSITerminal in
  print_play_stats bg st;
  print_string [green;Bold]("\nAttacks:\n");
  print_attacks (st |> State.get_char_stat |> who |> Character.get_attack_names) 
    st;
  (print_string [white] ("---------------------------------------" ^
                         "---------------------------------------------"));
  (print_string [green;Bold] ("\nPowerups:\n" ));
  (print_string [white] ("---------------------------------------" ^
                         "---------------------------------------------\n"));
  print_items (st |> State.get_char_stat |> who |> Character.get_item_names) st;

  print_enemy_stats st

(** [change_state char st] runs the game using [char] as the user's character
    from state [st].

    Side effects:
    - prints to the console when the player loses
    - prints to the console when the player wins a round
    - prints to the console when the player wins the final round
    - prints the name of the enemy the player is facing
    - prints the action performed (either attack, defend, or use powerup)
    - prints a goodbye message if the user quits the game. 
*)
let rec change_state (char : Character.t) st = 
  if (is_in_battle st) then 
    change_state_battle char st
  else
    change_state_market char st

(** [change_state_battle char st] is the state [st'] of the game after a move is 
    made from [st]. *)
and change_state_battle char st = 
  (* let () = erase Screen in *)
  let open Action in
  let winner = curr_winner st in 
  match winner with
  | Enemy -> print_endline "Sorry, you lost"; exit 0
  | Player -> 
    begin
      match State.get_round st with
      | 4 -> 
        ANSITerminal.(print_string [black;on_white] 
                        ("Congratulations!" ^ 
                         "You have succesfully led your team to paradise!"));
        print_endline ""; exit 0
      | _ -> 
        let enemy_name = 
          if State.get_round st = 2 then "ogre" else "demon" 
        in print_art enemy_name;
        ANSITerminal.(print_string [cyan;Bold] 
                        ("\nYou have come across your next enemy. 
                        Press any key to battle the " ^ enemy_name ^ ".\n\n"));
        ignore(read_line ());

        print_endline "";
        let item = Character.get_item (State.who (State.get_enemy_stat st)) in
        let new_char = (Character.update 
                          (Character.get_hp char) 
                          (Character.get_mp char) 0 
                          (Character.add_item item 
                             (State.who (State.get_char_stat st)))) in
        let new_enemy = 
          if State.get_round st = 2 
          then Character.from_json (Yojson.Basic.from_file "ogre.json")
          else Character.from_json (Yojson.Basic.from_file "demon.json") in
        change_state new_char 
          (State.init_round new_char new_enemy (State.get_round st))
    end
  | None ->
    let bg = (State.curr_turn st) = Play in
    let mess = if bg 
      then "It is your turn."
      else "It is the enemy's turn. Press anything to continue." in 
    (print_string [white;Bold;Underlined] ("\n" ^ mess ^ "\n"));
    print_stats bg st;
    if bg then print_endline "Enter a move." else ();
    let action = 
      if State.curr_turn st = State.Play 
      then parse_command (read_line ()) 
      else parse_command (Ai.ai_move st) in (* AI used here*)
    match action with 
    | Attack ap ->
      let atk_name = (String.concat " " ap) in
      let atk = State.attack atk_name st in
      begin
        match res_to_st atk with 
        | Succ_State st -> print_endline ("Attacked with " ^ atk_name); 
          if bg then print_enemy_stats st else ();
          change_state char st
        | Err_State err -> print_endline err; change_state char st
      end
    | Defend -> let def = State.defend st in 
      begin 
        match res_to_st def with
        | Succ_State st -> print_endline "Defended."; change_state char st
        | Err_State err -> print_endline err; change_state char st
      end
    | Use up -> 
      let item_name = Action.string_of_ap up in
      let item = State.powerup item_name st in
      begin
        match res_to_st item with
        | Succ_State st -> print_endline ("Used powerup " ^ item_name); 
          change_state char st
        | Err_State err -> print_endline err; change_state char st
      end
    | Quit -> print_endline "Bye friend. The animals will miss you."; exit 0
    | Buy _
    | Cancel -> print_endline "This is not a valid command in battle. Try again.";
      change_state char st

(** [change_state_market char st] is the state [st'] of the game during the 
    market stages following a change in [st]. *)
and change_state_market (char : Character.t) st = 
  let winner = curr_winner st in 
  let () = match winner with 
    | Player -> 
      print_endline "\nYou beat the enemy.";
    | Enemy -> print_endline "Sorry, you lost"; exit 0
    | None -> failwith "should only be in market if there is a winner" in
  let () = if State.get_round st <> 4 
    then 
      (print_endline 
         "You take twenty coins from their bag and pick up the item they dropped.";
       print_market_options st) 
    else change_state_battle char st in
  let open Action in
  let action = parse_command (read_line ()) in 
  match action with 
  | Attack _ 
  | Defend 
  | Use _ -> print_endline "This is not a valid command in market. Try again.";
    change_state char st
  | Buy item -> 
    let vendor = if get_round st = 2 then 
        Vendor.from_vend_json (Yojson.Basic.from_file "vendor1.json") else 
        Vendor.from_vend_json (Yojson.Basic.from_file "vendor2.json") in
    let item_name = Action.string_of_ap item in
    let new_char = buy item_name vendor st in 
    begin
      match res_to_st new_char with
      | Succ_State st -> 
        print_endline ("You bought " ^ item_name); 
        change_state char st
      | Err_State err -> 
        print_endline err; change_state char st
    end 
  | Cancel -> let new_state = State.cancel st in 
    begin
      match res_to_st new_state with
      | Succ_State st ->
        change_state char st
      | Err_State err ->
        print_endline err; change_state char st
    end
  | Quit -> print_endline "Bye friend. The animals will miss you."; exit 0

(** [play_game char_t] starts the first round with [char_t] as the character the
    user will play as. 
    Side effects:
    - prints a message giving the name of the first enemy*)
let play_game char_t char_name= 
  print_art char_name;
  ANSITerminal.(print_string [cyan;Bold] 
                  ("\n\nYou have chosen to play as " ^ 
                   String.lowercase_ascii char_name ^ 
                   ". Now begin your journey to find a new home.\n"));
  ANSITerminal.(print_string [cyan;Bold] ("Press any key to continue.\n"));
  ignore(read_line ());
  (* print_art "map1";
     ignore(read_line ()); *)
  print_art "dragon";
  ANSITerminal.(print_string [black;on_white] 
                  ("\nAs you travel along with your group of animals, " ^ 
                   "you come across a lazy dragon. " ^
                   "You must battle the dragon in order to continue your journey. " ^
                   "Press any key to continue."));
  ignore(read_line ());
  print_endline "";
  let enemy1 = Character.from_json (Yojson.Basic.from_file "dragon.json") in 
  let init = State.init_round char_t enemy1 1 in 
  change_state char_t init

(** [main ()] starts the interface and prompts the user for their choice of 
    character. If the user enters a valid character name, the game proceeds. 

    Side effects:
    - prints the instructions to the console
    - if the user does not enter a valid character name, prints a message 
      prompting them to enter a valid name *)
let main () = 
  print_art "title";
  ANSITerminal.(print_string [cyan;Bold] ("Press enter to play.\n"));
  ignore(read_line ());
  ANSITerminal.(print_string [black;on_white] 
                  ("\nWelcome to MIGRATION.\nThe homeland of your "^ 
                   "animal tribe has been destroyed by a disastrous " ^ 
                   "volcanic eruption.\nYour group will face three mythical " ^
                   "opponents from an enemy tribe who want to sabotage your " ^
                   "journey to a new homeland.\n"));
  ANSITerminal.(print_string [white;Bold] 
                  "\nInstructions:\n");
  ANSITerminal.(print_string [white;Underlined] 
                  "Making moves:\n");
  ANSITerminal.(print_string [white] 
                  ("During each battle, you can attack, defend, or use an "^
                   "item as a powerup.\n-To damage an enemy, type attack and "^
                   "the attack name.\n-To defend, type defend. This will protect"
                   ^" you from your opponent's next attack.\n"^
                   "-To use an item, type use and the item name. "^
                   "Items can affect your health, mana, or the strength of "^
                   "your attack only if you attack the turn after using the "^
                   "powerup.\n-See [Using items] for more information.\n"));
  ANSITerminal.(print_string [white;Underlined] 
                  "The enemies:\n");
  ANSITerminal.(print_string [white] 
                  ("Each enemy will attack or defend depending on your and its"^
                   " current stats.\nThere are three enemies total. They get "^
                   "progressively harder to defeat.\nHowever, once you defeat "^
                   "an enemy, the item they drop gets added to your powerup "^
                   "inventory and you will be able to use it to defeat "^
                   "consequent enemies.\nThe enemy will also drop coins, which "^
                   "you can use to buy items.\n"));
  ANSITerminal.(print_string [white;Underlined] 
                  "Using items:\n");
  ANSITerminal.(print_string [white] 
                  ("Once you use an item, it will be removed from your powerups"
                   ^" and cannot be used again.\nEach item has a corresponding "^
                   "multiplier that affects the indicated category (i.e. "^
                   "health).\nItems that boost your subsequent attack are po" ^
                   "werless if the enemy defended and cannot be reused.\n\n"));
  ANSITerminal.(print_string [cyan;Bold] 
                  "Let's start the game:\n");
  ANSITerminal.(print_string [cyan]
                  ("Now its time to save your animal tribe! Pick the leader "^
                   "who will fight each battle.
                  \n[ elephant | koala | penguin ]\n\n"));
  match read_line () with 
  | exception End_of_file -> ()
  | char_name -> 
    match valid_char_json char_name with
    | Succ_Char char_t -> play_game char_t char_name
    | Err_Char err_mess -> print_endline err_mess

let () = main ()