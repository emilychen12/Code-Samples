
type winner = None | Player | Enemy
type player = Play | Enem

type t = {
  player_turn: player;
  win: winner;
  char_stat: Character.t*bool*int;
  enemy_stat: Character.t*bool*int;
  round: int;
  in_battle: bool
}

let get_char_stat st = 
  st.char_stat

let get_enemy_stat st = 
  st.enemy_stat

let get_round st = 
  st.round

let is_in_battle st = 
  st.in_battle

let init_round char enemy round = 
  let open Character in
  {
    player_turn = Play;
    win = None;
    char_stat = char,false,1;
    enemy_stat = enemy,false,1;
    round = round;
    in_battle = true
  }

let curr_winner st = 
  st.win

let curr_turn st = 
  st.player_turn

type result = Legal of t | Illegal of string

let who (first,_,_) = first

(** [defended stat] is the boolean representing whether the player represented 
    by stat defended on their last turn. *)
let defended (_,second,_) = second

(** [multiplier stat] is the amount that the next move performed by the player 
    represented by stat will be multiplied by. *)
let multiplier (_,_,third) = third

let attack atk st = 
  let current,opponent,current_name = if st.player_turn = Play
    then st.char_stat,st.enemy_stat,Play
    else st.enemy_stat,st.char_stat,Enem in
  try
    let curr_atk = Character.find_attack atk (who current) in 
    let atk_mana = Character.get_attack_mana curr_atk in
    if atk_mana <= Character.get_mp (who current) 
    then 
      let damage = 
        if defended opponent then 0 else Character.get_attack_damage curr_atk in
      let has_winner = 

        Character.get_hp (who opponent) <= ((multiplier current) * damage) in

      match current_name with
      | Play ->               
        Legal {char_stat = 
                 Character.update 
                   (Character.get_hp (who current)) 
                   (Character.get_mp (who current) - atk_mana) 
                   (if has_winner then 20 else 0)
                   (who current),
                 (defended current),1; 
               enemy_stat = Character.update 
                   (Character.get_hp (who opponent) 
                    - (multiplier current) * damage) 
                   (Character.get_mp (who opponent)) 0
                   (who opponent),
                            false,1;
               win = if has_winner then Player else None;
               player_turn = if st.player_turn = Play then Enem else Play;
               round = if has_winner then st.round+1 else st.round; 
               in_battle = not has_winner}
      | Enem -> 
        Legal {st with 
               enemy_stat = 
                 Character.update 
                   (Character.get_hp (who current)) 
                   (Character.get_mp (who current) - atk_mana) 0
                   (who current),
                 (defended current),1; 
               char_stat = Character.update 
                   (Character.get_hp (who opponent) - damage) 
                   (Character.get_mp (who opponent)) 0
                   (who opponent),
                           false,multiplier opponent;
               win = if has_winner then Enemy else None;
               player_turn = if st.player_turn = Play then Enem else Play; 
               in_battle = not has_winner}
    else Illegal "Insufficient mana"
  with
  | (Character.UnknownAttack _) -> Illegal "Attack not found"

let defend st = 
  let current,current_name = 
    if st.player_turn = Play then st.char_stat,Play else st.enemy_stat,Enem in
  match current_name with
  | Play -> Legal {st with 
                   char_stat = who current, true, multiplier current; 
                   player_turn = Enem}
  | Enem -> Legal {st with 
                   enemy_stat = who current, true, multiplier current; 
                   player_turn = Play}

let powerup item st = 
  try
    let curr_item = Character.find_item item (who st.char_stat) in
    let item_type = Character.get_item_type curr_item in
    match item_type with 
    | "hp" -> 
      Legal {st with char_stat = 
                       Character.update 
                         (Character.get_item_effect curr_item 
                          + Character.get_hp (who st.char_stat)) 
                         (Character.get_mp (who st.char_stat)) 0
                         (Character.delete_item curr_item (who st.char_stat)),
                       false,1; 
                     player_turn = if st.player_turn = Play then Enem else Play}
    | "mp" -> 
      Legal {st with char_stat = 
                       Character.update 
                         (Character.get_hp (who st.char_stat))
                         (Character.get_item_effect curr_item 
                          + Character.get_mp (who st.char_stat)) 0
                         (Character.delete_item curr_item (who st.char_stat)),
                       false,1; 
                     player_turn = if st.player_turn = Play then Enem else Play}
    | "atk" -> 
      Legal {st with char_stat = 
                       Character.delete_item curr_item (who st.char_stat),
                       false,Character.get_item_effect curr_item; 
                     player_turn = if st.player_turn = Play then Enem else Play}
    | _ -> Illegal "Illegal item"
  with
  | Character.UnknownItem _ -> Illegal "Item not found"

let buy item vendor st = 
  try 
    let item_from_name = Vendor.find_vend_item item vendor in
    let new_char = 
      Character.buy_update (Character.get_item_cost item_from_name) 
        item_from_name (who st.char_stat) in
    if (Character.get_item_cost item_from_name) <= 
      ((who st.char_stat) |> Character.get_coins) then
        Legal {st with 
              char_stat = 
                new_char, defended st.char_stat, multiplier st.char_stat;
              in_battle = true
              }
        else Illegal "You do not have enough coins to buy this item."
  with 
  | Character.UnknownItem _ -> Illegal "Item not found"

let cancel st = 
  Legal {st with in_battle = true; win = Player}