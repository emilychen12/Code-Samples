open State
open Character

(** The type representing moves the AI is capable of. *)
type move = Legal_Atk of attack | Illegal_Atk | Defend

(** [get_strongest_move atk_list] is the move in [atk_list] that does the 
    highest amount of damage. If two or more moves do the same amount of 
    damage, then an arbitrary one will be returned. The result is an 
    Illegal_Atk if [atk_list] is empty. 
*)
let get_strongest_move atk_list = 
  try 
    let str_move = List.hd atk_list in
    let rec helper_strongest atk_list str_move =
      match atk_list with 
      |[] -> Legal_Atk str_move
      |h::t -> if get_attack_damage h > get_attack_damage str_move
        then helper_strongest t h 
        else helper_strongest t str_move in
    helper_strongest atk_list str_move
  with 
  | _ -> Illegal_Atk

(** [easy_ai_move enem poss_atks] is the command representing the move the easy
    AI [enem] does when its possible attacks consist of the list [poss_atks].
*)
let easy_ai_move enem poss_atks = 
  let name = 
    match (get_strongest_move poss_atks) with
    | Legal_Atk atk -> get_attack_name atk
    | Defend -> failwith "Not an attack"
    | Illegal_Atk -> failwith "AI cannot attack" in
  let str_move_name = "attack "^ name
  in
  if get_hp enem > (get_original_hp enem)/4 then str_move_name
  else
    let rand_int = Random.int 2 in 
    if rand_int = 0 then str_move_name else "defend"

(** [rank move_lst factor] is a list with the same length as [move_lst] where
    each move is the integer ranking (in comparison to the other moves in 
    [move_lst]) of the corresponding move in [move_lst] using the first element
    of [factor] as the ranking criterion. If the second element of [factor] is 
    "high", the 1 ranking is given to the move with the lowest value of the 
    criterion. If it is "low", the 1 ranking is given to the move with the 
    highest value of the criterion. 
*)
let rank move_lst factor = 
  let get_factors_lst move_lst fact_tp = 
    let func = if fact_tp = "damage" then get_attack_damage else get_attack_mana in
    let rec helper_factors move_lst acc =
      match move_lst with
      | [] -> acc
      | h::t -> begin
          match h with
          | Legal_Atk atk -> helper_factors t (((Legal_Atk atk), func atk)::acc)
          | Defend -> helper_factors t ((Defend, 0)::acc)
          | Illegal_Atk -> failwith "AI cannot attack"
        end in
    helper_factors move_lst [] in 
  let factor_lst = get_factors_lst move_lst (snd factor) in 
  let sorted_lst = List.sort (fun x y -> 
      if snd x > snd y then 1 else 
      if snd x = snd y then 0 else -1) factor_lst in
  let len = List.length sorted_lst in
  let rec give_ranking fact_lst acc curr_length =
    match fact_lst with 
    |[] -> acc  
    |(name,_)::t -> give_ranking t ((name,curr_length)::acc) (curr_length-1) in 
  let lst = if fst factor = "low" then sorted_lst else List.rev sorted_lst in
  give_ranking lst [] len

(** [adv_ai_move st enem poss_atks] is the command representing the move chosen 
    for the advanced AI [enem] when its possible attacks are [poss_atks] and the 
    game state is [st].
*)
let adv_ai_move st enem poss_atks = 
  let char_hp = st |> get_char_stat |> who |> get_hp |> float_of_int in
  let enem_hp = st |> get_enemy_stat |> who |> get_hp |> float_of_int in 
  let enem_mp = st |> get_enemy_stat |> who |> get_mp |> float_of_int in
  let og_char_hp = st |> get_char_stat |> who |> get_original_hp |> float_of_int in
  let og_enem_hp =  st |> get_enemy_stat |> who |> get_original_hp |> float_of_int in 
  let og_enem_mp =  st |> get_char_stat |> who |> get_original_mp |> float_of_int in 
  let rec get_move_lst poss_atks acc = 
    match poss_atks with
    | [] -> Defend::acc
    | h::t -> get_move_lst t ((Legal_Atk h)::acc) in
  let move_lst = get_move_lst poss_atks [] in

  let lmb = rank move_lst ("low","mana") in 
  let hmb = rank move_lst ("high","mana") in 
  let lhb = rank move_lst ("high","damage") in
  let hhb = rank move_lst ("low","mana") in
  let chh = rank move_lst ("high","damage") in 
  let clh = rank move_lst ("high","damage") in

  let all_scores_lst = lmb::hmb::lhb::hhb::chh::clh::[] in

  let weights = [
    (1. -. enem_mp /. og_enem_mp);
    (enem_mp /. og_enem_mp);
    (1. -. enem_hp /. og_enem_hp);
    (enem_hp /. og_enem_hp);
    (char_hp /. og_char_hp);
    (1. -. char_hp /. og_char_hp)
  ] in

  let sum_score_atk name all_scores weights acc = 
    let rec helper_sum all_scores weights acc = 
      match all_scores,weights with
      | [], [] -> acc
      | hs::ts, hw::tw -> 
        helper_sum ts tw (hw*.(float_of_int (List.assoc name hs))+.acc) 
      | _ -> failwith "Invalid weights and/or scores do not match" in
    helper_sum all_scores weights 0. in

  let sum_score_atks move_lst all_scores acc = 
    let rec helper_scores move_lst acc = 
      match move_lst with 
      |[] -> acc
      |h::t -> helper_scores t 
                 ((h,sum_score_atk h all_scores weights 0.)::acc) in 
    helper_scores move_lst [] in 
  let score_lst = sum_score_atks move_lst all_scores_lst [] in 
  let max_score score_lst = 
    try
      let temp_max = List.hd score_lst in
      let rec helper_max score_lst tmp = 
        match score_lst with
        |[] -> tmp 
        |h::t -> if snd h > snd tmp then helper_max t h else 
          if snd h = snd tmp 
          then let rand_int = Random.int 2 in 
            if rand_int = 0 then helper_max t h else helper_max t tmp 
          else
            helper_max t tmp in
      (helper_max score_lst temp_max) |> fst
    with
    | _ -> Illegal_Atk in
  match max_score score_lst with 
  | Legal_Atk atk -> "attack " ^ (get_attack_name atk)
  | Defend -> "defend" 
  | Illegal_Atk -> failwith "AI is not working"

let ai_move st : string = 
  let enem = st |> get_enemy_stat |> who in
  let curr_mp =  enem |> get_mp in 
  let curr_round = get_round st in
  let rec get_possible_attacks atk_lst acc = 
    match atk_lst with
    | [] -> acc
    | h::t -> if get_attack_mana h <= curr_mp 
      then get_possible_attacks t (h::acc) 
      else get_possible_attacks t acc in 
  let possible_attacks = get_possible_attacks (get_attacks enem) [] in
  if curr_round = 1 then easy_ai_move enem possible_attacks
  else adv_ai_move st enem possible_attacks