open Yojson.Basic.Util

type atk_name = string
type item_name = string

type attack = {
  name: atk_name; 
  damage: int; 
  mana: int
}

type item = {
  name: item_name;
  effect: int;
  tp: string;
  cost: int
}

exception UnknownAttack of atk_name
exception UnknownItem of item_name

type t = {
  original_hp: int;
  original_mp: int;
  health: int;
  mana: int;
  attacks: attack list;
  powerups: item list;
  coins: int
} 

let from_json json = 
  let j = to_assoc json in 
  let attacks_from_json j = List.assoc "attacks" j |> to_list in
  let rec get_attacks acc lst = 
    match lst with 
    |[] -> acc 
    |h::t -> 
      let n = h |> to_assoc |> List.assoc "attack name" |> to_string in
      let d =  h |> to_assoc |> List.assoc "damage" |> to_int in
      let m = h |> to_assoc |> List.assoc "mp" |> to_int in 
      get_attacks ({ name =  n; damage = d; mana = m }::acc) t in
  let items_from_json j = List.assoc "powerups" j |> to_list in 
  let rec get_items acc lst = 
    match lst with 
    |[] -> acc 
    |h::t -> 
      let n = h |> to_assoc |> List.assoc "item name" |> to_string in
      let e =  h |> to_assoc |> List.assoc "effect" |> to_int in
      let ty = h |> to_assoc |> List.assoc "item type" |> to_string in 
      let c = h |> to_assoc |> List.assoc "cost" |> to_int in
      get_items ({ name =  n; effect = e; tp = ty; cost = c}::acc) t in 
  {
    original_hp = List.assoc "health points" j |> to_int;
    health = List.assoc "health points" j |> to_int;
    original_mp = List.assoc "mana points" j |> to_int;
    mana = List.assoc "mana points" j |> to_int;
    attacks = get_attacks [] (attacks_from_json j);
    powerups = get_items [] (items_from_json j);
    coins = 0;
  }

let get_original_hp char = 
  char.original_hp

let get_original_mp char = 
  char.original_mp

let get_hp char =
  char.health

let get_mp char = 
  char.mana 

let get_coins char = 
  char.coins

let find_attack atk_name char = 
  let rec find atk_name (atk_list : attack list) = 
    match atk_list with
    | [] -> raise (UnknownAttack atk_name)
    | h::t -> 
      if h.name = atk_name then h else find atk_name t in
  find atk_name char.attacks

let find_item item_name char = 
  let rec find item_name (item_list: item list) = 
    match item_list with
    | [] -> raise (UnknownItem item_name)
    | h::t -> 
      if h.name = item_name then h else find item_name t in
  find item_name char.powerups

let get_attack_name (atk:attack) = atk.name

let get_attack_mana (atk: attack) = atk.mana

let get_attack_damage (atk: attack) = atk.damage

let get_item_name (item: item) = item.name

let get_item_type (item: item) = item.tp

let get_item_effect (item: item) = item.effect

let get_item_cost (item: item) = item.cost

let update new_hp new_mp add_coins char = 
  {char with health = new_hp; mana = new_mp; coins = char.coins + add_coins}

let buy_update cost item char = 
  {char with powerups = item::char.powerups; coins = char.coins - cost}

let delete_item item char = 
  let rec delete init item_list item = 
    match item_list with 
    |[] -> init
    |h::t -> 
      if h.name <> item.name then delete (h::init) t item else delete init t item in 
  {char with powerups = delete [] char.powerups item}

let add_item item char = 
  {char with powerups = item::char.powerups}

let get_item char =
  match char.powerups with
  | [] -> failwith "no items found"
  | h::t -> h

(** [append lst obj] is a list containing the names of the elements of [obj]. *)
let append lst obj = 
  if List.mem obj lst then lst else obj::lst

let get_attack_names char = 
  List.fold_left (fun lst (tp : attack)-> append lst tp.name) [] char.attacks

let get_item_names char = 
  List.fold_left (fun lst (tp : item) -> append lst tp.name) [] char.powerups

let get_attacks char = 
  char.attacks

let create_item name effect tp cost = 
  {name = name; effect = effect; tp = tp; cost = cost}