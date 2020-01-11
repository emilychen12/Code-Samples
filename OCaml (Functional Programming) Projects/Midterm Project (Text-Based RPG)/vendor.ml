open Yojson.Basic.Util

type t = 
  {
    items: Character.item list
  }

let find_vend_item item_name vendor = 
  let rec find item_name (item_list: Character.item list) = 
    match item_list with
    | [] -> raise (Character.UnknownItem item_name)
    | h::t -> 
      if Character.get_item_name h = item_name then h else find item_name t in
  find item_name vendor.items 

let from_vend_json json = 
  let j = to_assoc json in 
  let items_from_json j = List.assoc "items" j |> to_list in 
  let rec get_items acc lst = 
    match lst with 
    |[] -> acc 
    |h::t -> 
      let n = h |> to_assoc |> List.assoc "item name" |> to_string in
      let e =  h |> to_assoc |> List.assoc "effect" |> to_int in
      let ty = h |> to_assoc |> List.assoc "item type" |> to_string in 
      let c = h |> to_assoc |> List.assoc "cost" |> to_int in
      get_items ((Character.create_item n e ty c)::acc) t in 
  {
    items = get_items [] (items_from_json j);
  }

let append lst obj = 
  if List.mem obj lst then lst else obj::lst (** this is repeated code from character.ml*)

let get_vend_item_names vendor = 
  List.fold_left (fun lst tp -> append lst (Character.get_item_name tp)) [] vendor.items

let find_vend_item item_name vendor = 
  let rec find item_name (item_list: Character.item list) = 
    match item_list with
    | [] -> raise (Character.UnknownItem item_name)
    | h::t -> 
      if Character.get_item_name h = item_name then h else find item_name t in
  find item_name vendor.items 