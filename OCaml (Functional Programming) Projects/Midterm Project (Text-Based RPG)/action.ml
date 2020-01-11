type action_phrase = string list 

type action = 
  |Attack of action_phrase
  |Defend 
  |Use of action_phrase
  |Quit
  |Buy of action_phrase
  |Cancel

exception Empty
exception Malformed 

(** [trimmer str] is the list of words parsed from the string the player inputs 
    with the spaces removed. 

    Required: [str] contains only alphanumeric and space characters. *)
let trimmer str = 
  let trimmed = String.split_on_char ' ' (String.lowercase_ascii str) in 
  let rec rm_space acc = function 
    |[] -> List.rev acc 
    |h::t -> if h <> "" then rm_space (h::acc) t
      else rm_space acc t in 
  rm_space [] trimmed

(** [parse_phrase phrase] parses the player's input into an action verb and 
    a list of words in the action_phrase. 

    Raises: [Empty] if [phrase] is the empty string or contains only spaces.

    Raises: [Malformed] if the action is malformed. An action
    is malformed if the verb is "defend", "continue," or "quit" and there is a
    non-empty object phrase, or if the verb is "attack" or "use" and there is 
    an empty object phrase. *)
let parse_phrase phrase = 
  let action = trimmer phrase in
  if action = [] then raise Empty else
    let verb = List.hd action in 
    let cmd = List.tl action in 
    if verb = "defend" && cmd = [] then Defend else
    if verb = "quit" && cmd = [] then Quit else 
    if verb = "attack" && cmd <> [] then Attack cmd else 
    if verb = "use" && cmd <> [] then Use cmd else 
    if verb = "buy" && cmd <> [] then Buy cmd else
    if verb = "cancel" && cmd = [] then Cancel else
      raise Malformed 

let string_of_ap ap =
  String.concat " " ap