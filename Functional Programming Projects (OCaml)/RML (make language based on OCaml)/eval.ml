open Dwt.Infix
open Types

exception ExpectedUnit
exception ExpectedBool
exception ExpectedInt
exception ExpectedFunction
exception ExpectedHandle
exception ExpectedPromise
exception ExpectedRef
exception ExpectedList
exception ExpectedString
exception UnboundVariable
exception InexhaustivePatterns
exception IncompatibleTypes
exception ArgumentMismatch
exception LetMismatch
exception AwaitMismatch

(** [is_value e] is true if [e] is a value and false otherwise. *)
let rec is_value exp : bool = match exp with 
  | Unit 
  | Bool _
  | Int _
  | Str _
  | Nil
  | Fun _ -> true
  | Pair (e1, e2) 
  | Cons (e1, e2) -> is_value e1 && is_value e2
  | _ -> false

(** [add_to_assoc key value alist] is [alist] with the pair ([key], [value]) 
    added if [key] is not in [alist] or [alist] with the existing value of [key]
    replaced with [value] if [key] is already in [alist]. 
    Requires: [alist] is an association list. *)
let add_to_assoc key value alist = 
  let no_key =
    if List.mem_assoc key alist then List.remove_assoc key alist else alist in 
  (key, value)::no_key

(** [add_lst_to_assoc new_lst alist] is [alist] with the key-value pairs of 
    [new_lst] added. If any key of the pairs of [new_lst] already exists in 
    [alist], that value is replaced with the corresponding value from [new_lst]. 
    Requires: [alist] and [new_lst] are association lists. *)
let add_lst_to_assoc new_lst alist = 
  let rec helper lst acc = 
    match lst with 
    | [] -> acc
    | (k,v)::t -> 
      if List.mem_assoc k acc 
      then let removed = (List.remove_assoc k acc) in 
        let replaced = (k,v)::removed in 
        helper t replaced 
      else helper t ((k,v)::acc) in 
  helper new_lst alist

(** [make_prom_list valu acc] is a list of all the values in [nil_cons] 
    represented as promises. *)
let rec make_prom_list nil_cons acc = 
  begin 
    match nil_cons with
    | VNil -> acc
    | VCons (h, t) -> 
      let prom = 
        begin
          match h with 
          | VDwt x -> x
          | _ -> raise ExpectedPromise
        end in
      make_prom_list t (List.rev (prom :: acc))
    | _ -> raise ExpectedList
  end

(* [eval env exp] is [v], where [v] is the result of evaluating [exp] under the
 * environment [env]
 * side effects: produces the applicable i/o side effects *)
let rec eval (env : env) (exp : exp) : value = match exp with 
  | Unit -> VUnit
  | Bool b -> VBool b
  | Pair (e1, e2) -> VPair (eval env e1, eval env e2)
  | Int i -> VInt i
  | Str s -> VStr s 
  | Var x -> eval_var env x
  | Fun (p, e) -> VFun (p, e, env)
  | App (e1, e2) -> eval_app env e1 e2
  | Let (x, e1, e2) -> eval_let env x e1 e2
  | Nil -> VNil
  | Cons (e1, e2) -> VCons (eval env e1,(assert_list (eval env e2)))
  | Bin (f, e1, e2)-> eval_bin env f e1 e2
  | Una (f, e) ->
    begin
      match eval env e, f with
      | VInt v, Neg -> VInt (-v)
      | VBool v, Not -> VBool (not v)
      |_ -> raise ArgumentMismatch
    end
  | Seq (e1, e2) -> 
    begin
      match eval env e1 with 
      | VUnit -> eval env e2
      | _ -> raise ExpectedUnit
    end
  | IfThen (e1, e2, e3) -> 
    begin
      match eval env e1 with
      | VBool b -> 
        if b then eval env e2 else eval env e3
      | _ -> raise ExpectedBool
    end 
  | Match (e1, mlst) -> eval_match env e1 mlst
  | Ref e -> eval_ref env e
  | Deref e -> eval_deref env e
  | Assign (e1, e2) -> eval_assign env e1 e2
  | LetRec (f, e1, e2) -> eval_let_rec env f e1 e2
  | Await (p, e1, e2) -> eval_await env p e1 e2
  | Spawn (e1, e2) -> eval_spawn env e1 e2
  | Send (e1, e2) -> eval_send env e1 e2
  | Recv e -> eval_recv env e
  | Join e -> eval_join env e
  | Pick e -> eval_pick env e
  | Return e -> VDwt (Dwt.return (eval env e))

and eval_var env x : value = 
  try List.assoc x env 
  with Not_found -> raise UnboundVariable

and eval_bin env f e1 e2 : value = 
  let val_e1 = eval env e1 in
  let val_e2 = eval env e2 in 
  match val_e1,val_e2 with
  | VInt v1, VInt v2 ->
    begin
      match f with 
      |Add -> VInt (v1 + v2)
      |Sub -> VInt (v1 - v2)
      |Mul -> VInt (v1 * v2)
      |Div -> VInt (v1 / v2)
      |Mod -> VInt (v1 mod v2)
      |Lt -> VBool (v1 < v2)
      |Le -> VBool (v1 <= v2)
      |Gt -> VBool (v1 > v2)
      |Ge -> VBool (v1 >= v2)
      |Eq -> VBool (v1 = v2)
      |Ne -> VBool (v1 <> v2)
      |_ -> raise ArgumentMismatch
    end
  | VBool v1, VBool v2 -> 
    begin
      match f with
      |And -> VBool (v1 && v2)
      |Or -> VBool (v1 || v2)
      |Eq -> VBool (v1 = v2)
      |Ne -> VBool (v1 <> v2)
      |_ -> raise ArgumentMismatch
    end
  | VStr v1, VStr v2 -> 
    begin
      match f with 
      |Cat -> VStr (v1^v2)
      |Eq -> VBool (v1 = v2)
      |Ne -> VBool (v1 <> v2)
      | _ -> raise ArgumentMismatch
    end
  |_ -> raise ArgumentMismatch

and eval_app env e1 e2 : value = 
  let v2 = eval env e2 in 
  match e1,v2 with 
  | Var "print", y -> let s = Pretty.print y in 
    print_string s; VUnit
  | Var "println", y -> let s = Pretty.print y in 
    print_endline s; VUnit
  | Var "string_of_int", y ->
    begin
      match y with 
      | VInt i -> VStr (string_of_int i) 
      | _ -> raise ExpectedInt
    end
  | Var "sleep", y -> 
    begin
      match y with 
      | VInt i -> 
        let unit_to_vunit = fun () -> Dwt.return (VUnit) in 
        (VDwt ((Dwt.sleep i)>>=unit_to_vunit))
      | _ -> raise ExpectedInt
    end
  | Var "random", y ->
    begin
      match y with 
      | VInt n ->  if n < 0 then raise (Invalid_argument "Must be positive") else
          (VInt (Random.int n))
      | _ -> raise ExpectedInt
    end
  | Var "ignore", y -> VUnit
  | _ ->
    let e1clos = eval env e1 in 
    begin
      match e1clos with
      | VFun (p_cl, e_cl, env_cl) ->
        let rec fun_helper env p v = 
          begin
            match p,v with
            | PVar x, _ -> add_to_assoc x v2 env_cl
            | PPair (x, y), VPair (z, a) 
            | PCons (x, y), VCons (z, a) -> 
              add_lst_to_assoc (fun_helper env y a) (fun_helper env x z)
            | PInt x, VInt y when x = y -> env_cl
            | PStr x, VStr y when x = y -> env_cl
            | PBool x, VBool y when x = y -> env_cl
            | PUnit, VUnit -> env_cl
            | PNil, VNil -> env_cl
            | PWild, _ -> env_cl
            | _ -> raise ArgumentMismatch 
          end
        in 
        let new_env = fun_helper env p_cl v2 in 
        eval new_env e_cl
      | VFunRec (p_cl, e_cl, env_cl) ->       
        let rec fun_rec_helper env p v = 
          begin
            match p,v with
            | PVar x, _ -> 
              add_to_assoc x v2 !env_cl
            | PPair (x, y), VPair (z, a) 
            | PCons (x, y), VCons (z, a) -> 
              add_lst_to_assoc (fun_rec_helper env y a) (fun_rec_helper env x z)
            | PInt x, VInt y when x = y -> !env_cl
            | PStr x, VStr y when x = y -> !env_cl
            | PBool x, VBool y when x = y -> !env_cl
            | PUnit, VUnit -> !env_cl
            | PNil, VNil -> !env_cl
            | PWild, _ -> !env_cl
            | _ -> raise ArgumentMismatch 
          end in
        let new_env = fun_rec_helper env p_cl v2 in 
        eval new_env e_cl
      | _ -> raise ExpectedFunction
    end

and eval_let env x e1 e2 = 
  let v1 = eval env e1 in 
  let rec eval_let_helper env e v = 
    match e,v with 
    | PVar x, _ -> add_to_assoc x v1 env
    | PPair (x, y), VPair (z, a)
    | PCons (x, y), VCons (z, a) -> 
      add_lst_to_assoc (eval_let_helper env y a) (eval_let_helper env x z)
    | PInt x, VInt y when x = y -> env
    | PStr x, VStr y when x = y -> env
    | PBool x, VBool y when x = y -> env
    | PUnit, VUnit -> env
    | PNil, VNil -> env
    | PWild, _ -> env
    | _ -> raise LetMismatch in
  let new_env = eval_let_helper env x v1 in 
  eval new_env e2

and eval_match env e1 mlst = 
  let v = eval env e1 in 
  match mlst with 
  | [] -> raise InexhaustivePatterns
  | h::t -> let env_opt = bind_pat (fst h) v in
    begin
      match env_opt with 
      | None -> eval_match env e1 t
      | Some x -> 
        let new_env = add_lst_to_assoc x env in
        eval new_env (snd h)
    end

and eval_ref env e = 
  let v = eval env e in
  VRef (ref v)

and eval_deref env e = 
  let v = eval env e in 
  match v with
  | VRef r -> !r
  | _ -> raise ExpectedRef

and eval_assign env e1 e2 = 
  let v1 = eval env e1 in 
  let v2 = eval env e2 in 
  match v1 with
  | VRef r -> r := v2; VUnit
  | _ -> raise ExpectedRef

(** [bind_pat p v] is [None] where [v] does not match [p], and [Some b]
 * where [v] matches [p] producing new bindings [b] *)
and bind_pat (p : pat) (v : value) : env option =

  let op_help (b_lst:env option) = 
    match b_lst with
    | Some b -> b
    | None -> [] in

  let rec bind_pat_help p v env_op = 
    let rec pair_helper p1 p2 = 
      match p1,p2 with 
      |None,None -> None
      |Some a, None -> None
      |None,Some b -> None
      |Some a, Some b -> 
        Some (add_lst_to_assoc (op_help p2)(op_help p1)) in
    match p,v with
    | PVar x, _ -> Some (add_to_assoc x v (op_help env_op))
    | PPair (x, y), VPair (z, a) -> 
      let p1 = bind_pat_help x z env_op in 
      let p2 = bind_pat_help y a env_op in pair_helper p1 p2 
    | PCons (x, y), VCons (z, a) ->   
      let c1 = bind_pat_help x z env_op in 
      let c2 = bind_pat_help y a env_op in 
      pair_helper c1 c2
    | PInt x, VInt y when x = y -> Some []
    | PStr x, VStr y when x = y -> Some []
    | PBool x, VBool y when x = y -> Some []
    | PUnit, VUnit -> Some []
    | PNil, VNil -> Some []
    | PWild, _ -> Some []
    | _ -> None 

  in 
  bind_pat_help p v (None)

and eval_let_rec env f e1 e2 = 
  let v1 = eval env e1 in 
  let e1_rec = 
    match v1 with 
    | VFun (p_cl, e_cl, env_cl) -> VFunRec (p_cl,e_cl, ref env_cl)
    | _ -> raise ExpectedFunction in
    let () =
    match e1_rec with 
    | VFunRec (p_cl, e_cl, env_cl_ref) -> 
      env_cl_ref := add_to_assoc f e1_rec !env_cl_ref 
    | _ -> raise ExpectedFunction in 
  eval (add_to_assoc f e1_rec env) e2

and eval_await env p e1 e2 = 
  let prom1 = eval env e1 |> assert_dwt in
  let prom2 = Dwt.bind prom1 (fun v1 -> let new_env = 
    match bind_pat p v1 with
    | None -> raise AwaitMismatch
    | Some b -> add_lst_to_assoc b env in (eval new_env e2) |> assert_dwt) in 
  VDwt(prom2)

and eval_join env e = 
  let prom_list = make_prom_list (eval env e) [] in 
  let list_prom = Dwt.join prom_list in 
  let list_to_vlist_prom = (fun list -> Dwt.return (vlist_of_list list)) in 
  (* convert OCaml list promise to VList promise*)
  VDwt (list_prom >>= list_to_vlist_prom) 

and eval_pick env e = 
  let p_list = make_prom_list (eval env e) [] in 
  let picked = Dwt.pick p_list in 
  VDwt (picked >>= Dwt.return)

and eval_send env e1 e2 = 
  let message_v = (eval env e1) |> Serialize.string_of_value in 
  let hand = (eval env e2) |> assert_handle in 
  Dwt.send message_v hand; VUnit

and eval_recv env e = 
  let hand = (eval env e) |> assert_handle in 
  let prom = Dwt.recv hand in 
  let s_prom_to_val_prom = fun s -> Dwt.return (Serialize.value_of_string s) in
  VDwt (prom >>= s_prom_to_val_prom)

and eval_spawn env e1 e2 = 
  let f = (eval env e1) in 
  let v = eval env e2 in
  let f_s = f |> Serialize.string_of_value in 
  let v_s = v |> Serialize.string_of_value in 
  let hand = Dwt.spawn f_s v_s in 
  VHandle hand

(* You may use the following utility functions in your implementation.
 * Example usage: [eval env exp |> assert_unit] *)
and assert_unit = function
  | VUnit -> ()
  | v -> raise ExpectedUnit

and assert_bool = function
  | VBool b -> b
  | _ -> raise ExpectedBool

and assert_int = function
  | VInt i -> i
  | _ -> raise ExpectedInt

and assert_fun = function
  | VFun (f, b, env) -> (f, b, env)
  | VFunRec (f, b, env) -> (f,b,!env)
  | _ -> raise ExpectedFunction

and assert_handle = function
  | VHandle h -> h
  | _ -> raise ExpectedHandle

and assert_dwt = function
  | VDwt dwt -> dwt
  | _ -> raise ExpectedPromise

and assert_ref = function
  | VRef ref -> ref
  | _ -> raise ExpectedRef

and assert_list vs =
  match vs with
  | VNil | VCons _ -> vs
  | _ -> raise ExpectedList

and assert_string = function
  | VStr s -> s
  | _ -> raise ExpectedString

(* Converts a list into a VList. *)
and vlist_of_list l =
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (VCons(h,acc)) t in
  loop VNil (List.rev l)

