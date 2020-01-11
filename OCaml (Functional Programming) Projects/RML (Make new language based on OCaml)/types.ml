type var = string
[@@deriving yojson]

type handle = int
[@@deriving yojson]

type exp =
  | Unit
  | Bool of bool
  | Pair of exp * exp
  | Int of int
  | Str of string
  | Var of var
  | Fun of pat * exp
  | App of exp * exp
  | Let of pat * exp * exp
  | LetRec of var * exp * exp
  | Nil
  | Cons of exp * exp
  | Assign of exp * exp
  | Ref of exp
  | Deref of exp
  | Bin of bin * exp * exp
  | Una of una * exp
  | Seq of exp * exp
  | IfThen of exp * exp * exp
  | Match of exp * (pat * exp) list
  | Await of pat * exp * exp
  | Spawn of exp * exp
  | Send of exp * exp
  | Recv of exp
  | Join of exp
  | Pick of exp
  | Return of exp
[@@deriving yojson]

and pat =
    | PUnit
  | PWild
  | PBool of bool
  | PInt of int
  | PStr of string
  | PVar of var
  | PPair of pat * pat
  | PNil
  | PCons of pat * pat
[@@deriving yojson]

and bin =
    | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat
[@@deriving yojson]

and una =
    | Neg
  | Not
[@@deriving yojson]

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VStr of string
  | VFun of pat * exp * env
  | VFunRec of pat * exp * env ref
  | VDwt of value Dwt.t
  | VRef of value ref
  | VPair of value * value
  | VNil
  | VCons of value * value
  | VHandle of handle

and env = (var * value) list
