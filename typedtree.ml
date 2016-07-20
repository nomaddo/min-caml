open Sexplib.Std

type texp = {texp_desc: texp_desc; ttype: Type.t}

and texp_desc =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of texp
  | Neg of texp
  | Add of texp * texp
  | Sub of texp * texp
  | FNeg of texp
  | FAdd of texp * texp
  | FSub of texp * texp
  | FMul of texp * texp
  | FDiv of texp * texp
  | Eq of texp * texp
  | LE of texp * texp
  | If of texp * texp * texp
  | Let of Id.t * texp * texp
  | Var of Id.t
  | LetRec of tfundef * texp
  | App of texp * texp list
  | Tuple of texp list
  | LetTuple of Id.t list * texp * texp
  | Array of texp * texp
  | Get of texp * texp
  | Put of texp * texp * texp

and tfundef =
  { fname : Id.t; targs : Id.t list; fbody : texp; ftype: Type.t}
[@@deriving sexp]
