open Sexplib.Std

type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of string * t * t
  | Var of string
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of string list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t

and fundef = { name : string; args : string list; body : t }
[@@deriving sexp]
