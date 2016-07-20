(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

exception Unbound of string

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 =
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
    unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
    (try List.iter2 unify t1s t2s
     with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ ->
    if occur r1 t2 then raise (Unify(t1, t2));
    r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
    if occur r2 t1 then raise (Unify(t1, t2));
    r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec typing env = function
  | Var s -> begin try
      let id = Env.find_name env s in
      let typ = Env.find_type env id in
      {Typedtree.ttype = typ; texp_desc = Typedtree.Var id}
    with Not_found -> begin
        match Env.find_externs s with
        | None ->
          Printf.printf "external variable: %s\n" s;
          let id = Id.genid s in
          let typ = Type.gentyp () in
          Env.add_extern id typ;
          {Typedtree.ttype = typ; texp_desc = Typedtree.Var id}
        | Some (id, typ) ->
          {Typedtree.ttype = typ; texp_desc = Typedtree.Var id}
      end
    end
  | Unit -> {Typedtree.ttype = Type.Unit; texp_desc = Typedtree.Unit}
  | Bool b -> {Typedtree.ttype = Type.Bool; texp_desc = Typedtree.Bool b}
  | Int i -> {Typedtree.ttype = Type.Int; texp_desc = Typedtree.Int i}
  | Float f -> {Typedtree.ttype = Type.Float; texp_desc = Typedtree.Float f}
  | Not e ->
    let e = typing env e in
    unify e.Typedtree.ttype Type.Bool;
    {Typedtree.ttype = Type.Bool; texp_desc = Typedtree.Not e}
  | Neg e ->
    let e = typing env e in
    unify e.Typedtree.ttype Type.Int;
    {Typedtree.ttype = Type.Int; texp_desc = Typedtree.Neg e}
  | Add (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Int;
    unify e2.Typedtree.ttype Type.Int;
    {Typedtree.ttype = Type.Int; texp_desc = Typedtree.Add (e1, e2)}
  | Sub (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Int;
    unify e2.Typedtree.ttype Type.Int;
    {Typedtree.ttype = Type.Int; texp_desc = Typedtree.Sub (e1, e2)}
  | FNeg e ->
    let e = typing env e in
    unify e.Typedtree.ttype Type.Float;
    {Typedtree.ttype = Type.Float; texp_desc = Typedtree.FNeg e}
  | FAdd (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Float;
    unify e2.Typedtree.ttype Type.Float;
    {Typedtree.ttype = Type.Float; texp_desc = Typedtree.FAdd (e1, e2)}
  | FSub (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Float;
    unify e2.Typedtree.ttype Type.Float;
    {Typedtree.ttype = Type.Float; texp_desc = Typedtree.FSub (e1, e2)}
  | FMul (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Float;
    unify e2.Typedtree.ttype Type.Float;
    {Typedtree.ttype = Type.Float; texp_desc = Typedtree.FMul (e1, e2)}
  | FDiv (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Float;
    unify e2.Typedtree.ttype Type.Float;
    {Typedtree.ttype = Type.Float; texp_desc = Typedtree.FDiv (e1, e2)}
  | Eq (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Int;
    unify e2.Typedtree.ttype Type.Int;
    {Typedtree.ttype = Type.Bool; texp_desc = Typedtree.Eq (e1, e2)}
  | LE (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Int;
    unify e2.Typedtree.ttype Type.Int;
    {Typedtree.ttype = Type.Bool; texp_desc = Typedtree.LE (e1, e2)}
  | If (e1, e2, e3) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    let e3 = typing env e3 in
    unify e1.Typedtree.ttype Type.Bool;
    unify e2.Typedtree.ttype e3.Typedtree.ttype;
    {Typedtree.ttype = e3.Typedtree.ttype; texp_desc = Typedtree.If (e1, e2, e3)}
  | Let (s, e1, e2) ->
    let id = Id.genid s in
    let e1 = typing env e1 in
    let newenv = Env.add_name env s id in
    let e2 = typing newenv e2 in
    {Typedtree.ttype = e2.Typedtree.ttype; texp_desc = Typedtree.Let (id, e1, e2)}
  | LetRec (def, e) ->
    let fname = Id.genid def.name in
    let targs = List.map Id.genid def.args in
    let newenv =
      List.fold_left2 (fun env s id -> Env.add_name env s id) env
        (def.name :: def.args) (fname :: targs) in
    let fbody = typing newenv def.body in
    let e = typing newenv e in
    let arg_types =
      List.map (Env.find_type newenv) targs in
    let ftype = Type.Fun (arg_types, fbody.Typedtree.ttype) in
    { Typedtree.texp_desc =
        Typedtree.LetRec ({Typedtree.fname; targs; fbody; ftype}, e);
      ttype = e.Typedtree.ttype }
  | App (e, es) ->
    let e = typing env e in
    let es = List.map (typing env) es in
    let args = List.map (fun _ -> Type.gentyp ()) es in
    let ret = Type.gentyp () in
    unify e.Typedtree.ttype (Type.Fun (args, ret));
    List.iter2 (fun typ e -> unify typ e.Typedtree.ttype) args es;
    { Typedtree.texp_desc = Typedtree.App (e, es); ttype = ret}
  | Tuple es ->
    let es = List.map (typing env) es in
    let ttype = Type.Tuple (List.map (fun e -> e.Typedtree.ttype) es) in
    { Typedtree.texp_desc = Typedtree.Tuple es; ttype}
  | LetTuple (ss, e1, e2) ->
    let ids = List.map Id.genid ss in
    let e1 = typing env e1 in
    let newenv =
      List.fold_left2 (fun env s id -> Env.add_name env s id) env
        ss ids in
    let e2 = typing newenv e2 in
    let texp_desc = Typedtree.LetTuple (ids, e1, e2) in
    let ttype = e2.Typedtree.ttype in
    {Typedtree.texp_desc; ttype}
  | Array (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    unify e1.Typedtree.ttype Type.Int;
    let texp_desc = Typedtree.Array (e1, e2) in
    {texp_desc; Typedtree.ttype = Type.Array e2.Typedtree.ttype}
  | Get (e1, e2) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    let ret = Type.gentyp () in
    let typ = Type.Array ret in
    unify e1.Typedtree.ttype typ;
    unify e2.Typedtree.ttype Type.Int;
    let texp_desc = Typedtree.Get (e1, e2) in
    {Typedtree.texp_desc; ttype = ret}
  | Put (e1, e2, e3) ->
    let e1 = typing env e1 in
    let e2 = typing env e2 in
    let e3 = typing env e3 in
    let ret = Type.gentyp () in
    let typ = Type.Array ret in
    unify e1.Typedtree.ttype typ;
    unify e2.Typedtree.ttype Type.Int;
    unify ret e3.Typedtree.ttype;
    let texp_desc = Typedtree.Put (e1, e2, e3) in
    {Typedtree.texp_desc; ttype = Type.Unit}

let type_implementation s =
  Env.clean_externs ();
  let e = typing Env.empty s in
  try
    unify e.Typedtree.ttype Type.Unit;
    e
  with Unify _ ->
    Printf.printf "error: top-level must have type unit\n";
    exit 1
