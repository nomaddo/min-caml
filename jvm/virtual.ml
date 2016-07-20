open Asm
open Closure

let type_tbl = Hashtbl.create 100 (* id -> Type.t *)
let add_typ = Hashtbl.add type_tbl
let dummy_atyp = Type.Tuple []

type var = Local of int | Byte of Asm.instruction list * int

module Locals = Map.Make(struct type t = Id.t;; let compare = compare end)
(* id -> instruct list *)

let create_label =
  let r = ref 0 in
  fun s t -> incr r; (s ^ string_of_int !r, t ^ string_of_int !r)

let lib_name = "CamlLib/"
let cls_name = "Caml"

let is_external s =
  if String.length s < 9
  then false else String.sub s 0 9 = "min_caml_"

let size = function
| Type.Float -> 2
| _ -> 1

let transl_const = function
| Closure.Unit -> Some (Iconst 0, 1)
| Int i -> Some (Iconst i, 1)
| Float f -> Some (Dconst f, 2)
| _ -> None

let transl_id locals x =
  match Locals.find x locals with
  | Local i ->
    let inss = [Load (Hashtbl.find type_tbl x, i)] in
    (size (Hashtbl.find type_tbl x), inss)
  | Byte (inss, len) -> (len, inss)

let transl_arith locals t =
  let op2 x y last =
    let slen1, inss1 = transl_id locals x in
    let slen2, inss2 = transl_id locals y in
    (slen1 + slen2, inss1 @ inss2 @ last) in
  let op1 x last =
    let slen1, inss1 = transl_id locals x in
    (slen1, inss1 @ last) in
  match t with
  | Closure.Add (x, y) -> op2 x y [Asm.Add Type.Int]
  | Sub (x, y) -> op2 x y [Sub Type.Int]
  | Neg x -> op1 x [Neg Type.Int]
  | FNeg x -> op1 x [Neg Type.Float]
  | FAdd (x, y) -> op2 x y [Asm.Add Type.Float]
  | FSub (x, y) -> op2 x y [Asm.Sub Type.Float]
  | FMul (x, y) -> op2 x y [Asm.Mul Type.Float]
  | FDiv (x, y) -> op2 x y [Asm.Div Type.Float]
  | _ -> failwith "transl_arith"

let transl_get slen llen locals arr x =
  let content_typ (Type.Array typ) = typ in
  let typ = Hashtbl.find type_tbl arr |> content_typ in
  let slen1, inss1 = transl_id locals arr in
  let slen2, inss2 = transl_id locals x in
  (slen + 2, llen,
   locals,
   inss1 @ inss2 @ [Aload typ]
   @ (match typ with Type.Fun _ | Type.Array _ | Type.Tuple _ -> [CheckCast typ] | _ -> []))

let transl_put slen llen locals arr ind x =
  let slen1, inss1 = transl_id locals arr in
  let slen2, inss2 = transl_id locals ind in
  let slen3, inss3 = transl_id locals x in
  (max slen1 (max (slen2 + 1) (slen3 + 2)),
   llen,
   locals, inss1 @ inss2 @ inss3 @ [Astore (Hashtbl.find type_tbl x); Iconst 0])
(* XXX: putはunit型の演算なので返り値を最適化できるはずなのだけど、、、 *)

let transl_tuple slen llen locals ids =
  List.fold_left (fun (ind, slen, inss) id ->
    let slen', inss' = transl_id locals id in
    let typ = Hashtbl.find type_tbl id in
    let inss'' = Dup :: Iconst ind :: inss' @ [Boxing typ; Astore dummy_atyp] in
    (succ ind, max slen (slen' + 2), inss @ inss'') ) (0, 0, []) ids
  |> fun (_, slen', inss) ->
  (slen + slen', llen, locals, Iconst (List.length ids) :: AnewArray :: inss)

let rec transl_let_tuple slen llen locals l x t =
  let slen1, inss1 = transl_id locals x in
  let _, llen2, locals2, inss2 =
    List.fold_left (fun (ind, llen, locals, inss) (y, typ) ->
      Hashtbl.add type_tbl y typ;
      (succ ind,
       llen + 1,
       Locals.add y (Local llen) locals,
       inss @ inss1 @ [Iconst ind; Aload dummy_atyp; CheckCast typ; Unboxing typ; Store (typ, llen)])
    ) (0, llen, locals, []) l in
  let slen3, llen3, locals3, inss3 = transl slen llen2 locals2 t in
  (max (slen + 3) slen3, llen3, locals3, inss2 @ inss3)

and transl_if slen llen locals f x y else_t then_t =
  let slen1, inss1 = transl_id locals x in
  let slen2, inss2 = transl_id locals y in
  let slen3, llen3, locals3, inss3 = transl slen llen locals else_t in
  let slen4, llen4, locals4, inss4 = transl slen llen locals then_t in
  let l1, l2 = create_label "THEN" "END" in
  let inss' = inss1 @ inss2 @ [f l1] @ inss3 @ Goto l2 :: Label l1 :: inss4 @ [Label l2] in
  (max (max slen1 (slen2 + 1)) (max slen3 slen4),
   max llen3 llen4,
   locals4, (* XXX: ここで返すlocalsはこれでOK？ *)
   inss')

and transl slen llen locals t = match t with
| Closure.Unit -> (slen + 1, llen, locals, [Iconst 0])
| Int i -> (slen + 1, llen, locals, [Iconst i])
| Float f -> (slen + 2, llen, locals, [Dconst f])
| Var x ->
  let slen', inss = transl_id locals x in
  (slen + slen', llen, locals, inss)
| Let ((x, typ), t1, t2) ->
  Hashtbl.add type_tbl x typ;
  begin match transl_const t1 with
  | Some (ins, size) ->
    let locals = Locals.add x (Byte ([ins], size)) locals in
    transl slen llen locals t2
  | None ->
    let slen1, llen1, locals1, inss1 = transl slen llen locals t1 in
    let locals = Locals.add x (Local llen) locals1 in
    let slen2, llen2, locals2, inss2 = transl slen (llen + size typ) locals t2 in
    (slen + max slen1 slen2 , llen + max llen1 llen2, locals2, inss1 @ Store (typ, llen) :: inss2)
  end
| AppDir (Id.L "min_caml_create_array", [x; y]) ->
  let slen1, inss1 = transl_id locals x in
  let slen2, inss2 = transl_id locals y in
  let typ =
    Hashtbl.find type_tbl y
    |> function Type.Int | Type.Bool | Type.Unit -> `I | Type.Float -> assert false | _ -> `A in
  (slen + max 2 (max slen1 (slen2 + 1)), llen, locals, inss1 @ inss2 @ [CreateArray typ])
| AppDir (Id.L "min_caml_create_float_array", [x; y]) ->
  let slen1, inss1 = transl_id locals x in
  let slen2, inss2 = transl_id locals y in
  let typ =
    Hashtbl.find type_tbl y
    |> function Type.Float -> `D | _ -> assert false in
  (slen + max 2 (max slen1 (slen2 + 1)), llen, locals, inss1 @ inss2 @ [CreateArray typ])
| AppDir (Id.L x, ids) ->
  let name = if is_external x then lib_name ^ x else cls_name ^ "/" ^ x in
  let ins = InvokeStatic (name, Hashtbl.find type_tbl x) in
  let slen', inss =
  List.fold_left (fun (slen, inss) id ->
    transl_id locals id
    |> fun (slen', inss') -> (slen + slen', inss @ inss')) (0, []) ids  in
  (slen + slen', llen, locals, inss @ [ins])
| Add _ | Sub _ | Neg _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ ->
  let slen', inss = transl_arith locals t in
  (slen + slen', llen, locals, inss)
| IfEq (x, y, t1, t2) -> transl_if slen llen locals (fun x -> Asm.Ifeq x) x y t1 t2
| IfLE (x, y, t1, t2) -> transl_if slen llen locals (fun x -> Asm.Ifle x) x y t1 t2
| Tuple ids -> transl_tuple slen llen locals ids
| LetTuple (l, x, t) -> transl_let_tuple slen llen locals l x t
| Get (x, y) -> transl_get slen llen locals x y
| Put (arr, ind, x) -> transl_put slen llen locals arr ind x
| ExtArray (Id.L arr) ->
  let ins = GetStatic (Hashtbl.find type_tbl arr, lib_name ^ arr) in
  (slen + 1, llen, locals, [ins])
| MakeCls _ | AppCls _ -> failwith "not implemented yet"

let fundef {name=(Id.L x, typ); args; formal_fv; body} =
  let ret_typ = function Type.Fun (_, t) -> t | _ -> assert false in
  assert (formal_fv = []);      (* closureに対応してない *)
  let llen, locals =
    List.fold_left (fun (ind, m) (x, typ) ->
      Hashtbl.add type_tbl x typ;
      (ind + (size typ), Locals.add x (Local ind) m)
    ) (0, Locals.empty) args in
  let slen, llen, _, inss = transl 0 llen locals body in
  {meth_name=x; meth_stack=slen; meth_locals=llen+1; meth_typ=typ;
   meth_instructs=inss @ [Return (ret_typ typ)]}

let f (Closure.Prog (fs, t)) =
  Hashtbl.clear type_tbl;
  Hashtbl.add type_tbl "min_caml_create_array" (Type.Fun ([Type.Int; Type.Float], Type.Array Type.Float));
  Hashtbl.add type_tbl "min_caml_create_array" (Type.Fun ([Type.Int; Type.Float], Type.Array Type.Float));
  List.iter (fun {name=(Id.L x, typ)} -> Hashtbl.add type_tbl x typ) fs;
  M.iter (fun k v -> Hashtbl.add type_tbl ("min_caml_" ^ k) v) !Typing.extenv;
  let meths = List.map fundef fs in
  let slen, llen, _, inss = transl 0 0 Locals.empty t in
  let main = (slen + 1, llen, inss @ [ReturnVoid]) in
  let cls = {cls_name=cls_name; cls_fields=[]; cls_methods=meths} in
  Asm.Prog ([], cls, main)
