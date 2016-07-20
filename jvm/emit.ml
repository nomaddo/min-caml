open Format
open Asm
open Type

let obj = "Ljava/lang/Object;"

let rec seq printer sep fmt = function
| [] -> ()
| [x] -> printer fmt x
| x::xs -> printer fmt x; fprintf fmt "%s" sep; seq printer sep fmt xs

let modifiers fmt l =
  let modifier fmt m =
    fprintf fmt (match m with
  | Public -> "public"
  | Static -> "static"
  | Private-> "private") in
  seq modifier " " fmt l

let prim_typ = function
| Unit | Bool | Int -> `I
| Float -> `D
| Fun _ | Tuple _ | Array _ -> `A
| Var _ -> assert false

let branch_prim typ =
  match prim_typ typ with `I -> "i" | `D -> "d" | `A -> "a"

let arith_typ = function
| Int -> `I
| Float -> `D
| _ -> assert false

let rec verbose_typ fmt = function
| Unit | Bool | Int -> fprintf fmt "I"
| Float -> fprintf fmt "D"
| Tuple _ -> fprintf fmt "[%s" obj
| Array t -> fprintf fmt "[%a" verbose_typ t
| _ -> assert false

let fun_typ fmt (Fun (l, r))  =
  fprintf fmt "(%a)%a" (seq verbose_typ "") l verbose_typ r

let instruct fmt = function
| Iconst (-1) -> fprintf fmt "iconst_m1"
| Iconst i when 0 <= i && i <= 5 -> fprintf fmt "iconst_%d" i
| Iconst i -> fprintf fmt "ldc %d" i
| Dconst 0.0 -> fprintf fmt "dconst_0"
| Dconst 1.0 -> fprintf fmt "dconst_1"
| Dconst f -> fprintf fmt "ldc2_w %f" f
| Load (typ, i)  -> fprintf fmt "%sload %d" (branch_prim typ) i
| Store (typ, i) -> fprintf fmt "%sstore %d"(branch_prim typ)  i
| Add typ -> fprintf fmt "%s" (match arith_typ typ with `I -> "iadd" | `D -> "dadd")
| Sub typ -> fprintf fmt "%s" (match arith_typ typ with `I -> "isub" | `D -> "dsub")
| Mul typ -> fprintf fmt "%s" (match arith_typ typ with `I -> "imul" | `D -> "dmul")
| Div typ -> fprintf fmt "%s" (match arith_typ typ with `I -> "idiv" | `D -> "ddiv")
| Neg typ -> fprintf fmt "%s" (match arith_typ typ with `I -> "ineg" | `D -> "dneg")
| InvokeStatic (f, typ) -> fprintf fmt "invokestatic %s%a" f fun_typ typ
| Goto l -> fprintf fmt "goto %s" l
| Label l -> fprintf fmt "%s:" l
| Ifeq l -> fprintf fmt "if_icmpeq %s" l
| Ifle l -> fprintf fmt "if_icmple %s" l
| GetStatic (typ, s) -> fprintf fmt "getstatic %s %a" s verbose_typ typ
| Return typ -> fprintf fmt "%sreturn" (branch_prim typ)
| ReturnVoid -> fprintf fmt "return"
| Dup -> fprintf fmt "dup"
| AnewArray -> fprintf fmt "anewarray %s" obj
| Aload typ -> fprintf fmt "%saload" (branch_prim typ)
| Astore typ -> fprintf fmt "%sastore" (branch_prim typ)

(* translate virtual instructions *)
| CreateArray `I ->
  fprintf fmt "invokestatic %smin_caml_create_array(II)[I" Virtual.lib_name
| CreateArray `D ->
  fprintf fmt "invokestatic %smin_caml_create_array(ID)[D" Virtual.lib_name
| CreateArray `A ->
  fprintf fmt "invokestatic %smin_caml_create_array(I%s)[%s" Virtual.lib_name obj obj
| Boxing   typ -> fprintf fmt "invokestatic %s"
    (match prim_typ typ with
    | `I -> "java/lang/Integer/valueOf(I)Ljava/lang/Integer;"
    | `D -> "java/lang/Double/valueOf(D)Ljava/lang/Double;"
    | `A -> "");
| Unboxing typ -> fprintf fmt "%s"
    (match prim_typ typ with
    | `I -> "invokevirtual java/lang/Integer/intValue()I"
    | `D -> "invokevirtual java/lang/Double/doubleValue()D"
    | `A -> "") (* XXX: castこれでよい？ *)
| CheckCast typ ->
  fprintf fmt "checkcast %s"
    (match prim_typ typ with
    | `I -> "java/lang/Integer"
    | `D -> "java/lang/Double"
    | `A -> "[Ljava/lang/Object;") (* XXX: castこれでよい？ *)

let instructs fmt l =
  let ins fmt s = match s with
  | Goto _ -> fprintf fmt "%a@." instruct s
  | _ -> fprintf fmt "  %a@." instruct s in
  (* let ins fmt s = fprintf fmt "%a@." instruct s in *)
  List.iter (ins fmt) l

let out_main fmt (slen, llen, l) =
  fprintf fmt ".method public static main([Ljava/lang/String;)V@.";
  fprintf fmt "  .limit stack %d@." slen;
  fprintf fmt "  .limit locals %d@." llen;
  instructs fmt l;
  fprintf fmt ".end method@."

let meth fmt {meth_name; meth_stack; meth_locals; meth_typ; meth_instructs} =
  fprintf fmt ".method %a %s%a@."
    modifiers [Public; Static] meth_name fun_typ meth_typ;
  fprintf fmt "  .limit stack %d@." meth_stack;
  fprintf fmt "  .limit locals %d@.@." meth_locals;
  fprintf fmt "%a" instructs meth_instructs;
  fprintf fmt ".end method@.@."

let out_cls fmt {cls_name; cls_methods} =
  fprintf fmt ".class %a %s@." modifiers [Public; Static] cls_name;
  fprintf fmt ".super %s@." "java/lang/Object";
  List.iter (meth fmt) cls_methods

let rec f outchan (Asm.Prog (l, cls, main)) =
  assert (l = []);              (* まだクロージャには対応してない *)
  assert (cls.cls_fields = []);
  let fmt = formatter_of_out_channel outchan in
  out_cls fmt cls;
  out_main fmt main
