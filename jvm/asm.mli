type label = string

type instruction =
(* constant *)
| Iconst       of int
| Dconst       of float
(* access for local values *)
| Load         of Type.t * int
| Store        of Type.t * int
(* arithmetic operations *)
| Add          of Type.t
| Sub          of Type.t
| Mul          of Type.t
| Div          of Type.t
| Neg          of Type.t
(* function call: need to exact types for overloading *)
| InvokeStatic of Id.t * Type.t
(* for if statements *)
| Goto         of label
| Label        of label
| Ifeq         of label
| Ifle         of label
| GetStatic    of Type.t * string
| Return       of Type.t
| ReturnVoid
| Dup
| CheckCast    of Type.t
(* array operations *)
| AnewArray
| Aload        of Type.t
| Astore       of Type.t
(* virtual instructions *)
| CreateArray  of [`I | `D | `A]
| Boxing       of Type.t
| Unboxing     of Type.t

type prog = Prog of cls list * cls * main

and main = int * int * instruction list

and cls = {
  cls_name: string;
  cls_fields: (Id.t * Type.t) list;
  cls_methods: meth list
}

and modifier = Public | Static | Private

and meth = {
  meth_name: string;
  meth_stack: int;
  meth_locals: int;
  meth_typ: Type.t;
  meth_instructs: instruction list
}
