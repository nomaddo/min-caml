open Sexplib.Std

let counter = ref 0

type t = {name: string; id: int}
[@@deriving sexp]

let genid name =
  incr counter;
  {name; id = !counter}
