val limit : int ref
val string : string -> unit
val file : string -> unit
val file_to_cls : string -> Closure.prog
val str_to_cls : string -> Closure.prog
val file_to_virtual: string -> Asm.prog
val str_to_virtual: string -> Asm.prog
