type label
val new_label : unit -> label
val label_str : label -> string
val label_int: label -> int

type temp
val new_temp : unit -> temp
val temp_str : temp -> string

type ir =
  | NOP
  | CONST of temp * int
  | STRING of temp * string
  | MOVE of temp * temp
  | STORE of int * temp
  | STOREP of temp * int * temp
  | LOAD of temp * int
  | LOADP of temp * temp * int 
  | ADD of temp * temp * temp
  | SUB of temp * temp * temp
  | MUL of temp * temp * temp
  | DIV of temp * temp * temp
  | ALLOC of temp * int
  | LABEL of label
  | JUMP of label
  | JUMP_IF_ZERO of label * temp
  | PRINT of temp

val dump_ir : ir -> unit
