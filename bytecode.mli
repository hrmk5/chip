type inst =
  | NOP
  | CONST of int
  | STRING of string
  | STORE of int
  | STOREP of int
  | LOAD of int
  | LOADP of int
  | ADD
  | SUB
  | MUL
  | DIV
  | ALLOC of int
  | JUMP of int
  | JUMP_IF_ZERO of int
  | PRINT

type t

val from_list : inst list -> t

val dump_inst : int -> inst -> unit
val dump_bytecode : t -> unit
