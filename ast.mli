type binop =
  | ADD
  | SUB
  | MUL
  | DIV

type expr =
  | INT of int
  | BOOL of bool
  | STRING of string
  | UNIT
  | TUPLE of expr list
  | VAR of string
  | BINOP of binop * expr * expr
  | SEQ of expr list
  | LET of string * expr
  | PRINT of expr
  | FIELD of int * expr
  | IF of expr * expr * expr

type program = { expr : expr }

val binop_name : binop -> string
val dump_expr : int -> expr -> unit
