open Printf

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
  | END

type t = inst Array.t

let from_list insts =
  Array.of_list insts

let get_inst bytecode pos =
  Array.get bytecode pos

(* Dump *)

let dump_inst i inst =
  printf "%d  " i;
  match inst with
  | NOP -> print_endline "NOP"
  | CONST n -> printf "CONST %d\n" n
  | STRING s -> printf "STRING \"%s\"\n" s
  | STORE loc -> printf "STORE %d\n" loc
  | STOREP offset -> printf "STOREP %d\n" offset
  | LOAD loc -> printf "LOAD %d\n" loc
  | LOADP offset -> printf "LOADP %d\n" offset
  | ADD -> print_endline "ADD"
  | SUB -> print_endline "SUB"
  | MUL -> print_endline "MUL"
  | DIV -> print_endline "DIV"
  | ALLOC size -> printf "ALLOC size=%d\n" size
  | JUMP i -> printf "JUMP %d\n" i
  | JUMP_IF_ZERO i -> printf "JUMP_IF_ZERO %d\n" i
  | PRINT -> print_endline "PRINT"
  | END -> print_endline "END"

let dump_bytecode bc =
  Array.iteri dump_inst bc
