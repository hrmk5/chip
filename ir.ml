open Printf 

(* Label *)
type label = int

let next_label = ref 0

let new_label () =
  let label = !next_label in
  next_label := !next_label + 1;
  label

let label_str label = "L" ^ string_of_int label

(* Temporary *)

type temp = int

let next_temp = ref 0

let new_temp () =
  let temp = !next_temp in
  next_temp := !next_temp + 1;
  temp

let temp_str temp = "t" ^ string_of_int temp

(* IR *)

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

let dump_ir ir = match ir with
  | NOP -> print_endline "NOP\n"
  | CONST (dst, n) -> printf "%s <- %d\n" (temp_str dst) n
  | STRING (dst, s) -> printf "%s <- %s\n" (temp_str dst) s
  | MOVE (dst, src) -> printf "%s <- %s\n" (temp_str dst) (temp_str src)
  | STORE (loc, src) -> printf "[%d] <- %s\n" loc (temp_str src)
  | STOREP (ptr, offset, src) -> 
     printf "[%s].%d <- %s\n" (temp_str ptr) offset (temp_str src)
  | LOAD (dst, loc) -> printf "%s <- [%d]\n" (temp_str dst) loc
  | LOADP (dst, ptr, offset) -> 
     printf "%s <- [%s].%d\n" (temp_str dst) (temp_str ptr) offset
  | ADD (dst, lhs, rhs) ->
     printf "%s <- %s + %s\n" (temp_str dst) (temp_str lhs) (temp_str rhs)
  | SUB (dst, lhs, rhs) ->
     printf "%s <- %s - %s\n" (temp_str dst) (temp_str lhs) (temp_str rhs)
  | MUL (dst, lhs, rhs) ->
     printf "%s <- %s * %s\n" (temp_str dst) (temp_str lhs) (temp_str rhs)
  | DIV (dst, lhs, rhs) ->
     printf "%s <- %s / %s\n" (temp_str dst) (temp_str lhs) (temp_str rhs)
  | ALLOC (dst, size) -> printf "%s <- alloc size=%d\n" (temp_str dst) size
  | LABEL label -> printf "%s:\n" (label_str label)
  | JUMP label -> printf "jump %s\n" (label_str label)
  | JUMP_IF_ZERO (label, temp) -> printf "jump_if_zero %s %s\n" (label_str label) (temp_str temp)
  | PRINT temp -> printf "print %s\n" (temp_str temp)
