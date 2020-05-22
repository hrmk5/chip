open Printf

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

let binop_name binop = match binop with
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"

let rec print_repeat str count = match count with
  | 0 -> ()
  | count -> 
     print_string str;
     print_repeat str (count - 1)

let rec dump_expr depth expr =
  print_repeat "  " depth;
  match expr with
  | INT n -> printf "%d\n" n
  | BOOL true -> print_endline "true"
  | BOOL false -> print_endline "false"
  | STRING s -> printf "\"%s\"\n" s
  | UNIT -> print_endline "()"
  | TUPLE exprs ->
     print_endline "tuple";
     List.iter (dump_expr (depth + 1)) exprs
  | VAR name -> printf "%s\n" name;
  | BINOP (op, lhs, rhs) ->
     printf "%s\n" (binop_name op);
     dump_expr (depth + 1) lhs;
     dump_expr (depth + 1) rhs;
  | SEQ exprs -> 
      print_endline "seq";
     List.iter (dump_expr (depth + 1)) exprs
  | LET (name, expr) ->
     printf "let %s =\n" name;
     dump_expr (depth + 1) expr
  | PRINT expr ->
     print_endline "print";
     dump_expr (depth + 1) expr
  | FIELD (n, expr) ->
     printf "#%d\n" n;
     dump_expr (depth + 1) expr;
  | IF (cond, thenc, elsec) ->
     print_endline "if";
     dump_expr (depth + 1) cond;
     print_repeat "  " depth;
     print_endline "then";
     dump_expr (depth + 1) thenc;
     print_repeat "  " depth;
     print_endline "else";
     dump_expr (depth + 1) elsec;

