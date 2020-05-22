let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program =  Parser.program Lexer.lex lexbuf in
  Ast.dump_expr 0 program.expr;;

main ()
