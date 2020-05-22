let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program =  Parser.program Lexer.lex lexbuf in
  let ir = Sema.analyze program in
  List.iter Ir.dump_ir ir;;

main ()
