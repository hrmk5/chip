let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program =  Parser.program Lexer.lex lexbuf in
  let ir = Sema.analyze program in
  let bytecode = Gen.gen ir in
  Bytecode.dump_bytecode bytecode;;

main ()
