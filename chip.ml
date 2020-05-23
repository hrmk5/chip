let main () =
  let lexbuf = Lexing.from_channel stdin in
  let program =  Parser.program Lexer.lex lexbuf in
  Ast.dump_expr 0 program.expr;
  print_endline "--------------------";
  let ir = Sema.analyze program in
  List.iter Ir.dump_ir ir;
  print_endline "--------------------";
  let bytecode = Gen.gen ir in
  Bytecode.dump_bytecode bytecode;;

main ()
