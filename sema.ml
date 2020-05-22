module A = Ast
module I = Ir

module VarMap = Map.Make(String)

type result = { ir : I.ir list; out : I.temp option }
type env = { vars: int VarMap.t }

let error msg =
  print_endline msg

let invalid_temp = I.new_temp ()
let unwrap temp = match temp with
  | Some temp -> temp
  | None -> invalid_temp

let new_var env name =
  let loc = VarMap.cardinal env.vars in
  let vars = VarMap.add name loc env.vars in
  (loc, {vars})

let rec trans_seq env exprs = match exprs with
  | [] -> []
  | A.LET (name, expr)::rest ->
     let (loc, env) = new_var env name in
     let expr = trans_expr env expr in
     [{ir = expr.ir @ [I.STORE (loc, unwrap expr.out)];
       out = None}]
     @ trans_seq env rest
  | expr::rest ->
     [trans_expr env expr]
     @ trans_seq env rest

and trans_tuple env ptr offset exprs = match exprs with
  | [] -> []
  | expr::rest ->
     let expr = trans_expr env expr in
     expr.ir
     @ [I.STOREP (ptr, offset, unwrap expr.out)]
     @ trans_tuple env ptr (offset + 1) rest

and trans_expr env expr = match expr with
  | A.INT n ->
     let temp = I.new_temp () in
     {ir = [I.CONST (temp, n)];
      out = Some temp}
  | A.BOOL true ->
     let temp = I.new_temp () in
     {ir = [I.CONST (temp, 1)];
      out = Some temp}
  | A.BOOL false ->
     let temp = I.new_temp () in
     {ir = [I.CONST (temp, 0)];
      out = Some temp}
  | A.STRING s ->
     let temp = I.new_temp () in
     {ir = [I.STRING (temp, s)];
      out = Some temp}
  | A.UNIT -> {ir = []; out = None}
  | A.TUPLE exprs ->
      let ptr = I.new_temp () in
      let ir = trans_tuple env ptr 0 exprs in
      {ir = [I.ALLOC (ptr, List.length exprs)]
       @ ir;
       out = Some ptr}
  | A.VAR name ->
    (match VarMap.find_opt name env.vars with
     | Some loc ->
        let temp = I.new_temp () in
        {ir = [I.LOAD (temp, loc)];
         out = Some temp}
     | None -> {ir = [I.NOP]; out = None})
  | A.BINOP (binop, lhs, rhs) ->
     let lhs = trans_expr env lhs in
     let rhs = trans_expr env rhs in
     let temp = I.new_temp () in
     let ir = 
       (match binop with
        | A.ADD -> I.ADD (temp, unwrap lhs.out, unwrap rhs.out)
        | A.SUB -> I.SUB (temp, unwrap lhs.out, unwrap rhs.out)
        | A.MUL -> I.MUL (temp, unwrap lhs.out, unwrap rhs.out)
        | A.DIV -> I.DIV (temp, unwrap lhs.out, unwrap rhs.out)) in
     {ir = lhs.ir @ rhs.ir @ [ir];
      out = Some temp}
  | A.SEQ exprs ->
     let results = trans_seq env exprs in
     let out = (List.hd results).out in
     {ir = results
           |> List.map (fun r -> r.ir)
           |> List.flatten;
      out}
  | A.LET _ -> 
     error "Invalid position of let expression";
     {ir = []; out = None}
  | A.PRINT expr ->
     let expr = trans_expr env expr in
     {ir = expr.ir @ [I.PRINT (unwrap expr.out)];
      out = None}
  | A.FIELD (n, tuple) ->
     let tuple = trans_expr env tuple in
     let temp = I.new_temp () in
     {ir = tuple.ir @ [I.LOADP (temp, unwrap tuple.out, n)];
      out = Some temp}
  | A.IF (cond, thenc, elsec) ->
     let cond = trans_expr env cond in
     let thenc = trans_expr env thenc in
     let elsec = trans_expr env elsec in
     let else_label = I.new_label () in 
     let end_label = I.new_label () in
     let temp = I.new_temp () in
     {ir = 
       cond.ir
       @ [I.JUMP_IF_ZERO (else_label, unwrap cond.out)]
       @ thenc.ir
       @ [I.JUMP end_label;
          I.LABEL else_label]
       @ elsec.ir
       @ [I.LABEL end_label;
          I.MOVE (temp, unwrap thenc.out);
          I.MOVE (temp, unwrap elsec.out)];
      out = Some temp}

let analyze (program : A.program) =
  let res = trans_expr {vars = VarMap.empty} program.expr in
  res.ir
