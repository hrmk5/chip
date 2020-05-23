module I = Ir
module B = Bytecode
module LabelMap = Map.Make(Int)

let rec genir labels i irs : int LabelMap.t * B.inst list = 
  let gen ir = match ir with
    | I.NOP -> []
    | I.CONST (_, n) -> [B.CONST n]
    | I.STRING (_, s) -> [B.STRING s]
    | I.MOVE _ -> []
    | I.STORE (loc, _) -> [B.STORE loc]
    | I.STOREP (_, offset, _) -> [B.STOREP offset]
    | I.LOAD (_, loc) -> [B.LOAD loc]
    | I.LOADP (_, _, offset) -> [B.LOADP offset]
    | I.ADD _ -> [B.ADD]
    | I.SUB _ -> [B.SUB]
    | I.MUL _ -> [B.MUL]
    | I.DIV _ -> [B.DIV]
    | I.ALLOC (_, size) -> [B.ALLOC size]
    | I.LABEL _ -> failwith "arienai"
    | I.JUMP label -> [B.JUMP (I.label_int label)]
    | I.JUMP_IF_ZERO (label, _) -> [B.JUMP_IF_ZERO (I.label_int label)]
    | I.PRINT _ -> [B.PRINT] in

  match irs with
  | [] -> (labels, [])
  | I.LABEL label::rest ->
     let labels = LabelMap.add (I.label_int label) i labels in
     genir labels i rest
  | ir::rest ->
     let inst = gen ir in
     let (labels, rest_insts) = genir labels (i + List.length inst) rest in
     (labels, inst @ rest_insts)

let resolve labels inst = match inst with
  | B.JUMP loc ->
     let loc = LabelMap.find loc labels in
     B.JUMP loc
  | B.JUMP_IF_ZERO loc ->
     let loc = LabelMap.find loc labels in
     B.JUMP_IF_ZERO loc
  | inst -> inst

let gen irs =
  let (labels, irs) = genir LabelMap.empty 0 irs in
  let irs = List.map (resolve labels) irs in
  let irs = irs @ [B.END] in
  B.from_list irs
