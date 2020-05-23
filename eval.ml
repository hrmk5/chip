module B = Bytecode

type location =
  | LOCAL of int
  | HEAP of int

module Memory = Map.Make(struct
    type t = location

    let for_compare loc = match loc with
      | LOCAL l -> (0, l)
      | HEAP l -> (1, l)

    let compare a b = 
      let (ai, a) = (for_compare a) in
      let (bi, b) = (for_compare b) in
      let c = Int.compare ai bi in
      if c = 0 then Int.compare a b else c
  end)

(* Value *)

type value =
  | INT of int
  | STRING of string
  | POINTER of location

let unwrap_ptr value = match value with
  | POINTER loc -> loc
  | _ -> failwith "expected a pointer..."

let unwrap_int value = match value with
  | INT n -> n
  | _ -> failwith "expected an integer..."

(* Virtual machine *)

type machine =
  {bytecode : B.t; ip : int; stack : value list; memory : value array Memory.t}

let current {ip; bytecode; _} = B.get_inst bytecode ip

let next vm = {vm with ip = vm.ip + 1}
let jump vm loc = {vm with ip = loc}

let push vm value = {vm with stack = [value] @ vm.stack}
let pop vm = 
  let last = List.hd vm.stack in
  (last, {vm with stack = List.tl vm.stack})

let write vm loc value =
  {vm with memory = Memory.add loc (Array.of_list [value]) vm.memory}
let read vm loc =
  let values = Memory.find loc vm.memory in
  Array.get values 0

let write_offset vm loc offset value =
  let values = Memory.find loc vm.memory in
  Array.set values offset value;
  {vm with memory = Memory.add loc values vm.memory}
let read_offset vm loc offset =
  let values = Memory.find loc vm.memory in
  Array.get values offset

let alloc vm size =
  let loc = HEAP (Memory.cardinal vm.memory) in
  (loc, {vm with memory = Memory.add loc (Array.make size (INT 0)) vm.memory})

(* Evalution *)

let rec eval_vm vm = match current vm with
  | B.NOP -> eval_vm (next vm)
  | B.CONST n -> eval_vm (next (push vm (INT n)))
  | B.STRING s -> eval_vm (next (push vm (STRING s)))
  | B.STORE loc ->
    let (value, vm) = pop vm in
    let vm = write vm (LOCAL loc) value in
    eval_vm (next vm)
  | B.STOREP offset ->
    let (value, vm) = pop vm in
    let (ptr, vm) = pop vm in
    let ptr = unwrap_ptr ptr in
    let vm = write_offset vm ptr offset value in
    eval_vm (next vm)
  | B.LOAD loc ->
    let value = read vm (LOCAL loc) in
    let vm = push vm value in
    eval_vm (next vm)
  | B.LOADP offset ->
    let (ptr, vm) = pop vm in
    let value = read_offset vm (unwrap_ptr ptr) offset in
    let vm = push vm value in
    eval_vm (next vm)
  | B.ADD ->
    let (rhs, vm) = pop vm in
    let (lhs, vm) = pop vm in
    let value = INT (unwrap_int lhs + unwrap_int rhs) in
    let vm = push vm value in
    eval_vm (next vm)
  | B.SUB ->
    let (rhs, vm) = pop vm in
    let (lhs, vm) = pop vm in
    let value = INT (unwrap_int lhs - unwrap_int rhs) in
    let vm = push vm value in
    eval_vm (next vm)
  | B.MUL ->
    let (rhs, vm) = pop vm in
    let (lhs, vm) = pop vm in
    let value = INT (unwrap_int lhs * unwrap_int rhs) in
    let vm = push vm value in
    eval_vm (next vm)
  | B.DIV ->
    let (rhs, vm) = pop vm in
    let (lhs, vm) = pop vm in
    let value = INT (unwrap_int lhs / unwrap_int rhs) in
    let vm = push vm value in
    eval_vm (next vm)
  | B.ALLOC size ->
    let (loc, vm) = alloc vm size in
    let vm = push vm (POINTER loc) in
    eval_vm (next vm)
  | B.JUMP loc ->
    eval_vm (jump vm loc)
  | B.JUMP_IF_ZERO loc ->
    let (b, vm) = pop vm in
    let b = unwrap_int b in
    if b = 0 then eval_vm (jump vm loc)
    else eval_vm (next vm)
  | B.PRINT ->
    let (value, vm) = pop vm in
    (match value with
    | INT n ->
      print_int n;
      print_newline ();
    | STRING s -> print_endline s
    | _ -> failwith "Unsupported value for printing");
    eval_vm (next vm)
  | B.END -> ()

let eval bytecode =
  let vm = {bytecode; ip = 0; stack = []; memory = Memory.empty} in
  eval_vm vm
