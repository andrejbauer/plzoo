type instruction =
  | NOOP          (* no operation *)
  | SET of int    (* set the given variable with value on stack *)
  | GET of int    (* push the value of the given variable onto stack *)
  | PUSH of int   (* push integer constant onto stack *)
  | VPUSH         (* push onto variable stack *)
  | VPOP          (* pop from vairable stack *)
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | EQ
  | LT
  | AND
  | OR
  | NOT
  | JMP of int    (* relative jump *)
  | JMPZ of int   (* relative jump if zero on the stack *)
  | PRINT

let print_instruction instr ppf =
  match instr with
  | NOOP -> Format.fprintf ppf "NOOP"
  | SET k -> Format.fprintf ppf "SET %d" k
  | GET k -> Format.fprintf ppf "GET %d" k
  | PUSH k -> Format.fprintf ppf "PUSH %d" k
  | VPOP -> Format.fprintf ppf "VPOP"
  | VPUSH -> Format.fprintf ppf "VPUSH"
  | ADD -> Format.fprintf ppf "ADD"
  | SUB -> Format.fprintf ppf "SUB"
  | MUL -> Format.fprintf ppf "MUL"
  | DIV -> Format.fprintf ppf "DIV"
  | MOD -> Format.fprintf ppf "MOD"
  | AND -> Format.fprintf ppf "AND"
  | EQ -> Format.fprintf ppf "EQ"
  | LT -> Format.fprintf ppf "LT"
  | OR -> Format.fprintf ppf "OR"
  | NOT -> Format.fprintf ppf "NOT"
  | JMP k -> Format.fprintf ppf "JMP %d" k
  | JMPZ k -> Format.fprintf ppf "JMPZ %d" k
  | PRINT -> Format.fprintf ppf "PRINT"

type error =
  | Empty_stack
  | Illegal_location of int
  | Illegal_instruction
  | Zero_division

let print_error err ppf =
  match err with
  | Empty_stack -> Format.fprintf ppf "empty stack"
  | Illegal_location k -> Format.fprintf ppf "illegal location %d" k
  | Illegal_instruction -> Format.fprintf ppf "illegal instruction"
  | Zero_division -> Format.fprintf ppf "division by zero"

exception Error of error

let runtime_error err = raise (Error err)

let print_code code ppf =
  Array.iteri (fun k instr -> Format.fprintf ppf "%03d %t@\n" k (print_instruction instr)) code

type state = {
  code : instruction array ;
  mutable stack : int list ; (* evaluation stack *)
  mutable var : int ref list ; (* stack of variables *)
  mutable pc : int (* program counter *)
}

let pop s =
  match s.stack with
  | [] -> runtime_error Empty_stack
  | x :: xs ->
     s.stack <- xs ;
     x

let get_var s k =
  try
    List.nth s.var k
  with
    Failure _ -> runtime_error (Illegal_location k)

let push s x =
  s.stack <- x :: s.stack

let exec s =
  match s.code.(s.pc) with

  | NOOP -> ()

  | SET k ->
     let a = pop s in
     (get_var s k) := a

  | GET k -> push s !(get_var s k)

  | PUSH x -> push s x

  | VPOP ->
     begin match s.var with
     | [] -> runtime_error Illegal_instruction
     | _ :: xs -> s.var <- xs
     end

  | VPUSH ->
     let a = pop s in
     s.var <- ref a :: s.var

  | ADD ->
     let y = pop s in
     let x = pop s in
     push s (x + y)

  | SUB ->
     let y = pop s in
     let x = pop s in
     push s (x - y)

  | MUL ->
     let y = pop s in
     let x = pop s in
     push s (x * y)

  | DIV ->
     let y = pop s in
     let x = pop s in
     if y <> 0 then push s (x / y) else runtime_error Zero_division

  | MOD ->
     let y = pop s in
     let x = pop s in
     if y <> 0 then push s (x mod y) else runtime_error Zero_division

  | EQ ->
     let y = pop s in
     let x = pop s in
     if x = y then push s 1 else push s 0

  | LT ->
     let y = pop s in
     let x = pop s in
     if x < y then push s 1 else push s 0

  | AND ->
     let b = pop s in
     let a = pop s in
     if a <> 0 && b <> 0 then push s 1 else push s 0

  | OR ->
     let b = pop s in
     let a = pop s in
     if a <> 0 || b <> 0 then push s 1 else push s 0

  | NOT ->
     let a = pop s in
     if a <> 0 then push s 0 else push s 1

  | JMP k ->
     s.pc <- s.pc + k

  | JMPZ k ->
     let a = pop s in
     if a = 0 then s.pc <- s.pc + k

  | PRINT ->
     let a = pop s in
     Format.printf "%d@." a

let run s =
  while s.pc < Array.length s.code do
    exec s ;
    s.pc <- s.pc + 1
  done
