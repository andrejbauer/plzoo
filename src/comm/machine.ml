(** A simple machine with a program, RAM, program counter and a stack pointer. The program
   is an array of instructions. The stack grows downwards. Arithmetical and boolean
   operations operate on the stack. *)

(** The machine instructions. *)
type instruction =
  | NOOP          (* no operation *)
  | SET of int    (* pop from stack and store in the given location *)
  | GET of int    (* push from given location onto stack *)
  | PUSH of int   (* push integer constant onto stack *)
  | ADD           (* addition *)
  | SUB           (* subtraction *)
  | MUL           (* multiplication *)
  | DIV           (* division *)
  | MOD           (* remainder *)
  | EQ            (* equal *)
  | LT            (* less than *)
  | AND           (* conjunction *)
  | OR            (* disjunction *)
  | NOT           (* negation *)
  | JMP of int    (* relative jump *)
  | JMPZ of int   (* relative jump if zero on the stack *)
  | PRINT         (* pop from stack and print *)

(** Print an instruction. *)
let print_instruction instr ppf =
  match instr with
  | NOOP -> Format.fprintf ppf "NOOP"
  | SET k -> Format.fprintf ppf "SET %d" k
  | GET k -> Format.fprintf ppf "GET %d" k
  | PUSH k -> Format.fprintf ppf "PUSH %d" k
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

(** Machine errors *)
type error =
  | Illegal_address
  | Illegal_instruction
  | Zero_division

(** Print a machine error *)
let print_error err ppf =
  match err with
  | Illegal_address -> Format.fprintf ppf "illegal address"
  | Illegal_instruction -> Format.fprintf ppf "illegal instruction"
  | Zero_division -> Format.fprintf ppf "division by zero"

exception Error of error

(** Signal a machine error *)
let runtime_error err = raise (Error err)

(** Print the program *)
let print_code code ppf =
  Array.iteri (fun k instr -> Format.fprintf ppf "%03d %t@\n" k (print_instruction instr)) code

(** The state of the machine *)
type state = {
  code : instruction array ; (* program *)
  ram : int array ; (* random-access memory *)
  mutable pc : int ; (* program counter *)
  mutable sp : int (* stack pointer *)
}

(** Initialize the state with give program and RAM size *)
let make code ram_size =
  {
    code = code ;
    ram = Array.init ram_size (fun _ -> 0) ;
    pc = 0 ;
    sp = ram_size - 1
  }

(** In state [s], read from memory location [k] *)
let read s k =
  try
    s.ram.(k)
  with
  | Invalid_argument _ -> runtime_error Illegal_address

(** In state [s], write [x] to memory location [k] *)
let write s k x =
  try
    s.ram.(k) <- x
  with
  | Invalid_argument _ -> runtime_error Illegal_address

(** In given state [s], pop from the stack *)
let pop s =
  let x = read s s.sp in
  s.sp <- s.sp + 1 ;
  x

(** In given state [s], push [x] onto the stack *)
let push s x =
  s.sp <- s.sp - 1 ;
  write s s.sp x

(** Execute a command in the given state. *)
let exec s =
  match s.code.(s.pc) with

  | NOOP -> ()

  | SET k ->
     let a = pop s in
     write s k a

  | GET k ->
     let a = read s k in
     push s a

  | PUSH x ->
     push s x

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

(** In given state [s], run the program. *)
let run s =
  while s.pc < Array.length s.code do
    exec s ;
    s.pc <- s.pc + 1
  done
