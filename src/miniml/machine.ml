(** A simple abstract machine for executing programs compiled from
    MiniML or a similar purely functional language. *)


(** The abstract machine is built from frames environments and stacks.

    A frame is a list of machine instructions, usually representing
    the body of a function or a branch of conditional statement.

    An environment is a mapping from variable names to machine values.
    A machine value is an integer, a boolean value, or a closure. A
    closure represents a compiled function and is a triple
    [(x,frm,env)] where [x] is the name of the function argument,
    [frm] is the frame representing the function body, and [env] is
    the environment of variables that can be accessed by the function
    body.

    The state of the machine is described by a triple [(f,s,e)] where
    [f] is a stack of frames, [s] is a stack of machine values, and
    [e] is a stack of environments. At each step the machine executes
    the first instruction of the first frame from [f].
*)

(** The datatype of variable names. A more efficient implementation
    would use de Bruijn indices but we want to keep things simple. *)
type name = Syntax.name

(** Machine values. *)
type mvalue =
  | MInt of int                        (** Integer *)
  | MBool of bool                      (** Boolean value *)
  | MClosure of name * frame * environ (** Closure *)

(**
   There are two kinds of machine instructions.

   The first kind manipules tha stack of machine values. These are
   arithmetical operations, integer comparison, variable lookup,
   placing constants onto the stack, and closure formation.

   The second kind are the control instructions. These are branching
   instruction, execution of a closure, and popping of an environment.
*)

and instr =
  | IMult                           (** multiplication *)
  | IAdd                            (** addition *)
  | ISub                            (** subtraction *)
  | IEqual                          (** equality *)
  | ILess                           (** less than *)
  | IVar of name  		    (** push value of variable *)
  | IInt of int   		    (** push integer constant *)
  | IBool of bool 		    (** push boolean constant *)
  | IClosure of name * name * frame (** push closure *)
  | IBranch of frame * frame        (** branch *)
  | ICall                           (** execute a closure *)
  | IPopEnv                         (** pop environment *)

(** A frame is a list (stack) of instructions *)
and frame = instr list

(** An environment is an association list mapping names to values *)
and environ = (name * mvalue) list

(** A stack of machine values *)
and stack = mvalue list

(** Exception indicating a runtime error *)
exception Machine_error of string

(** Report a runtime error *)
let error msg = raise (Machine_error msg)

(** Convert a machine value to string *)
let string_of_mvalue = function
  | MInt k -> string_of_int k
  | MBool b -> string_of_bool b
  | MClosure _ -> "<fun>" (** Closures cannot be reasonably displayed *)

(** [lookup x envs] scans through the list of environments [envs] and
    returns the first value of variable [x] found. *)
let lookup x = function
  | env::_ -> (try List.assoc x env with Not_found -> error ("unknown " ^ x))
  | _ -> error ("unknown" ^ x)

(** Decompose a stack into top and rest. *)
let pop = function
  | [] -> error "empty stack"
  | v::s -> (v, s)

(** Pop a boolean value from a stack. *)
let pop_bool = function
  | MBool b :: s -> (b, s)
  | _ -> error "bool expected"

(** Pop a value and a closure from a stack. *)
let pop_app = function
  | v :: MClosure (x, f, e) :: s -> (x, f, e, v, s)
  | _ -> error "value and closure expected"

(** Arithmetical operations take their arguments from a stack and put the
    result onto the stack. We use auxiliary functions that do this. *)

(** Multiplication *)
let mult = function
  | (MInt x) :: (MInt y) :: s -> MInt (y * x) :: s
  | _ -> error "int and int expected in mult"

(** Addition *)
let add = function
  | (MInt x) :: (MInt y) :: s -> MInt (y + x) :: s
  | _ -> error "int and int expected in add"

(** Subtraction *)
let sub = function
  | (MInt x) :: (MInt y) :: s -> MInt (y - x) :: s
  | _ -> error "int and int expected in sub"

(** Equality *)
let equal = function
  | (MInt x) :: (MInt y) :: s -> MBool (y = x) :: s
  | _ -> error "int and int expected in equal"

(** Less than *)
let less = function
  | (MInt x) :: (MInt y) :: s -> MBool (y < x) :: s
  | _ -> error "int and int expected in less"

(** [exec instr frms stck envs] executes instruction [instr] in the
    given state [(frms, stck, envs)], where [frms] is a stack of frames,
    [stck] is a stack of machine values, and [envs] is a stack of
    environments. The return value is a new state. *)
let exec instr frms stck envs =
  match instr with
    (* Arithmetic *)
    | IMult  -> (frms, mult stck, envs)
    | IAdd   -> (frms, add stck, envs)
    | ISub   -> (frms, sub stck, envs)
    | IEqual -> (frms, equal stck, envs)
    | ILess  -> (frms, less stck, envs)
    (* Pushing values onto stack *)
    | IVar x  -> (frms, (lookup x envs) :: stck, envs)
    | IInt k  -> (frms, (MInt k) :: stck, envs)
    | IBool b -> (frms, (MBool b) :: stck, envs)
    | IClosure (f, x, frm) ->
	(match envs with
	     env :: _ ->
	       let rec c = MClosure (x, frm, (f,c) :: env) in
		 (frms, c :: stck, envs)
	   | [] -> error "no environment for a closure")
    (* Control instructions *)
    | IBranch (f1, f2) ->
	let (b, stck') = pop_bool stck in
	  ((if b then f1 else f2) :: frms, stck', envs)
    | ICall ->
	let (x, frm, env, v, stck') = pop_app stck in
	  (frm :: frms, stck', ((x,v) :: env) :: envs)
    | IPopEnv ->
	(match envs with
	     [] -> error "no environment to pop"
	   | _ :: envs' -> (frms, stck, envs'))

(** [run frm env] executes the frame [frm] in environment [env]. *)
let run frm env =
  let rec loop = function
    | ([], [v], _) -> v
    | ((i::is) :: frms, stck, envs) -> loop (exec i (is::frms) stck envs)
    | ([] :: frms, stck, envs) -> loop (frms, stck, envs)
    | _ -> error "illegal end of program"
  in
    loop ([frm], [], [env])
