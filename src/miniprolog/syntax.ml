(** Abstract syntax. *)

(** Constants and atoms are strings starting with lower-case letters. *)
type constant = string
    
(** Variables are strings starting with upper-case letters, followed
    by a number which indicates an instance of the variable. Thus a
    variable instance is a pair [(x,n)] where [x] is a variable and [n] is
    an integer. When the proof search depth is [n] all variables that we
    need to use are renamed from [(x,0)] to [(x,n)]. This is necessary so
    that we do not use the same variable name in two different
    applications of the same assertion. *)
type variable = string * int

(** The datatype of terms. *)
type term =
  | Var of variable             (** Variable [X1], [Y0], [Z2], ... *)
  | Const of constant           (** Constant [a], [b], [c], ... *)
  | App of constant * term list (** Compound term [f(t_1,...,t_n)] *)

(** Atomic proposition [p(t_1, ..., t_n)] *)
type atom = constant * term list

(** A conjunction of atomic propositions [p_1, ..., p_n]. The empty
    list represents [true]. *)
type clause = atom list

(** An assertion [(a,b_1,...,b_n)] is a Horn formula [b_1 & ... & b_n => a]. *)
type assertion = atom * clause

(** An environment is a list of pairs [(x, e)] where [x] is a variable
    instance and [e] is a term. An environment represents the current
    values of variables. *)
type environment = (variable * term) list

(** A database is a list of assertions. It represents the current
    program. *)
type database = assertion list

(** Toplevel commands. *)    
type toplevel_cmd =
  | Assert of assertion  (** Assertion [a :- b_1, ..., b_n.] or [a.] *)
  | Goal of clause       (** Query [?- a] *)
      
(** [lookup env x] returns the value of variable instance [x] in
    environment [env]. It returns [Var x] if the variable does not occur
    in [env]. *)
let rec lookup env x =
  try List.assoc x env with Not_found -> Var x


(** [subst_term sub t] substitutes in term [t] values for variables,
    as specified by the associative list [s]. It substitutes
    repeatedly until the terms stops changing, so this is not the
    usual kind of substitution. It is what we need during
    unification. *)
let rec subst_term env = function
  | Var x as e ->
      (let e' = lookup env x in
	 if e = e' then e' else subst_term env e')
  | Const _ as e -> e
  | App (c, ls) -> App (c, List.map (subst_term env) ls)	


(** [string_of_term t] converts term [t] to its string represenation. *)
let rec string_of_term = function
  | Var (v, 0) -> v
  | Var (v, n) -> v ^ string_of_int n
  | Const c -> c
  | App (f, ls) -> f ^ "(" ^ (String.concat ", " (List.map string_of_term ls)) ^ ")"

(** [string_of_env env] converts environment [env] to its string
    representation. It only keeps instance variables at level 0, i.e.,
    those that appear in the toplevel goal. *)
let string_of_env env =
  match List.filter (fun ((_, n), _) -> n = 0) env with
    | [] -> "Yes"
    | env' -> String.concat "\n"
	(List.map
	   (fun ((x,n), e) ->
	      x ^ " = " ^ string_of_term (subst_term env e))
	   (List.rev env'))

(** [occurs x t] returns [true] when variable instance [x] appears in
    term [t]. *)
let rec occurs x = function
    Var y -> x = y
  | Const _ -> false
  | App (_, ts) -> List.exists (occurs x) ts
