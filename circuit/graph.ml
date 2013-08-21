(** Generate a circuit from an expression or a function definition. *)

(* PROBLEM: We can't tell by looking at the graph what the order of arguments
   is in a function application. *)

module S = Syntax

type name = string

type id = int

type node = string

(* A circuit is a list of nodes and a list of edges. Each node has an id an a label,
   while an edge is a connection between two nodes, given by ids. *)
type circuit = { nodes : (id * node) list ; edges : (id * id) list }

let add_node c k n = { c with nodes = (k,n) :: c.nodes }

let add_edge c j k = { c with edges = (j,k) :: c.edges }

let dot_of_circuit ?(name="circuit") c =
  "digraph " ^ name ^ " {\n  " ^
    String.concat ";\n  " (List.map (fun (k,x) -> string_of_int k ^ " [label = \"" ^ x ^ "\"]") c.nodes) ^
    ";\n  " ^
    String.concat ";\n  " (List.map (fun (m,n) -> string_of_int m ^ " -> " ^ string_of_int n) c.edges) ^
  ";\n}"

let count =
  let k = ref 0 in
  fun () -> (incr k; !k)

let circuit_of_expr xs e =
  let rec eval env c = function
    | S.Var x -> c, List.assoc x env
    | S.Int m ->
        let k = count () in
          add_node c k (string_of_int m), k
    | S.Bool b ->
        let k = count () in
          add_node c k (string_of_bool b), k
    | S.String s ->
        let k = count () in
          add_node c k s, k
    | S.Cond (e1, e2, e3) ->
      let c, k1 = eval env c e1 in
      let c, k2 = eval env c e2 in
      let c, k3 = eval env c e3 in
      let k = count () in
      let c = add_node c k "if" in
      let c = add_edge c k1 k in
      let c = add_edge c k2 k in
      let c = add_edge c k3 k in
      c, k
    | S.Apply (f, lst) ->
        let k = count () in
        let c = add_node c k f in
        let c =
          List.fold_right (fun e c -> let c, n = eval env c e in add_edge c n k) lst c
        in
          c, k          
    | S.Let (x, e1, e2) ->
        let k = count () in
        let c = add_node c k x in
        let c, m = eval env c e1 in
        let c = add_edge c m k in
          eval ((x,k)::env) c e2
    | S.Copy (e1, x, y, e2) ->
        let l = count () in
        let m = count () in
        let n = count () in
        let c, k = eval env c e1 in
        let c = add_node c l "*" in
        let c = add_edge c k l in
        let c = add_node c m x in
        let c = add_edge c l m in
        let c = add_node c n y in
        let c = add_edge c l n in
          eval ((x,m)::(y,n)::env) c e2
  in
  let (env, c) =
    List.fold_right
      (fun x (env,c) -> let k = count () in (x,k)::env, add_node c k x)
      xs
      ([], {nodes=[]; edges=[]})
  in
    fst (eval env c e)
