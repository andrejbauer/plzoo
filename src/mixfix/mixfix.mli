(* The type describing all known mixfix operators *)
type t

val empty : t (* initial mixfix state *)

val toplevel_cmd : t * Input.toplevel_cmd -> Syntax.toplevel_cmd
