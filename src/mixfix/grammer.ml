module type T =
sig
  
  type state
  
  val add_operator : state -> Syntax.operator -> state

  val initial_state : unit -> state

  val parse_presyntax : state -> (Syntax.name * Syntax.htype) list -> Presyntax.expr -> Syntax.expr

end
