(** Source code locations. *)
type location

(** A datum tagged with a source code location *)
type 'a located = private { data : 'a ; loc : location }

(** Tag a datum with an (optional) location. *)
val locate : ?loc:location -> 'a -> 'a located

(** Convert a [Lexing.lexbuf] location to a [location] *)
val location_of_lex : Lexing.lexbuf -> location

(** [make_location p1 p2] creates a location which starts at [p1] and ends at [p2]. *)
val make_location : Lexing.position -> Lexing.position -> location

(** Print a location *)
val print_location : location -> Format.formatter -> unit

(** [error ~kind ~loc msg] raises an exception which is caught by the toplevel and
    prints the given message. *)
val error :
   ?kind:string -> ?loc:location -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Print miscellaneous information *)
val print_info : ('a, Format.formatter, unit, unit) format4 -> 'a

(** Print an expression, possibly placing parentheses around it. We always
    print things at a given "level" [at_level]. If the level exceeds the
    maximum allowed level [max_level] then the expression should be parenthesized.

    Let us consider an example. When printing nested applications, we should print [App
    (App (e1, e2), e3)] as ["e1 e2 e3"] and [App(e1, App(e2, e3))] as ["e1 (e2 e3)"]. So
    if we assign level 1 to applications, then during printing of [App (e1, e2)] we should
    print [e1] at [max_level] 1 and [e2] at [max_level] 0.
*)
val print_parens : ?max_level:int -> ?at_level:int ->
                   Format.formatter -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** The definition of a programming language *)
module type LANGUAGE =
  sig
    (** The name of the language (used for prompt) *)
    val name : string

    (** The type of top-level commands *)
    type command

    (** The runtime environment *)
    type environment

    (** Additional command-line options *)
    val options : (Arg.key * Arg.spec * Arg.doc) list

    (** The initial runtime environment *)
    val initial_environment : environment

    (** Given the interactive input so far, should we read more? *)
    val read_more : string -> bool

    (** A parser for parsing entire files *)
    val file_parser : (Lexing.lexbuf -> command list) option

    (** A parser for parsing one toplevel command *)
    val toplevel_parser : (Lexing.lexbuf -> command) option

    (** Execute a toplevel command in the given environment and
        return the new environment. *)
    val exec : environment -> command -> environment
  end

(** Create a language from its definition. *)
module Main : functor (L : LANGUAGE) ->
                      sig
                        (** The main program *)
                        val main : unit -> unit
                      end
