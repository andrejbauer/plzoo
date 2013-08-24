(** Error reporting. *)

(** Exception [Error (loc, err, msg)] indicates an error of type [err] with
    error message [msg], occurring at position [loc]. *)
exception Error of (Position.t * string * string)

(** Support for printing of errors at various levels of verbosity. *)

let verbosity = ref 2

(** Print a message at a given location [loc] of message type [msg_type] and
    verbosity level [v]. *)
let report ?(loc=Position.Nowhere) msg_type v =
  if v <= !verbosity then
    begin
      Format.eprintf "%s at %t:@\n@[" msg_type (Position.position loc) ;
      Format.kfprintf (fun ppf -> Format.fprintf ppf "@]@.") Format.err_formatter
    end
  else
    Format.ifprintf Format.err_formatter

(** Common message types. *)
let print (loc, err_type, msg) = report ~loc err_type 1 "%s" msg
let warning msg = report "Warning" 2 msg
let debug msg = report "Debug" 3 msg

(** [error loc err_type] raises an error of type [err_type]. The [kfprintf] magic allows
    one to write [msg] using a format string. *)
let trigger ~loc err_type =
  let k _ =
    let msg = Format.flush_str_formatter () in
      raise (Error (loc, err_type, msg))
  in
    Format.kfprintf k Format.str_formatter

(** Common error kinds. *)
let fatal ~loc msg = trigger ~loc "Fatal error" msg
let syntax ~loc msg = trigger ~loc "Syntax error" msg
let typing ~loc msg = trigger ~loc "Typing error" msg
let runtime ~loc msg = trigger ~loc "Runtime error" msg
let warning ~loc msg = trigger ~loc "Warning" msg
