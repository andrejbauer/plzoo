(** Error reporting. *)

(** Exception [Error (err, msg)] indicates an error of type [err] with
    error message [msg]. *)
exception Error of (Common.position * string * string)

(** [error ~loc err_type msg] raises an error of type [err_type], and a message [msg]. The
    [kfprintf] magic allows one to write [msg] using a format string. *)
let error ~loc err_type =
  let k _ =
    let msg = Format.flush_str_formatter () in
      raise (Error (loc, err_type, msg))
  in
    Format.kfprintf k Format.str_formatter

let fatal ~loc msg = error ~loc "Fatal error" msg
let syntax ~loc msg = error ~loc "Syntax error" msg
let typing ~loc msg = error ~loc "Typing error" msg
let runtime ~loc msg = error ~loc "Runtime error" msg
let exc ~loc msg = error ~loc "Exception" msg
