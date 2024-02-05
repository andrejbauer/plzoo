

(** Explicit Maybe *)
type name_part = Presyntax.expr

type 'a parse_result = Error | Result of ('a * name_part list) list

let proj = function
  | Error -> []
  | Result s -> s


type 'a parser = name_part list -> 'a parse_result

let fail :'a parser = fun _ -> Error

let (||) (p1:'a parser) (p2:'a parser):'a parser = 
  fun inp ->  let r1 = proj (p1 inp) in let r2 = proj (p2 inp) in
  Result (r1 @ r2)
;;

let star (fun_parser: ('a -> 'b) parser) (x_parser: 'a parser):'b parser = 
  fun inp ->
    match fun_parser inp with
    | Error -> Error
    | Result h_list -> 
      let rec loop = function
        | [] -> []
        | (f, rest)::t -> 
          let x_parse = x_parser rest in
          match x_parse with
          | Error -> loop t
          | Result h_list -> 
            List.map (fun (x, rest) -> (f x, rest)) h_list @ (loop t)
    in Result (loop h_list)
;;

let dolla (f: 'a->'b) (x_parser: 'a parser): 'b parser = 
  fun inp -> 
    match x_parser inp with
    | Error -> Error
    | Result h_list -> Result (List.map (fun (x, rest) -> (f x, rest)) h_list)
;;

let flatmap f l = List.flatten (List.map f l)
;;

(** Applies function on all outcomes. Useful for writing a function for one output and just applying it to all outcomes *)

let flatmap_result (f:('a * name_part list) -> 'a parse_result) (l:('a * name_part list) list) = 
  flatmap proj (List.map f l)
;;

(* 

type 'a nonempty =
  | First of 'a
  | (::) of ('a * 'a nonempty)
;;

*)


let iter (p:'a parser): 'a list parser = 
  fun s ->
  let rec f = fun s ->
  match p s with
    | Error -> [([], s)]
    | Result options ->
      let map (head, leftover) = (List.map (fun (tail, leftover) -> (head::tail, leftover)) (f leftover)) 
    in flatmap map options
  in 
  Result(f s)

let iterplus (p: 'a parser): 'a list parser =
  fun s ->
    match (iter p s) with
      | Error | Result [] -> Error
      | Result _ as result -> result
    ;;

let between (p: 'a parser) (tokens: string list): 'a list parser =
  fun s -> 
    match p s with
      | Error -> Error
      | Result