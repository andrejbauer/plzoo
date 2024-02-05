

(** As per reference in the paper *)
type name_part = Presyntax.expr

type 'a parse_result = ('a * name_part list)

type 'a parser = name_part list -> 'a parse_result list

(*
Notes:
Sm probal Error | Result [] -> Vseeno če samo [], kjer prazen seznam pomeni error.
Sm probal s flatmap = List.flatten List.map ...  -> obstaja List.concat_map, manj teksta :)
Sm probal samo list monad-ish, ampak potem 'spodaj' ostane še ('a parse_result), in je vseeno če oboje v eno monado damo.
*)

let fail :'a parser = fun _ -> []
;;

let return (x:'a) : 'a parser = fun s -> [(x, s)]
;;

let (>>=) (x: 'a parser) (f:'a -> 'b parser): 'b parser= 
  fun s ->
    let g (x, rest) = (f x) rest in
    List.concat_map g (x s)
;;

let (||) (p1:'a parser) (p2:'a parser):'a parser = 
  fun inp ->  (p1 inp) @ (p2 inp)
;;

(** Application ? *)
(** Applicatove functor operator** <- več o tem *)


let star (fun_parser: ('a -> 'b) parser) (x_parser: 'a parser): 'b parser = 
  fun_parser >>= (fun f -> x_parser >>= (fun x -> return (f x)))
;;

let dolla (f: 'a->'b) (x_parser: 'a parser): 'b parser = 
  x_parser >>= (fun x -> return (f x))
;;

let iter (p:'a parser): 'a list parser = 
  let rec iter' p =
    p >>= (fun x -> iter' p >>= (fun xs -> return (x::xs)))
  in
  iter' p
;;

(* Tuki nekaj ne štima, mogoče pa vseeno rabimo explicit Error ? *)
(* Zna biti da je option čisto odveč. Ne smemo kar vrnit empty seznama, če so še tokeni !! *)
let between (p: 'a parser) (tokens: name_part list) =
  (** between' takes 'a parser, tokens: name_part list, s: name_part list *)
  let rec between' p tokens s = 
    match (tokens, s) with 
    | ([], _) ->  return (Some []) (* Edge case: We ran out of tokens, everything is ok *)
    | (t::ts, s1::s2) when t = s1 -> 
      p >>= (fun x -> 
        (between' p ts s2 >>= (fun xs -> 
          (* Use of Option.bind for x::xs *)
          return (Option.bind xs (fun xs -> 
              Some (x::xs)
            ))
          ))
      )
    | _ -> return (None) (* Error case, one of the tokens didn't match *)
  in
  (* TODO: kaj pomeni ~defauolt ! *)
  fun s -> (between' p tokens s) >>= (fun xs -> return (Option.value ~default:[] xs))
  ;;