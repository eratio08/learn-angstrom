open Angstrom

let parens p = char '(' *> p <* char ')'
let add = char '+' *> return ( + )
let sub = char '-' *> return ( - )
let mul = char '*' *> return ( * )
let div = char '/' *> return ( / )

let integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| int_of_string
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let expr : int t =
  fix (fun expr ->
    let factor = parens expr <|> integer in
    let term = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))
;;

let eval (str : string) : int =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> failwith msg
;;
