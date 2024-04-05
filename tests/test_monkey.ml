open Monkey

let rec pp_list ?(sep = "; ") fmt_a fmt = function
  | [] -> Format.fprintf fmt ""
  | x :: [] -> Format.fprintf fmt "%a" fmt_a x
  | x :: xs -> Format.fprintf fmt "%a%s%a" fmt_a x sep (pp_list ~sep fmt_a) xs
;;

let rec pp fmt = function
  | `Let (`Identifier ident, exp) -> Format.fprintf fmt "`let %s = %a;`" ident pp exp
  | `String s -> Format.fprintf fmt "\"%s\"" s
  | `Int d -> Format.fprintf fmt "%d" d
  | `Bang exp -> Format.fprintf fmt "!%a" pp exp
  | `Bool b -> Format.fprintf fmt "%b" b
  | `Identifier ident -> Format.fprintf fmt "%s" ident
  | `Neg exp -> Format.fprintf fmt "-%a" pp exp
  | `Stmts ls -> Format.fprintf fmt "%a" (pp_list ~sep:"\n " pp) ls
  | `Error msg -> Format.fprintf fmt "Error %s" msg
  | `Group exp -> Format.fprintf fmt "( %a )" pp exp
  | `If (cond, stmts) ->
    Format.fprintf fmt "if( %a ) { %a }" pp cond (pp_list ~sep:"" pp) stmts
  | `ExpStmt exp -> Format.fprintf fmt "`%a;`" pp exp
  | `Fn (ident, params, stmts) ->
    Format.fprintf
      fmt
      "fn %a( %a ) { %a }"
      pp
      ident
      (pp_list pp)
      params
      (pp_list pp)
      stmts
  | `Array exps -> Format.fprintf fmt "[ %a ]" (pp_list pp) exps
  | `Hash pairs -> Format.fprintf fmt "{ %a }" (pp_list ~sep:", " pp) pairs
  | `Pair (key, value) -> Format.fprintf fmt "%a: %a" pp key pp value
  | `Add (left, right) -> Format.fprintf fmt "%a + %a" pp left pp right
  | `Sub (left, right) -> Format.fprintf fmt "%a - %a" pp left pp right
  | `Mult (left, right) -> Format.fprintf fmt "%a * %a" pp left pp right
  | `Div (left, right) -> Format.fprintf fmt "%a / %a" pp left pp right
  | `Eq (left, right) -> Format.fprintf fmt "%a == %a" pp left pp right
  | `Neq (left, right) -> Format.fprintf fmt "%a != %a" pp left pp right
  | `Lt (left, right) -> Format.fprintf fmt "%a < %a" pp left pp right
  | `Gt (left, right) -> Format.fprintf fmt "%a > %a" pp left pp right
  | `Call (ident, params) ->
    Format.fprintf fmt "%a(%a)" pp ident (pp_list ~sep:", " pp) params
  | `Index (ident, idx) -> Format.fprintf fmt "%a[%a]" pp ident pp idx
  | _ -> Format.printf "Unmatched"
;;

let () =
  eval
    "let some_name = -1;\n\
     let some_other = (!true);\n\
     if(true){ true; false;};\n\
     let x = some_name;\n\
     fn some_func(a, b) { true; };\n\
     let y = \"some string \";\n\
     let arr = [1,2,3,\"2\"];\n\
     let arr_empty = [];\n\
     let hash = { first : 1, second : \"abc\" };\n\
     let hash_empty = { };\n\
     let x = 1 + 2;\n\
     let x = 1 + 2 * 3 / 4 - 5;\n\
     let eq = (true == false) != true;\n\
     let lt = 1 < 2;\n\
     let gt = 3 > 2;\n\
     let x = ident();\n\
     let y = ident(1,2,b);\n\
     let idx = arr[2];\n"
  |> fun ast -> Format.printf "Eval: %a@." pp ast
;;
