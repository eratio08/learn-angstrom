open Monkey

let rec pp_list ?(sep = "; ") fmt_a fmt = function
  | [] -> Format.fprintf fmt ""
  | x :: [] -> Format.fprintf fmt "%a" fmt_a x
  | x :: xs -> Format.fprintf fmt "%a%s%a" fmt_a x sep (pp_list ~sep fmt_a) xs
;;

open Ast

let rec pp_stmt fmt =
  let rec pp_exp fmt =
    let pp_prefix_exp fmt = function
      | Bang exp -> Format.fprintf fmt "!%a" pp_exp exp
      | Negation exp -> Format.fprintf fmt "-%a" pp_exp exp
    in
    let pp_block fmt (Block stmts) =
      Format.fprintf fmt "{ %a }" (pp_list ~sep:"" pp_stmt) stmts
    in
    let pp_pair fmt (exp1, exp2) = Format.fprintf fmt "%a: %a" pp_exp exp1 pp_exp exp2 in
    let pp_infix fmt = function
      | Add (left, right) -> Format.fprintf fmt "%a + %a" pp_exp left pp_exp right
      | Sub (left, right) -> Format.fprintf fmt "%a - %a" pp_exp left pp_exp right
      | Mul (left, right) -> Format.fprintf fmt "%a * %a" pp_exp left pp_exp right
      | Div (left, right) -> Format.fprintf fmt "%a / %a" pp_exp left pp_exp right
      | Eq (left, right) -> Format.fprintf fmt "%a == %a" pp_exp left pp_exp right
      | Neq (left, right) -> Format.fprintf fmt "%a != %a" pp_exp left pp_exp right
      | Lt (left, right) -> Format.fprintf fmt "%a < %a" pp_exp left pp_exp right
      | Gt (left, right) -> Format.fprintf fmt "%a > %a" pp_exp left pp_exp right
      | Call (ident, params) ->
        Format.fprintf fmt "%a(%a)" pp_exp ident (pp_list ~sep:", " pp_exp) params
      | Index (ident, idx) -> Format.fprintf fmt "%a[%a]" pp_exp ident pp_exp idx
    in
    function
    | Prefix p_exp -> Format.fprintf fmt "%a" pp_prefix_exp p_exp
    | String s -> Format.fprintf fmt "\"%s\"" s
    | Integer d -> Format.fprintf fmt "%d" d
    | Bool b -> Format.fprintf fmt "%b" b
    | Group exp -> Format.fprintf fmt "( %a )" pp_exp exp
    | If (cond, block) -> Format.fprintf fmt "if( %a ) %a" pp_exp cond pp_block block
    | IfElse (cond, block, else_block) ->
      Format.fprintf
        fmt
        "if( %a ) %a else %a"
        pp_exp
        cond
        pp_block
        block
        pp_block
        else_block
    | Function (ident, params, stmts) ->
      Format.fprintf
        fmt
        "fn %s( %a ) { %a }"
        ident
        (pp_list (fun fmt t -> Format.fprintf fmt "%s" t))
        params
        pp_block
        stmts
    | Infix exp -> Format.fprintf fmt "%a" pp_infix exp
    | Array exps -> Format.fprintf fmt "[ %a ]" (pp_list pp_exp) exps
    | Hash pairs -> Format.fprintf fmt "{ %a }" (pp_list ~sep:", " pp_pair) pairs
    | Identifier ident -> Format.fprintf fmt "%s" ident
  in
  function
  | LetStatement (ident, exp) -> Format.fprintf fmt "`let %s = %a;`" ident pp_exp exp
  | ReturnStatement stmt -> Format.fprintf fmt "return %a;" pp_exp stmt
  | ExpressionStatement exp -> Format.fprintf fmt "`%a;`" pp_exp exp
;;

let pp fmt (Program stmts) = Format.fprintf fmt "%a" (pp_list ~sep:"\n" pp_stmt) stmts

let () =
  eval
    "let some_name = -1;\n\
     let digits = 12;\n\
     let some_other = (!true);\n\
     if(true){ true; false;};\n\
     if(true){ true; false;} else { return true; };\n\
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
  |> function
  | Error err -> Format.printf "Error: %s!" err
  | Ok ast -> Format.printf "Eval: %a@." pp ast
;;

let () =
  eval "let x = 2;\nx + x * x;\nif (x != 2) { 1; } else { return \"bla\"; };\n"
  |> function
  | Error err -> Format.printf "Error: %s!" err
  | Ok program ->
    program |> Evaluator.eval |> fun obj -> Format.printf "$ %a" Evaluator.Object.pp obj
;;
