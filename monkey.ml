(*
   Monkey Lang

   Grammar (BNF)

   statement ::= let-statement | return-statement | expression-statement
   let-statement ::= "let" identifier "=" expression ';'
   identifier ::= ??
   expression ::= prefix-expression | infix-expression
   prefix-expression ::= | '!' expression
                         | '-' expression
                         | integer-literal
                         | boolean
                         | group-expression
                         | if-expression
                         | function-literal
                         | string-literal
                         | array-literal
                         | hash-literal
                         | identifier
   infix-expression ::= | expression '+' expression
                        | expression '-' expression
                        | expression '/' expression
                        | expression '*' expression
                        | expression "==" expression
                        | expression "!=" expression
                        | expression '<' expression
                        | expression '>' expression
                        | call-expression
                        | index-expression
   integer-literal ::= digit | digit integer-literal
   digit ::= '0' .. '9'
   boolean ::= "true" | "false"
   group-expression ::= '(' expression ')'
   if-expression ::=| "if" '(' expression ')' block-statement
                    | "if" '(' expression ')' block-statement "else" block-statement
   block-statement ::= '{' many statement '}'
   function-literal ::= "fn" identifier '(' function-parameter ')' block-statement
   function-parameter ::= | empty
                          | identifier
                          | identifier ',' function-parameter
   string-literal ::= '"' chars '"'
   chars ::= char | char chars
   char ::= 'a' .. 'z' | ''
   array-literal ::= '[' expression-list ']'
   expression-list ::= expression | expression ',' expression-list
   hash-literal ::= '{' pairs '}'
   pairs ::= pair | pair ',' pairs
   pair ::= expression ':' expression
   call-expression ::= identifier '(' expression-list ')'
   index-expression ::= '[' expression ']'
   return-statement ::= 'return' expression ';'
   expression-statement ::= expression ';'
*)

open Angstrom

(* predicates *)
let is_ws = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_allowed_signs = function
  | '_' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* parsers *)
(* helper *)
let ws = skip_while is_ws
let token t = t >>= fun a -> ws >>= fun _ -> return a
let char_sym c = token (char c)
let symbol s = token (string s)

let identifier =
  take_while1 (fun c -> is_letter c || is_digit c || is_allowed_signs c) <* ws
;;

(* Repeated application of parser [p] separated by left associative operation op. *)
(* let chain_l_1 p op = *)
(*   let rec rest a = op >>= (fun f -> p >>= fun b -> rest (f a b)) <|> return a in *)
(*   p >>= fun a -> rest a *)
(* ;; *)

(* tokens *)
let semicolon = token (char ';')
let debug fmt s = Format.printf "#Debug: %a@." fmt s
let debug_str s = debug Format.pp_print_string s

let ( >>? ) (t : 'a t) s : 'a t =
  t
  >>| fun a ->
  debug_str s;
  a
;;

(* = *)
let rec stmt () =
  let ident =
    peek_char_fail
    >>= (function
           | c when is_letter c ->
             identifier >>= fun identifier -> return (`Identifier identifier)
           | _ -> fail "Identifier need to start with a character")
    |> token
    >>? "ident"
  in
  let digits =
    take_while1 is_digit
    >>= (fun digit -> return (`Int (int_of_string digit)))
    |> token
    >>? "digits"
  in
  (* Expression *)
  let rec exp () =
    (* Prefix Expressions *)
    let bang_exp =
      char_sym '!' >>= (fun _ -> exp ()) >>= fun v -> return (`Bang v) >>? "bank_exp"
    in
    let neg_exp =
      char_sym '-' >>= (fun _ -> exp ()) >>= fun v -> return (`Neg v) >>? "minus_exp"
    in
    let boolean =
      symbol "true"
      <|> symbol "false"
      >>= fun b -> return (`Bool (bool_of_string b)) >>? "boolean_exp"
    in
    let grouping_exp =
      char_sym '('
      >>= fun _ -> exp () <* char_sym ')' >>= fun v -> return (`Group v) >>? "grouping"
    in
    let block_stmt =
      char_sym '{' >>= fun _ -> many (stmt ()) <* char_sym '}' >>? "block"
    in
    let if_exp =
      symbol "if" *> char_sym '('
      >>= (fun _ -> exp ())
      <* char_sym ')'
      >>= fun condition ->
      block_stmt >>= fun stmts -> return (`If (condition, stmts)) >>? "if_exp"
    in
    let fn_params =
      peek_char_fail >>= fun _ -> sep_by (char_sym ',') (exp ()) |> token >>? "fn_paramas"
    in
    let fn_literal =
      symbol "fn" *> ident
      >>= fun ident ->
      char_sym '('
      >>= (fun _ -> fn_params)
      <* char_sym ')'
      >>= fun params ->
      block_stmt >>= fun stmts -> return (`Fn (ident, params, stmts)) >>? "fn_literal"
    in
    let string_literal =
      char_sym '"' *> take_till (fun c -> Char.equal '"' c)
      >>= fun str -> return (`String str) <* char_sym '"'
    in
    let exp_list =
      peek_char >>= fun _ -> sep_by (char_sym ',') (exp ()) |> token >>? "exp_list"
    in
    let array_literal =
      char_sym '['
      >>= fun _ ->
      exp_list <* char_sym ']' >>= fun exps -> return (`Array exps) >>? "array_literal"
    in
    let pair =
      peek_char_fail
      >>= fun _ ->
      exp ()
      >>= fun key ->
      char_sym ':'
      >>= fun _ -> exp () >>= fun value -> return (`Pair (key, value)) |> token >>? "pair"
    in
    let pairs = sep_by (char_sym ',') pair >>? "pairs" in
    let hash_literal =
      char_sym '{'
      >>= fun _ ->
      pairs <* char_sym '}' >>= fun pairs -> return (`Hash pairs) >>? "hash_literal"
    in
    let prefix_exp =
      digits
      <|> boolean
      <|> neg_exp
      <|> bang_exp
      <|> grouping_exp
      <|> if_exp
      <|> fn_literal
      <|> string_literal
      <|> array_literal
      <|> hash_literal
      <|> ident
      >>? "prefix_exp"
    in
    (* Infix Expressions *)
    let addition left =
      char_sym '+' *> exp () >>= fun right -> return (`Add (left, right))
    in
    let substract left =
      char_sym '-' *> exp () >>= fun right -> return (`Sub (left, right))
    in
    let multiplication left =
      char_sym '*' *> exp () >>= fun right -> return (`Mult (left, right))
    in
    let division left =
      char_sym '/' *> exp () >>= fun right -> return (`Div (left, right))
    in
    let eq left = symbol "==" *> exp () >>= fun right -> return (`Eq (left, right)) in
    let neq left = symbol "!=" *> exp () >>= fun right -> return (`Neq (left, right)) in
    let lt left = char_sym '<' *> exp () >>= fun right -> return (`Lt (left, right)) in
    let gt left = char_sym '>' *> exp () >>= fun right -> return (`Gt (left, right)) in
    let call ident =
      char_sym '(' *> sep_by (char_sym ',') (exp ())
      <* char_sym ')'
      >>= fun args -> return (`Call (ident, args))
    in
    let index ident =
      char_sym '[' *> exp () <* char_sym ']' >>= fun idx -> return (`Index (ident, idx))
    in
    (* *)
    prefix_exp
    >>= fun left ->
    ws
    *> (peek_char
        >>= function
        | Some '+' -> addition left
        | Some '-' -> substract left
        | Some '*' -> multiplication left
        | Some '/' -> division left
        | Some '=' -> eq left
        | Some '!' -> neq left
        | Some '<' -> lt left
        | Some '>' -> gt left
        | Some '(' -> call left
        | Some '[' -> index left
        | Some ';' | Some ')' | Some ',' | Some ']' | Some '}' | Some ':' -> return left
        | _ -> fail "" >>? "infix fail")
    >>? "exp"
  in
  (* Statements *)
  let let_stmt =
    symbol "let"
    *> (ident
        >>= fun ident ->
        char_sym '=' >>= fun _ -> exp () >>= fun value -> return (`Let (ident, value)))
    <* semicolon
    >>? "let_stmt"
  in
  let return_stmt =
    symbol "return"
    >>= (fun _ -> exp ())
    >>= fun value -> return (`ReturnStmt value) >>? "return_stmt"
  in
  let exp_stmt =
    peek_char_fail
    >>= (fun _ -> exp ())
    <* char_sym ';'
    >>= fun exp -> return (`ExpStmt exp) >>? "exp_stmt"
  in
  let_stmt <|> return_stmt <|> exp_stmt >>? "Stmt done\n"
;;

let stmts = many (stmt ()) >>= fun ls -> return (`Stmts ls)

(* Eval *)

let eval str =
  debug_str "eval";
  match parse_string ~consume:Prefix stmts str with
  | Ok v -> v
  | Error msg -> `Error msg
;;

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
