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

(* let quoted p = char '"' *> p <* char '"' *)

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
  let rec exp () =
    let bang_exp =
      char_sym '!' >>= (fun _ -> exp ()) >>= fun v -> return (`Bang v) >>? "bank_exp"
    in
    let minus_exp =
      char_sym '-' >>= (fun _ -> exp ()) >>= fun v -> return (`Minus v) >>? "minus_exp"
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
    let exp_list = peek_char >>= fun _ -> sep_by (char_sym ',') (exp ()) |> token in
    let array_literal =
      char_sym '['
      >>= fun _ -> exp_list <* char_sym ']' >>= fun exps -> return (`Array exps)
    in
    let pair =
      peek_char_fail
      >>= fun _ ->
      exp ()
      >>= fun key ->
      char_sym ':'
      >>= fun _ -> exp () >>= fun value -> return (`Pair (key, value)) |> token
    in
    let pairs = sep_by (char_sym ',') pair in
    let hash_literal =
      char_sym '{'
      >>= fun _ -> pairs <* char_sym '}' >>= fun pairs -> return (`Hash pairs)
    in
    let prefix_exp () =
      digits
      <|> boolean
      <|> minus_exp
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
    (* let infix_exp () = *)
    (*   debug_str "Infix"; *)
    (*   return `Todo *)
    (* in *)
    prefix_exp () (* <|> infix_exp () *) >>? "exp"
  in
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
  let_stmt <|> return_stmt <|> exp_stmt
;;

let stmts = many (stmt ()) >>= fun ls -> return (`Stmts ls)

(* Eval *)

let eval str =
  debug_str "eval";
  match parse_string ~consume:Prefix stmts str with
  | Ok v -> v
  | Error msg -> `Error msg
;;

let rec pp_list ?(sep = ";") fmt_a fmt = function
  | [] -> Format.fprintf fmt ""
  | x :: [] -> Format.fprintf fmt "%a" fmt_a x
  | x :: xs -> Format.fprintf fmt "%a%s %a" fmt_a x sep (pp_list fmt_a) xs
;;

let rec pp fmt = function
  | `Let (`Identifier ident, exp) -> Format.fprintf fmt "`let %s = %a;`" ident pp exp
  | `String s -> Format.fprintf fmt "\"%s\"" s
  | `Int d -> Format.fprintf fmt "%d" d
  | `Bang exp -> Format.fprintf fmt "!%a" pp exp
  | `Bool b -> Format.fprintf fmt "%b" b
  | `Identifier ident -> Format.fprintf fmt "%s" ident
  | `Minus exp -> Format.fprintf fmt "-%a" pp exp
  | `Stmts ls -> Format.fprintf fmt "%a" (pp_list pp) ls
  | `Error msg -> Format.fprintf fmt "Error %s" msg
  | `Group exp -> Format.fprintf fmt "( %a )" pp exp (* | ` *)
  | `If (cond, stmts) ->
    Format.fprintf fmt "if(%a) { %a }" pp cond (pp_list ~sep:"" pp) stmts
  | `ExpStmt exp -> Format.fprintf fmt "`%a;`" pp exp
  | `Fn (ident, params, stmts) ->
    Format.fprintf fmt "fn %a(%a) { %a }" pp ident (pp_list pp) params (pp_list pp) stmts
  | `Array exps -> Format.fprintf fmt "[ %a ]" (pp_list pp) exps
  | `Hash pairs -> Format.fprintf fmt "{ %a }" (pp_list ~sep:"," pp) pairs
  | `Pair (key, value) -> Format.fprintf fmt "%a: %a" pp key pp value
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
     let hash_empty = { };\n"
  |> fun ast -> Format.printf "Eval: %a@." pp ast
;;
