(*
   Monkey Lang

   Grammar (BNF)

   statement ::= let-statement | return-statement | expression-statement
   let-statement ::= "let" identifier "=" expression ';'
   identifier ::= char | char(chars | digits)
   expression ::= | prefix-expression
                  | integer-literal
                  | boolean
                  | group-expression
                  | if-expression
                  | function-literal
                  | string-literal
                  | array-literal
                  | hash-literal
                  | identifier
                  | infix-expression
   prefix-expression ::= | '!' expression
                         | '-' expression
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
   call-expression ::= expression '(' expression-list ')'
   index-expression ::= expression '[' expression ']'
   return-statement ::= 'return' expression ';'
   expression-statement ::= expression ';'
*)

open Angstrom

module Ast = struct
  type prefix_expression =
    | Bang of expression
    | Negation of expression

  and infix_expression =
    | Add of expression * expression
    | Sub of expression * expression
    | Mul of expression * expression
    | Div of expression * expression
    | Eq of expression * expression
    | Neq of expression * expression
    | Lt of expression * expression
    | Gt of expression * expression
    | Call of expression * expression list
    | Index of expression * expression

  and expression =
    | Prefix of prefix_expression
    | Infix of infix_expression
    | Integer of int
    | String of string
    | Bool of bool
    | Group of expression
    | Array of expression list
    | Hash of (expression * expression) list
    | Function of string * string list * block_statement
    | If of expression * block_statement
    | IfElse of expression * block_statement * block_statement
    | Identifier of string

  and block_statement = Block of statement list

  and statement =
    | LetStatement of string * expression
    | ReturnStatement of expression
    | ExpressionStatement of expression

  and programm = Program of statement list

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

  (* tokens *)
  let semicolon = token (char ';')

  (* *)
  let rec stmt () =
    (* <identifier> Parser *)
    let ident =
      peek_char_fail
      >>= (function
             | c when is_letter c ->
               identifier >>= fun identifier -> return (Identifier identifier)
             | _ -> fail "Identifier need to start with a character")
      |> token
    in
    (* <digits> Parser *)
    let digits =
      take_while1 is_digit
      >>= (fun digit -> return (Integer (int_of_string digit)))
      |> token
    in
    (* Expression *)
    let rec exp () =
      (* Prefix Expressions *)
      (* <!><exp> Parser *)
      let bang_exp = char_sym '!' >>= (fun _ -> exp ()) >>= fun v -> return (Bang v) in
      (* <-><exp> Parser *)
      let neg_exp = char_sym '-' >>= (fun _ -> exp ()) >>= fun v -> return (Negation v) in
      (* <true>|<false> Parser *)
      let boolean =
        symbol "true" <|> symbol "false" >>= fun b -> return (Bool (bool_of_string b))
      in
      (* '('<exp>')' Parser *)
      let grouping_exp =
        char_sym '(' >>= fun _ -> exp () <* char_sym ')' >>= fun v -> return (Group v)
      in
      (* '{'<exp>'}' Parser *)
      let block_stmt =
        char_sym '{'
        >>= fun _ -> many (stmt ()) <* char_sym '}' >>= fun stmts -> return (Block stmts)
      in
      (* "if"'('<exp>')'<block>"else"<block> | "if"'('<exp>')'<block> Parser *)
      let if_exp =
        symbol "if" *> char_sym '('
        >>= (fun _ -> exp ())
        <* char_sym ')'
        >>= (fun condition ->
              block_stmt
              >>= fun stmts ->
              symbol "else" *> block_stmt
              >>= fun else_block_stmt ->
              return (IfElse (condition, stmts, else_block_stmt)))
        <|> (symbol "if" *> char_sym '('
             >>= (fun _ -> exp ())
             <* char_sym ')'
             >>= fun condition ->
             block_stmt >>= fun stmts -> return (If (condition, stmts)))
      in
      (* <exp>|<exp>','<fn-params>  Parser *)
      let fn_params =
        peek_char_fail >>= fun _ -> sep_by (char_sym ',') identifier |> token
      in
      (* "fn"'('<fn-params>')'<block> Parser *)
      let fn_literal =
        symbol "fn" *> identifier
        >>= fun ident ->
        char_sym '('
        >>= (fun _ -> fn_params)
        <* char_sym ')'
        >>= fun params ->
        block_stmt >>= fun stmts -> return (Function (ident, params, stmts))
      in
      (* '"'<utf8>'"' Parser *)
      let string_literal =
        char_sym '"' *> take_till (fun c -> Char.equal '"' c)
        >>= fun str -> return (String str) <* char_sym '"'
      in
      (* <exp>|<exp>,<exp_list> Parser *)
      let exp_list = peek_char >>= fun _ -> sep_by (char_sym ',') (exp ()) |> token in
      let array_literal =
        char_sym '['
        >>= fun _ -> exp_list <* char_sym ']' >>= fun exps -> return (Array exps)
      in
      let pair =
        peek_char_fail
        >>= fun _ ->
        exp ()
        >>= fun key ->
        char_sym ':' >>= fun _ -> exp () >>= fun value -> return (key, value) |> token
      in
      let pairs = sep_by (char_sym ',') pair in
      let hash_literal =
        char_sym '{'
        >>= fun _ -> pairs <* char_sym '}' >>= fun pairs -> return (Hash pairs)
      in
      let prefix_exp = neg_exp <|> bang_exp >>= fun exp -> return (Prefix exp) in
      (* Infix Expressions *)
      let addition left =
        char_sym '+' >>= fun _ -> exp () >>= fun right -> return (Add (left, right))
      in
      let substract left =
        char_sym '-' >>= fun _ -> exp () >>= fun right -> return (Sub (left, right))
      in
      let multiplication left =
        char_sym '*' >>= fun _ -> exp () >>= fun right -> return (Mul (left, right))
      in
      let division left =
        char_sym '/' >>= fun _ -> exp () >>= fun right -> return (Div (left, right))
      in
      let eq left =
        symbol "==" >>= fun _ -> exp () >>= fun right -> return (Eq (left, right))
      in
      let neq left =
        symbol "!=" >>= fun _ -> exp () >>= fun right -> return (Neq (left, right))
      in
      let lt left =
        char_sym '<' >>= fun _ -> exp () >>= fun right -> return (Lt (left, right))
      in
      let gt left =
        char_sym '>' >>= fun _ -> exp () >>= fun right -> return (Gt (left, right))
      in
      let call ident =
        char_sym '('
        >>= fun _ ->
        sep_by (char_sym ',') (exp ())
        <* char_sym ')'
        >>= fun args -> return (Call (ident, args))
      in
      let index ident =
        char_sym '['
        >>= fun _ -> exp () <* char_sym ']' >>= fun idx -> return (Index (ident, idx))
      in
      let infix_exp left =
        addition left
        <|> substract left
        <|> multiplication left
        <|> division left
        <|> eq left
        <|> neq left
        <|> lt left
        <|> gt left
        <|> call left
        <|> index left
        >>= fun exp -> return (Infix exp)
      in
      (* *)
      prefix_exp
      <|> digits
      <|> boolean
      <|> grouping_exp
      <|> if_exp
      <|> fn_literal
      <|> string_literal
      <|> array_literal
      <|> hash_literal
      <|> ident
      >>= fun left ->
      ws
      *> (peek_char
          >>= function
          | None -> fail ""
          | Some c ->
            (match c with
             | ';' | ')' | ',' | ']' | '}' | ':' -> return left
             | _ -> infix_exp left))
    in
    (* Statements *)
    let let_stmt =
      symbol "let"
      *> (identifier
          >>= fun ident ->
          char_sym '='
          >>= fun _ -> exp () >>= fun value -> return (LetStatement (ident, value)))
      <* semicolon
    in
    let return_stmt =
      symbol "return"
      >>= (fun _ -> exp ())
      <* char_sym ';'
      >>= fun value -> return (ReturnStatement value)
    in
    let exp_stmt =
      peek_char_fail
      >>= (fun _ -> exp ())
      <* char_sym ';'
      >>= fun exp -> return (ExpressionStatement exp)
    in
    let_stmt <|> return_stmt <|> exp_stmt
  ;;

  let stmts = many (stmt ()) >>= fun ls -> return (Program ls)
end

module Evaluator = struct
  module Store = Map.Make (String)

  type env =
    | Environment of obj Store.t * env
    | Nil

  and obj =
    | Null
    | Return of obj
    | Error of string
    | Integer of int
    | Array of obj list
    | Hash of (obj * obj) list
    | Fn of (obj list -> obj)
    | Function of string * string list * Ast.block_statement * env
    | String of string
    | True
    | False

  module Environment = struct
    let empty = Environment (Store.empty, Nil)
    let new_enclosing outenv : env = Environment (Store.empty, outenv)

    let set key value = function
      | Environment (store, outer) -> Environment (Store.add key value store, outer)
      | Nil -> Nil
    ;;

    let rec get key t =
      match t with
      | Nil -> None
      | Environment (store, outer) ->
        (match Store.find key store with
         | exception Not_found -> get key outer
         | value -> Some value)
    ;;
  end

  module Object = struct
    let rec pp_list fmt_a fmt = function
      | [] -> Format.fprintf fmt ""
      | a :: [] -> Format.fprintf fmt "%a" fmt_a a
      | a :: rest -> Format.fprintf fmt "%a, %a" fmt_a a (pp_list fmt_a) rest
    ;;

    let rec pp fmt = function
      | Null -> Format.fprintf fmt "Null"
      | Return t -> Format.fprintf fmt "%a" pp t
      | Error msg -> Format.fprintf fmt "Error: %s" msg
      | Integer i -> Format.fprintf fmt "%d" i
      | Array l -> Format.fprintf fmt "[%a]" (pp_list pp) l
      | Hash h -> Format.fprintf fmt "{%a}" (pp_list pp_pair) h
      | Fn _ -> Format.fprintf fmt "builtin function"
      | String s -> Format.fprintf fmt "%s" s
      | True -> Format.fprintf fmt "true"
      | False -> Format.fprintf fmt "false"
      | Function (name, _, _, _) -> Format.fprintf fmt "fn %s(..){..}" name

    and pp_pair fmt (key, value) = Format.fprintf fmt "%a: %a" pp key pp value

    let rec equal t1 t2 =
      match t1, t2 with
      | Null, Null -> true
      | Return o1, Return o2 -> equal o1 o2
      | Error m1, Error m2 -> String.equal m1 m2
      | Integer i1, Integer i2 -> Int.equal i1 i2
      | Array l1, Array l2 -> List.equal equal l1 l2
      | Hash h1, Hash h2 ->
        List.equal (fun (k1, v1) (k2, v2) -> equal k1 k2 && equal v1 v2) h1 h2
      | String s1, String s2 -> String.equal s1 s2
      | True, True | False, False -> true
      | Function (n1, _, _, _), Function (n2, _, _, _) -> String.equal n1 n2
      | Fn _, Fn _ -> false
      | _, _ -> false
    ;;
  end

  let built_ins = function
    | "len" ->
      Some
        (Fn
           (function
             | x :: [] ->
               (match x with
                | String s -> Integer (String.length s)
                | Array a -> Integer (List.length a)
                | _ -> Error "len not supported")
             | _ -> Error "wrong number of arguments, expecte 1"))
    | "first" ->
      Some
        (Fn
           (function
             | x :: [] ->
               (match x with
                | Array a -> if List.length a > 0 then List.hd a else Null
                | _ -> Error "argument to first must be an array")
             | _ -> Error "wrong number of arguments, expect 1"))
    | "last" ->
      Some
        (Fn
           (function
             | x :: [] ->
               (match x with
                | Array a ->
                  if List.length a > 0 then List.nth a (List.length a - 1) else Null
                | _ -> Error "argument to last must be an array")
             | _ -> Error "wrong number of arguments, expected 1"))
    | "rest" ->
      Some
        (Fn
           (function
             | x :: [] ->
               (match x with
                | Array a -> if List.length a > 0 then Array (List.tl a) else Null
                | _ -> Error "argument to rest must be an array")
             | _ -> Error "wrong number of arguments, expected 1"))
    | "push" ->
      Some
        (Fn
           (function
             | [ x; y ] ->
               (match x, y with
                | Array a, t -> if List.length a > 0 then Array (t :: a) else Null
                | _ -> Error "argument to push must be an array and any object")
             | _ -> Error "wrong number of arguments, expected 2"))
    | "puts" ->
      Some
        (Fn
           (fun args ->
             List.fold_left
               (fun acc a ->
                 Format.printf "%a\n" Object.pp a;
                 acc)
               Null
               args))
    | _ -> None
  ;;

  let eval (Ast.Program stmts) =
    let rec eval_programm ?(res = Null) env : Ast.statement list -> obj =
      let rec eval_stmt env : Ast.statement -> env * obj =
        let eval_block env (Ast.Block stmts) =
          let rec eval_statments ?(res = Null) env = function
            | [] -> env, res
            | stmt :: stmts ->
              let env, result = eval_stmt env stmt in
              (match result with
               | Error _ | Return _ -> env, result
               | _ -> eval_statments ~res:result env stmts)
          in
          eval_statments env stmts
        in
        let rec eval_exp env ast : env * obj =
          let rec eval_expressions ?(res = []) env = function
            | [] -> env, res
            | e :: es ->
              let env, result = eval_exp env e in
              (match result with
               | Error _ -> env, [ result ]
               | _ -> eval_expressions ~res:(result :: res) env es)
          in
          let eval_identifier env identifier =
            match Environment.get identifier env with
            | Some value -> env, value
            | None ->
              (match built_ins identifier with
               | Some fn -> env, fn
               | None -> env, Error ("unknown identifier: " ^ identifier))
          in
          let eval_fn env ident params stmts =
            env, Function (ident, params, stmts, env)
          in
          let eval_array env elems =
            let env, elems = eval_expressions env elems in
            match elems with
            | (Error _ as err) :: [] -> env, err
            | _ -> env, Array elems
          in
          let is_truthy = function
            | Null -> false
            | True -> true
            | False -> false
            | _ -> true
          in
          let eval_if env condition if_block =
            let env, conditon = eval_exp env condition in
            match conditon with
            | Error _ -> env, conditon
            | _ ->
              (match is_truthy conditon with
               | true -> eval_block env if_block
               | false -> env, Null)
          in
          let eval_if_else env condition if_block else_block =
            let env, conditon = eval_exp env condition in
            match conditon with
            | Error _ -> env, conditon
            | _ ->
              (match is_truthy conditon with
               | true -> eval_block env if_block
               | false -> eval_block env else_block)
          in
          let bool_to_bool_obj env b =
            match b with
            | true -> env, True
            | false -> env, False
          in
          let eval_bang env right =
            let env, right = eval_exp env right in
            match right with
            | Error _ -> env, right
            | True -> env, False
            | False -> env, True
            | Null -> env, True
            | _ -> env, False
          in
          let eval_negation env right =
            let env, right = eval_exp env right in
            match right with
            | Error _ -> env, right
            | Integer int -> env, Integer (Int.neg int)
            | _ -> env, Error "Unkown operator -"
          in
          let eval_addition env left right =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, Integer (i1 + i2)
            | String s1, String s2 -> env, String (s1 ^ s2)
            | _, _ ->
              env, Error "Type missmatch with '+' types on both side need to be the same"
          in
          let eval_substration env left right =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, Integer (i1 - i2)
            | _, _ -> env, Error "Type missmatch with '-' only ingeger are supported"
          in
          let eval_multiplication env left right =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, Integer (i1 * i2)
            | _, _ -> env, Error "Type missmatch with '*' only ingeger are supported"
          in
          let eval_devision env left right =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, Integer (i1 / i2)
            | _, _ -> env, Error "Type missmatch with '/' only ingeger are supported"
          in
          let eval_lt env right left =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, if i1 < i2 then True else False
            | _, _ -> env, Error "Type missmatch with '<' only ingeger can be compared"
          in
          let eval_gt env right left =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, if i1 > i2 then True else False
            | _, _ -> env, Error "Type missmatch with '<' only ingeger can be compared"
          in
          let eval_eq env right left =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, if i1 = i2 then True else False
            | String s1, String s2 -> env, if String.equal s1 s2 then True else False
            | True, True | False, False -> env, True
            | False, True | True, False -> env, False
            | _, _ ->
              ( env
              , Error
                  "Type missmatch with '==' only ingeger, boolean and string can be \
                   compared" )
          in
          let eval_neq env right left =
            let env, left = eval_exp env left in
            let env, right = eval_exp env right in
            match left, right with
            | (Error _ as err), _ | _, (Error _ as err) -> env, err
            | Integer i1, Integer i2 -> env, if i1 != i2 then True else False
            | String s1, String s2 ->
              env, if String.equal s1 s2 |> not then True else False
            | True, False | False, True -> env, True
            | False, False | True, True -> env, False
            | _, _ ->
              ( env
              , Error
                  "Type missmatch with '!=' only ingeger, boolean and string can be \
                   compared" )
          in
          let eval_call env ident args =
            let rec extend_function_env env args params =
              match args, params with
              | _, [] -> env
              | a :: aa, p :: ps -> extend_function_env (Environment.set p a env) aa ps
              | [], p :: ps -> extend_function_env (Environment.set p Null env) [] ps
            in
            let env, ident = eval_exp env ident in
            match ident with
            | Error _ -> env, ident
            | _ ->
              let env, args = eval_expressions env args in
              (match ident, args with
               | _, (Error _ as err) :: [] -> env, err
               | Function (_, parameter, blocks, fn_env), args ->
                 let new_env = Environment.new_enclosing fn_env in
                 let new_env = extend_function_env new_env args parameter in
                 let env, evaluated = eval_block new_env blocks in
                 (match evaluated with
                  | Return obj -> env, obj
                  | _ -> env, evaluated)
               | Fn fn, args -> env, fn args
               | _ -> env, Error "unknown function")
          in
          let eval_infix env ident index =
            let env, ident = eval_exp env ident in
            let env, index = eval_exp env index in
            match ident, index with
            | Array arr, Integer i ->
              if i > List.length arr - 1 then env, Null else env, List.nth arr i
            | Hash hash, key ->
              ( env
              , List.find_opt (fun (k, _) -> Object.equal key k) hash
                |> Option.map (fun (_, value) -> value)
                |> Option.value ~default:Null )
            | _, _ -> env, Error "invalid hash"
          in
          let eval_hash env pairs =
            let pairs =
              List.map (fun (key, value) -> eval_exp env key, eval_exp env value) pairs
            in
            let res =
              List.fold_left
                (fun acc ((_, key), (_, value)) ->
                  match acc with
                  | Result.Error _ as err -> err
                  | Result.Ok m ->
                    (match key, value with
                     | Error _, _ -> Result.Error key
                     | _, Error _ -> Result.Error value
                     | _, _ -> Result.Ok ((key, value) :: m)))
                (Result.Ok [])
                pairs
            in
            match res with
            | Result.Error err -> env, err
            | Result.Ok l -> env, Hash l
          in
          let eval_prefix env = function
            | Ast.Bang exp -> eval_bang env exp
            | Ast.Negation exp -> eval_negation env exp
          in
          let eval_infix env = function
            | Ast.Add (left, right) -> eval_addition env left right
            | Ast.Sub (left, right) -> eval_substration env left right
            | Ast.Mul (left, right) -> eval_multiplication env left right
            | Ast.Div (left, right) -> eval_devision env left right
            | Ast.Lt (left, right) -> eval_lt env right left
            | Ast.Gt (left, right) -> eval_gt env right left
            | Ast.Eq (left, right) -> eval_eq env right left
            | Ast.Neq (left, right) -> eval_neq env right left
            | Ast.Call (ident, args) -> eval_call env ident args
            | Ast.Index (ident, index) -> eval_infix env ident index
          in
          match ast with
          | Ast.Prefix prefix -> eval_prefix env prefix
          | Ast.Infix infix -> eval_infix env infix
          | Ast.Identifier identifier -> eval_identifier env identifier
          | Ast.Function (ident, params, stmts) -> eval_fn env ident params stmts
          | Ast.Array elems -> eval_array env elems
          | Ast.If (condition, if_block) -> eval_if env condition if_block
          | Ast.IfElse (condition, if_block, else_block) ->
            eval_if_else env condition if_block else_block
          | Ast.Integer int -> env, Integer int
          | Ast.Bool b -> bool_to_bool_obj env b
          | Ast.String str -> env, String str
          | Ast.Group exp ->
            env, Error "" (* Should not be part of as but rather turn in to precedence *)
          | Ast.Hash pairs -> eval_hash env pairs
        in
        let eval_let env ident value =
          let env, result = eval_exp env value in
          match result with
          | Error _ -> env, result
          | _ -> Environment.set ident result env, Null
        in
        let eval_return env stmt =
          let env, result = eval_exp env stmt in
          match result with
          | Error _ -> env, result
          | _ -> env, Return result
        in
        function
        | Ast.LetStatement (ident, value) -> eval_let env ident value
        | Ast.ReturnStatement stmt -> eval_return env stmt
        | Ast.ExpressionStatement stmt -> eval_exp env stmt
      in
      function
      | [] -> res
      | stmt :: stmts ->
        let env, result = eval_stmt env stmt in
        (match result with
         | Return value -> value
         | Error _ -> result
         | _ -> eval_programm ~res:result env stmts)
    in
    eval_programm Environment.empty stmts
  ;;
end

(* Eval *)

let eval str = parse_string ~consume:Prefix Ast.stmts str
