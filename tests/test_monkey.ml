open Monkey
open Ast

type 'a test_case =
  { input : string
  ; expected : 'a
  }

type parser_test_case = programm test_case

let () =
  let suite : parser_test_case list =
    [ { input = "let x = 1;"; expected = Program [ LetStatement ("x", Integer 1) ] }
    ; { input = "let x = 1 + 1;"
      ; expected = Program [ LetStatement ("x", Add (Integer 1, Integer 1)) ]
      }
    ; { input = "let x = 1 * 1;"
      ; expected = Program [ LetStatement ("x", Mul (Integer 1, Integer 1)) ]
      }
    ; { input = "let x = -1;"
      ; expected = Program [ LetStatement ("x", Negation (Integer 1)) ]
      }
    ; { input = "let x = !true;"
      ; expected = Program [ LetStatement ("x", Bang (Bool true)) ]
      }
    ; { input = "let x = (2 + 2) * 2;"
      ; expected =
          Program [ LetStatement ("x", Mul (Add (Integer 2, Integer 2), Integer 2)) ]
      }
    ; { input = "let x = 2 + 2 * 2;"
      ; expected =
          Program [ LetStatement ("x", Add (Integer 2, Mul (Integer 2, Integer 2))) ]
      }
    ; { input = "let x = 2 != 3;"
      ; expected = Program [ LetStatement ("x", Neq (Integer 2, Integer 3)) ]
      }
    ; { input = "let x = 2 != 1 + 3 / 2 + 10;"
      ; expected =
          Program
            [ LetStatement
                ( "x"
                , Neq
                    ( Integer 2
                    , Add (Add (Integer 1, Div (Integer 3, Integer 2)), Integer 10) ) )
            ]
      }
    ; { input = "let x = 2 > 1;"
      ; expected = Program [ LetStatement ("x", Gt (Integer 2, Integer 1)) ]
      }
    ; { input = "let x = 2 > (1 + 2) * 5 == true;"
      ; expected =
          Program
            [ LetStatement
                ( "x"
                , Eq
                    ( Gt (Integer 2, Mul (Add (Integer 1, Integer 2), Integer 5))
                    , Bool true ) )
            ]
      }
    ; { input = "fn(a, b){ return a + b; };"
      ; expected =
          Program
            [ ExpressionStatement
                (Function
                   ( [ "a"; "b" ]
                   , Block [ ReturnStatement (Add (Identifier "a", Identifier "b")) ] ))
            ]
      }
    ; { input = "if ( 2 > x ) { return b == c; };"
      ; expected =
          Program
            [ ExpressionStatement
                (If
                   ( Gt (Integer 2, Identifier "x")
                   , Block [ ReturnStatement (Eq (Identifier "b", Identifier "c")) ] ))
            ]
      }
    ; { input = "if ( 2 > x ) { return b == c; } else { return x; };"
      ; expected =
          Program
            [ ExpressionStatement
                (IfElse
                   ( Gt (Integer 2, Identifier "x")
                   , Block [ ReturnStatement (Eq (Identifier "b", Identifier "c")) ]
                   , Block [ ReturnStatement (Identifier "x") ] ))
            ]
      }
    ; { input = "let x = add(2, 4);"
      ; expected =
          Program
            [ LetStatement ("x", Call (Identifier "add", [ Integer 2; Integer 4 ])) ]
      }
    ; { input = "let x = fn(c, d) { return true; };"
      ; expected =
          Program
            [ LetStatement
                ("x", Function ([ "c"; "d" ], Block [ ReturnStatement (Bool true) ]))
            ]
      }
    ; { input = "let x = { a: 2, 5: \"a\" };"
      ; expected =
          Program
            [ LetStatement ("x", Hash [ Identifier "a", Integer 2; Integer 5, String "a" ])
            ]
      }
    ; { input = "let x = [\"b\", \"a\"];"
      ; expected = Program [ LetStatement ("x", Array [ String "b"; String "a" ]) ]
      }
    ; { input = "index[2];"
      ; expected = Program [ ExpressionStatement (Index (Identifier "index", Integer 2)) ]
      }
    ; { input = "fn(x) { return x; };}"
      ; expected =
          Program
            [ ExpressionStatement
                (Function ([ "x" ], Block [ ReturnStatement (Identifier "x") ]))
            ]
      }
    ; { input = "fn (x) { if (x) { return 1; } else { return 2; };};"
      ; expected =
          Program
            [ ExpressionStatement
                (Function
                   ( [ "x" ]
                   , Block
                       [ ExpressionStatement
                           (IfElse
                              ( Identifier "x"
                              , Block [ ReturnStatement (Integer 1) ]
                              , Block [ ReturnStatement (Integer 2) ] ))
                       ] ))
            ]
      }
    ]
  in
  suite
  |> List.iter (fun case ->
    eval case.input
    |> Result.get_ok
    |> fun res ->
    if equal case.expected res
    then ignore ()
    else (
      let msg =
        Format.asprintf
          "#%s\nexpected: %a\ngot:      %a\n"
          case.input
          Ast.pp
          case.expected
          Ast.pp
          res
      in
      Format.eprintf "%s" msg;
      failwith "Test failed"))
;;

type interpreter_test_case = Evaluator.obj test_case

let () =
  let suite : interpreter_test_case list =
    [ { input = "let x = 2;"; expected = Evaluator.Null }
    ; { input = "let x = 2; x;"; expected = Evaluator.Integer 2 }
    ; { input = "let x = { \"a\": 2, 2: 1 }; x;"
      ; expected = Evaluator.Hash [ Integer 2, Integer 1; String "a", Integer 2 ]
      }
    ; { input = "let x = { \"a\": 2, 2: 1 }; x[2];"; expected = Evaluator.Integer 1 }
    ; { input = "true;"; expected = Evaluator.True }
    ; { input = "false;"; expected = Evaluator.False }
    ; { input = "\"str\";"; expected = Evaluator.String "str" }
    ; { input = "[1,2,3];"
      ; expected = Evaluator.Array [ Integer 3; Integer 2; Integer 1 ]
      }
    ; { input =
          "let some_fn = fn (x) { if (x) { return 1; } else { return 2; };}; \
           some_fn(true);"
      ; expected = Evaluator.Integer 1
      }
    ; { input =
          "let some_fn = fn (x) { if (x) { return 1; } else { return 2; };}; \
           some_fn(false);"
      ; expected = Evaluator.Integer 2
      }
    ; { input =
          "let some_fn = fn (f) { if (f()) { return 1; } else { return 2; };}; \
           some_fn(fn () { return true;});"
      ; expected = Evaluator.Integer 1
      }
      (* builtins *)
    ; { input = "puts(2);"; expected = Evaluator.Null }
    ]
  in
  suite
  |> List.iter (fun case ->
    case.input
    |> eval
    |> Result.map Evaluator.eval
    |> Result.get_ok
    |> fun (_, res) ->
    if Evaluator.Object.equal case.expected res
    then ignore ()
    else (
      let msg =
        Format.asprintf
          "#%s\nexpected: %a\ngot:      %a\n"
          case.input
          Evaluator.Object.pp
          case.expected
          Evaluator.Object.pp
          res
      in
      Format.eprintf "%s" msg;
      failwith "Test failed"))
;;
