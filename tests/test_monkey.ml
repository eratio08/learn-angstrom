open Monkey
open Ast

type test_case =
  { input : string
  ; expected : programm
  }

let () =
  let suite : test_case list =
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
    ; { input = "fn add(a, b){ return a + b; };"
      ; expected =
          Program
            [ ExpressionStatement
                (Function
                   ( "add"
                   , [ "a"; "b" ]
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
    ; { input = "let x = add(2, 4);"
      ; expected =
          Program
            [ LetStatement ("x", Call (Identifier "add", [ Integer 2; Integer 4 ])) ]
      }
    ; { input = "let x = fn add(c, d) { return true; };"
      ; expected =
          Program
            [ LetStatement
                ( "x"
                , Function ("add", [ "c"; "d" ], Block [ ReturnStatement (Bool true) ]) )
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
    ]
  in
  List.iter
    (fun case ->
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
    suite
;;

let () =
  eval
    "let x = 2;\n\
     puts(x + x * x);\n\
     puts(\"~~\");\n\
     let y = { \"a\": 2, 2: 1 };\n\
     puts(\"Hash:\");\n\
     puts(y);\n\
     puts(y[2]);\n\
     y;\n\
     puts(true == false);\n\
     puts(true != false);\n\
     if (x != 2) { return 1; } else { return \"bla\"; };\n"
  |> function
  | Error err -> Format.printf "Error: %s!" err
  | Ok program ->
    program |> Evaluator.eval |> fun obj -> Format.printf "$ %a" Evaluator.Object.pp obj
;;
