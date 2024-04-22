let () =
  let promt = ">>" in
  let rec loop env =
    Format.printf "%s " promt;
    Format.print_flush ();
    try
      let line = read_line () in
      line
      |> Monkey.Ast.eval
      |> Result.get_ok
      |> Monkey.Evaluator.eval ~env
      |> fun (env, obj) ->
      Format.asprintf "%a" Monkey.Evaluator.Object.pp obj
      |> fun it ->
      print_endline it;
      loop env
    with
    | End_of_file -> print_endline "Goodbye!"
    | Invalid_argument str ->
      print_endline ("Invalid argument " ^ str);
      loop env
    | _ -> loop env
  in
  loop Monkey.Evaluator.Environment.empty
;;
