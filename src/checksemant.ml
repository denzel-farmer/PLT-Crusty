let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
  let linear_result = Linear.check sprogram in
  print_endline (Astprint.string_of_program program);
  print_endline (Sastprint.string_of_sprogram sprogram);
  print_endline ("Passed linearity check: " ^ string_of_bool (Result.is_ok linear_result));