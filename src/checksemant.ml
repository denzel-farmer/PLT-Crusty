let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  let sprogram = Semant2.check program in
  let linear_result = Linear.check sprogram in
  print_endline (Astprint.string_of_program program);
  print_endline (Sastprint.string_of_sprogram sprogram);
  print_endline (string_of_bool (Result.is_error linear_result));