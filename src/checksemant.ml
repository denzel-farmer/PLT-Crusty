let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Crustyparse.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
  let linear_result = Linear.check sprogram in
  print_endline "\nAbstract Syntax Tree:";
  print_endline (Astprint.string_of_program program);
  print_endline "\nSemantic Checked Abstract Syntax Tree:";
  print_endline (Sastprint.string_of_sprogram sprogram);
  print_endline "\nLinearity Results:";
  print_endline (Linear.string_of_linear_result linear_result);
  if Linear.lin_result_failed linear_result then
    exit 1
  else
    print_endline ("\nCOMPILATION SUCCESS");